//! AST-to-IR lowering pass. Transforms type-checked AST into flat IR.

const std = @import("std");
const ast = @import("ast.zig");
const ir = @import("ir.zig");
const types = @import("types.zig");
const source = @import("source.zig");
const errors = @import("errors.zig");
const checker = @import("checker.zig");
const comptime_mod = @import("comptime.zig");
const token = @import("token.zig");
const arc = @import("arc_insertion.zig");
const target_mod = @import("../core/target.zig");

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
    /// Compilation target — used for @targetOs(), @targetArch(), @target() comptime builtins
    target: target_mod.Target = target_mod.Target.native(),
    /// Async state machine: local index of the state pointer param for the current async poll function.
    /// Stored as a local index (not IR node) so we can reload it in each block (Wasm br_table dispatch
    /// doesn't carry values across blocks).
    current_async_state_local: ?ir.LocalIdx = null,
    /// Async state machine: result type for the current async function
    current_async_result_type: ?TypeIndex = null,
    /// Maps variable names to their async poll function names.
    /// Populated when `const f = async_fn(args)` is lowered, used by `await f`.
    async_poll_names: std.StringHashMap([]const u8) = undefined,
    /// Global error variant table: maps error variant names to globally unique indices.
    /// Zig pattern: each error value has a unique integer across all error sets.
    /// Reference: Zig compiler uses a global error value table for @intFromError/@errorName.
    global_error_table: std.StringHashMap(i64) = undefined,
    next_error_idx: i64 = 0,
    /// Release mode: when false (default/debug), emit runtime safety checks.
    /// When true (--release), skip safety checks for performance.
    release_mode: bool = false,
    /// Zig Sema pattern: current switch subject enum type for .variant shorthand resolution
    current_switch_enum_type: TypeIndex = types.invalid_type,
    /// Comptime structured value bindings — for inline for over comptime arrays (Phase 5)
    comptime_value_vars: std.StringHashMap(comptime_mod.ComptimeValue),
    /// Global (top-level) comptime values — persists across functions, not cleared per-function.
    global_comptime_values: std.StringHashMap(comptime_mod.ComptimeValue),

    pub const Error = error{OutOfMemory};

    /// Zig SRET pattern: functions returning structs/tuples > 8 bytes use a hidden
    /// first parameter for the return value. Caller allocates space, passes pointer.
    /// Reference: references/zig/src/codegen/wasm/CodeGen.zig:1354 firstParamSRet()
    fn needsSret(self: *const Lowerer, type_idx: TypeIndex) bool {
        const info = self.type_reg.get(type_idx);
        return (info == .struct_type or info == .tuple) and self.type_reg.sizeOf(type_idx) > 8;
    }

    const LoopContext = struct {
        cond_block: ir.BlockIndex,
        exit_block: ir.BlockIndex,
        cleanup_depth: usize,
        label: ?[]const u8 = null,
    };

    pub fn init(allocator: Allocator, tree: *const Ast, type_reg: *TypeRegistry, err: *ErrorReporter, chk: *checker.Checker, target: target_mod.Target) Lowerer {
        return initWithBuilder(allocator, tree, type_reg, err, chk, ir.Builder.init(allocator, type_reg), target);
    }

    pub fn initWithBuilder(allocator: Allocator, tree: *const Ast, type_reg: *TypeRegistry, err: *ErrorReporter, chk: *checker.Checker, builder: ir.Builder, target: target_mod.Target) Lowerer {
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
            .float_const_values = std.StringHashMap(f64).init(allocator),
            .float_const_types = std.StringHashMap(TypeIndex).init(allocator),
            .test_names = .{},
            .test_display_names = .{},
            .bench_names = .{},
            .bench_display_names = .{},
            .lowered_generics = std.StringHashMap(void).init(allocator),
            .target = target,
            .global_error_table = std.StringHashMap(i64).init(allocator),
            .async_poll_names = std.StringHashMap([]const u8).init(allocator),
            .comptime_value_vars = std.StringHashMap(comptime_mod.ComptimeValue).init(allocator),
            .global_comptime_values = std.StringHashMap(comptime_mod.ComptimeValue).init(allocator),
        };
    }

    pub fn setTestMode(self: *Lowerer, enabled: bool) void { self.test_mode = enabled; }
    pub fn setFailFast(self: *Lowerer, enabled: bool) void { self.fail_fast = enabled; }
    pub fn addTestName(self: *Lowerer, name: []const u8) !void { try self.test_names.append(self.allocator, name); }
    pub fn addTestDisplayName(self: *Lowerer, name: []const u8) !void { try self.test_display_names.append(self.allocator, name); }
    pub fn getTestNames(self: *const Lowerer) []const []const u8 { return self.test_names.items; }
    pub fn getTestDisplayNames(self: *const Lowerer) []const []const u8 { return self.test_display_names.items; }

    pub fn setBenchMode(self: *Lowerer, enabled: bool) void { self.bench_mode = enabled; }
    pub fn addBenchName(self: *Lowerer, name: []const u8) !void { try self.bench_names.append(self.allocator, name); }
    pub fn addBenchDisplayName(self: *Lowerer, name: []const u8) !void { try self.bench_display_names.append(self.allocator, name); }
    pub fn getBenchNames(self: *const Lowerer) []const []const u8 { return self.bench_names.items; }
    pub fn getBenchDisplayNames(self: *const Lowerer) []const []const u8 { return self.bench_display_names.items; }

    pub fn deinit(self: *Lowerer) void {
        self.loop_stack.deinit(self.allocator);
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
        self.builder.deinit();
    }

    pub fn deinitWithoutBuilder(self: *Lowerer) void {
        self.loop_stack.deinit(self.allocator);
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
    }

    pub fn lower(self: *Lowerer) !ir.IR {
        try self.lowerToBuilder();
        return try self.builder.getIR();
    }

    pub fn lowerToBuilder(self: *Lowerer) !void {
        for (self.tree.getRootDecls()) |decl_idx| try self.lowerDecl(decl_idx);
        // Generic impl block methods are now queued on-demand via lowerMethodCall
        // (which calls ensureGenericFnQueued when it encounters a call to a method
        // in generic_inst_by_name). This avoids lowering unused methods.
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
            const err_void_type = self.type_reg.makeErrorUnion(TypeRegistry.VOID) catch TypeRegistry.VOID;

            // fail_count local — counts test failures, returned as exit code
            const fail_count = try fb.addLocalWithSize("__fail_count", TypeRegistry.I64, true, 8);
            const zero_init = try fb.emitConstInt(0, TypeRegistry.I64, span);
            _ = try fb.emitStoreLocal(fail_count, zero_init, span);

            // pass_count local — counts test passes, for summary output
            const pass_count = try fb.addLocalWithSize("__pass_count", TypeRegistry.I64, true, 8);
            _ = try fb.emitStoreLocal(pass_count, zero_init, span);

            // Note: total timing start is lazy-initialized in __test_print_name
            // (adding extra IR nodes to the test runner disrupts native br_table dispatch)

            for (self.test_names.items, self.test_display_names.items) |test_name, display_name| {
                // Print test name (also records per-test start time internally)
                const name_copy = try self.allocator.dupe(u8, display_name);
                const str_idx = try fb.addStringLiteral(name_copy);
                const str_slice = try fb.emitConstSlice(str_idx, span);
                const name_ptr = try fb.emitSlicePtr(str_slice, ptr_type, span);
                const name_len = try fb.emitSliceLen(str_slice, span);
                var print_args = [_]ir.NodeIndex{ name_ptr, name_len };
                _ = try fb.emitCall("__test_print_name", &print_args, false, TypeRegistry.VOID, span);

                // Call test function — returns pointer to error union (!void)
                var no_args = [_]ir.NodeIndex{};
                const eu_ptr = try fb.emitCall(test_name, &no_args, false, err_void_type, span);

                // Read error union tag at [eu_ptr + 0]
                const tag_val = try fb.emitPtrLoadValue(eu_ptr, TypeRegistry.I64, span);
                const zero = try fb.emitConstInt(0, TypeRegistry.I64, span);
                const is_error = try fb.emitBinary(.ne, tag_val, zero, TypeRegistry.BOOL, span);

                // Branch: error → fail, ok → pass
                const fail_block = try fb.newBlock("test.fail");
                const pass_block = try fb.newBlock("test.pass");
                const merge_block = try fb.newBlock("test.merge");
                _ = try fb.emitBranch(is_error, fail_block, pass_block, span);

                // Fail block: __test_fail() computes timing internally
                fb.setBlock(fail_block);
                var fail_args = [_]ir.NodeIndex{};
                _ = try fb.emitCall("__test_fail", &fail_args, false, TypeRegistry.VOID, span);
                const cur_fail = try fb.emitLoadLocal(fail_count, TypeRegistry.I64, span);
                const one_f = try fb.emitConstInt(1, TypeRegistry.I64, span);
                const new_fail = try fb.emitBinary(.add, cur_fail, one_f, TypeRegistry.I64, span);
                _ = try fb.emitStoreLocal(fail_count, new_fail, span);

                if (self.fail_fast) {
                    // --fail-fast: print summary and exit immediately after first failure
                    const ff_pass = try fb.emitLoadLocal(pass_count, TypeRegistry.I64, span);
                    const ff_fail = try fb.emitLoadLocal(fail_count, TypeRegistry.I64, span);
                    var ff_summary_args = [_]ir.NodeIndex{ ff_pass, ff_fail };
                    _ = try fb.emitCall("__test_summary", &ff_summary_args, false, TypeRegistry.VOID, span);
                    const ff_exit = try fb.emitLoadLocal(fail_count, TypeRegistry.I64, span);
                    _ = try fb.emitRet(ff_exit, span);
                } else {
                    _ = try fb.emitJump(merge_block, span);
                }

                // Pass block: __test_pass() computes timing internally
                fb.setBlock(pass_block);
                var pass_args = [_]ir.NodeIndex{};
                _ = try fb.emitCall("__test_pass", &pass_args, false, TypeRegistry.VOID, span);
                const cur_pass = try fb.emitLoadLocal(pass_count, TypeRegistry.I64, span);
                const one_p = try fb.emitConstInt(1, TypeRegistry.I64, span);
                const new_pass = try fb.emitBinary(.add, cur_pass, one_p, TypeRegistry.I64, span);
                _ = try fb.emitStoreLocal(pass_count, new_pass, span);
                _ = try fb.emitJump(merge_block, span);

                // Merge block: continue to next test
                fb.setBlock(merge_block);
            }

            // Print summary: __test_summary(pass_count, fail_count)
            // Total timing computed internally from stored start time
            const final_pass = try fb.emitLoadLocal(pass_count, TypeRegistry.I64, span);
            const final_fail = try fb.emitLoadLocal(fail_count, TypeRegistry.I64, span);
            var summary_args = [_]ir.NodeIndex{ final_pass, final_fail };
            _ = try fb.emitCall("__test_summary", &summary_args, false, TypeRegistry.VOID, span);

            // Return fail_count as exit code (0 = all pass)
            const final_count = try fb.emitLoadLocal(fail_count, TypeRegistry.I64, span);
            _ = try fb.emitRet(final_count, span);
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
            .impl_trait => |d| try self.lowerImplTraitBlock(d),
            .test_decl => |d| if (self.test_mode) try self.lowerTestDecl(d),
            .bench_decl => |d| if (self.bench_mode) try self.lowerBenchDecl(d),
            .trait_decl, .enum_decl, .union_decl, .type_alias, .import_decl, .error_set_decl, .bad_decl => {},
        }
    }

    fn lowerFnDecl(self: *Lowerer, fn_decl: ast.FnDecl) !void {
        if (fn_decl.is_extern) return;
        if (fn_decl.type_params.len > 0) return; // Skip generic fn defs — lowered on demand at call sites
        if ((self.test_mode or self.bench_mode) and std.mem.eql(u8, fn_decl.name, "main")) return;

        // Async functions get special lowering
        if (fn_decl.is_async) {
            if (self.target.isWasm()) {
                try self.lowerAsyncStateMachine(fn_decl);
            } else {
                try self.lowerAsyncFiber(fn_decl);
            }
            return;
        }

        const return_type = if (fn_decl.return_type != null_node) self.resolveTypeNode(fn_decl.return_type) else TypeRegistry.VOID;
        // SRET: callee receives hidden __sret pointer, returns void at Wasm level
        const uses_sret = self.needsSret(return_type);
        const wasm_return_type = if (uses_sret) TypeRegistry.VOID else return_type;
        self.builder.startFunc(fn_decl.name, TypeRegistry.VOID, wasm_return_type, fn_decl.span);
        if (self.builder.func()) |fb| {
            // Store the original return type for lowerReturn to use
            if (uses_sret) fb.sret_return_type = return_type;
            // Free-function-style destructors: fn TypeName_deinit(self: *TypeName) void
            fb.is_destructor = std.mem.endsWith(u8, fn_decl.name, "_deinit");
            self.current_func = fb;
            self.cleanup_stack.clear();
            self.comptime_value_vars.clearRetainingCapacity();
            // SRET: add hidden first parameter (Zig firstParamSRet pattern)
            if (uses_sret) {
                _ = try fb.addParam("__sret", TypeRegistry.I64, 8);
            }
            for (fn_decl.params) |param| {
                var param_type = self.resolveTypeNode(param.type_expr);
                param_type = self.chk.safeWrapType(param_type) catch param_type;
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

    /// Wasm async: emit constructor function (allocs state, returns future ptr)
    /// and poll function (state machine with if-chain dispatch).
    /// Ported from Rust's coroutine transform (rustc_mir_transform/coroutine.rs).
    fn lowerAsyncStateMachine(self: *Lowerer, fn_decl: ast.FnDecl) !void {
        const inner_return_type = if (fn_decl.return_type != null_node) self.resolveTypeNode(fn_decl.return_type) else TypeRegistry.VOID;

        // Count await points in the body to determine state count
        var await_count: u32 = 0;
        if (fn_decl.body != null_node) {
            await_count = self.countAwaitPoints(fn_decl.body);
        }

        // State struct layout (all i64-aligned):
        //   offset 0: __state (i64) — 0=UNRESUMED, 1=RETURNED, 2=POISONED, 3+=suspend points
        //   offset 8+: __result — result value (1 word for simple, 2 for error union)
        //   offset 8+result_words*8: params (each i64)
        const result_size = if (inner_return_type != TypeRegistry.VOID) self.type_reg.sizeOf(inner_return_type) else 0;
        const result_words: u32 = if (result_size > 0) (result_size + 7) / 8 else 1;
        const param_count: u32 = @intCast(fn_decl.params.len);
        const state_size: u32 = (1 + result_words + param_count + await_count) * 8;

        // --- Emit constructor function (original name) ---
        // Returns a heap-allocated state pointer (the Future)
        self.builder.startFunc(fn_decl.name, TypeRegistry.VOID, TypeRegistry.I64, fn_decl.span);
        if (self.builder.func()) |fb| {
            self.current_func = fb;
            self.cleanup_stack.clear();

            // Add params matching the declared signature
            for (fn_decl.params) |param| {
                var param_type = self.resolveTypeNode(param.type_expr);
                param_type = self.chk.safeWrapType(param_type) catch param_type;
                _ = try fb.addParam(param.name, param_type, self.type_reg.sizeOf(param_type));
            }

            // Allocate state struct on heap: cot_alloc(metadata_ptr=0, size)
            const metadata = try fb.emitConstInt(0, TypeRegistry.I64, fn_decl.span);
            const size_val = try fb.emitConstInt(@intCast(state_size), TypeRegistry.I64, fn_decl.span);
            var alloc_args = [_]ir.NodeIndex{ metadata, size_val };
            const state_ptr = try fb.emitCall("alloc", &alloc_args, false, TypeRegistry.I64, fn_decl.span);

            // Store state_ptr to a local for multi-use (Wasm stack machine semantics)
            const state_local = try fb.addLocalWithSize("__state", TypeRegistry.I64, false, 8);
            _ = try fb.emitStoreLocal(state_local, state_ptr, fn_decl.span);

            // Store __state = 0 (UNRESUMED) at offset 0
            {
                const sp = try fb.emitLoadLocal(state_local, TypeRegistry.I64, fn_decl.span);
                const zero = try fb.emitConstInt(0, TypeRegistry.I64, fn_decl.span);
                _ = try fb.emitPtrStoreValue(sp, zero, fn_decl.span);
            }

            // Store params into state struct (offset after state + result words)
            const param_base: u32 = (1 + result_words) * 8;
            for (fn_decl.params, 0..) |param, i| {
                const sp = try fb.emitLoadLocal(state_local, TypeRegistry.I64, fn_decl.span);
                const local_idx = fb.lookupLocal(param.name) orelse continue;
                const param_ref = try fb.emitLoadLocal(local_idx, TypeRegistry.I64, fn_decl.span);
                const offset: i64 = @intCast(param_base + i * 8);
                const param_addr = try fb.emitAddrOffset(sp, offset, TypeRegistry.I64, fn_decl.span);
                _ = try fb.emitPtrStoreValue(param_addr, param_ref, fn_decl.span);
            }

            // Return state pointer (this is the Future)
            const ret_val = try fb.emitLoadLocal(state_local, TypeRegistry.I64, fn_decl.span);
            _ = try fb.emitRet(ret_val, fn_decl.span);
            self.current_func = null;
        }
        try self.builder.endFunc();

        // --- Emit poll function (FnName_poll) ---
        // Takes state pointer, returns i64 (0=PENDING, 1=READY)
        const poll_name = try std.fmt.allocPrint(self.allocator, "{s}_poll", .{fn_decl.name});
        self.builder.startFunc(poll_name, TypeRegistry.VOID, TypeRegistry.I64, fn_decl.span);
        if (self.builder.func()) |fb| {
            self.current_func = fb;
            self.cleanup_stack.clear();

            // Single parameter: state pointer
            _ = try fb.addParam("__state_ptr", TypeRegistry.I64, 8);
            const state_ptr_local = fb.lookupLocal("__state_ptr") orelse unreachable;

            // Load current state — reload state_ptr from local (Wasm br_table doesn't carry values across blocks)
            const state_ptr_0 = try fb.emitLoadLocal(state_ptr_local, TypeRegistry.I64, fn_decl.span);
            const state_val = try fb.emitPtrLoadValue(state_ptr_0, TypeRegistry.I64, fn_decl.span);

            // State 0 (UNRESUMED): run the body from the start
            // State 1 (RETURNED): already done, return READY
            const already_done = try fb.newBlock("poll.done");
            const run_body = try fb.newBlock("poll.body");

            const one_0 = try fb.emitConstInt(1, TypeRegistry.I64, fn_decl.span);
            const is_done = try fb.emitBinary(.eq, state_val, one_0, TypeRegistry.BOOL, fn_decl.span);
            _ = try fb.emitBranch(is_done, already_done, run_body, fn_decl.span);

            // Already done block: return READY (1)
            fb.setBlock(already_done);
            const ready_done = try fb.emitConstInt(1, TypeRegistry.I64, fn_decl.span);
            _ = try fb.emitRet(ready_done, fn_decl.span);

            // Body block: reload state_ptr from local (fresh load for this block)
            fb.setBlock(run_body);

            // Restore params from state struct into locals
            // Reload state_ptr from local for each use (Wasm stack doesn't persist across blocks)
            const poll_param_base: u32 = (1 + result_words) * 8;
            for (fn_decl.params, 0..) |param, i| {
                var param_type = self.resolveTypeNode(param.type_expr);
                param_type = self.chk.safeWrapType(param_type) catch param_type;
                const offset: i64 = @intCast(poll_param_base + i * 8);
                const sp = try fb.emitLoadLocal(state_ptr_local, TypeRegistry.I64, fn_decl.span);
                const param_addr = try fb.emitAddrOffset(sp, offset, TypeRegistry.I64, fn_decl.span);
                const val = try fb.emitPtrLoadValue(param_addr, param_type, fn_decl.span);
                const local = try fb.addLocalWithSize(param.name, param_type, false, self.type_reg.sizeOf(param_type));
                _ = try fb.emitStoreLocal(local, val, fn_decl.span);
            }

            // Lower the body — any `return val` in an async fn becomes:
            // store result to state[8], set state to RETURNED, return READY
            // This is handled by lowerReturn checking current_async_state_local
            self.current_async_state_local = state_ptr_local;
            self.current_async_result_type = inner_return_type;
            // Override fb.return_type so that lowerErrorLiteral/lowerReturn's
            // error union checks see the actual inner return type (not poll's I64).
            const saved_return_type = fb.return_type;
            fb.return_type = inner_return_type;
            if (fn_decl.body != null_node) {
                _ = try self.lowerBlockNode(fn_decl.body);
            }
            fb.return_type = saved_return_type;

            // If body falls through (void return), mark as RETURNED
            if (fb.needsTerminator()) {
                const sp = try fb.emitLoadLocal(state_ptr_local, TypeRegistry.I64, fn_decl.span);
                const one_body = try fb.emitConstInt(1, TypeRegistry.I64, fn_decl.span);
                _ = try fb.emitPtrStoreValue(sp, one_body, fn_decl.span);
                _ = try fb.emitRet(one_body, fn_decl.span);
            }

            self.current_async_state_local = null;
            self.current_async_result_type = null;
            self.current_func = null;
        }
        try self.builder.endFunc();
    }

    /// Native async: eager evaluation with body + constructor pattern.
    /// Ported from Zig's approach — the async fn body is lowered as a normal function,
    /// preserving natural call stacks and debug info. The constructor calls the body
    /// directly, stores the result in a future struct, and returns the future pointer.
    /// No state machine transform needed — this is much simpler than the Wasm approach.
    /// Real context-switching fibers will be added in Phase D with the event loop.
    fn lowerAsyncFiber(self: *Lowerer, fn_decl: ast.FnDecl) !void {
        const inner_return_type = if (fn_decl.return_type != null_node) self.resolveTypeNode(fn_decl.return_type) else TypeRegistry.VOID;

        // --- Emit body function (fn_name_body) — normal function, no state machine ---
        // This preserves the natural call stack for debuggers (Zig lesson: stackful > stackless).
        const body_name = try std.fmt.allocPrint(self.allocator, "{s}_body", .{fn_decl.name});
        self.builder.startFunc(body_name, TypeRegistry.VOID, inner_return_type, fn_decl.span);
        if (self.builder.func()) |fb| {
            self.current_func = fb;
            self.cleanup_stack.clear();
            self.comptime_value_vars.clearRetainingCapacity();
            for (fn_decl.params) |param| {
                var param_type = self.resolveTypeNode(param.type_expr);
                param_type = self.chk.safeWrapType(param_type) catch param_type;
                _ = try fb.addParam(param.name, param_type, self.type_reg.sizeOf(param_type));
            }
            if (fn_decl.body != null_node) {
                _ = try self.lowerBlockNode(fn_decl.body);
                if (inner_return_type == TypeRegistry.VOID and fb.needsTerminator()) {
                    try self.emitCleanups(0);
                    _ = try fb.emitRet(null, fn_decl.span);
                }
            }
            self.current_func = null;
        }
        try self.builder.endFunc();

        // --- Emit constructor (fn_name) — calls body eagerly, stores result in future ---
        // Future struct layout: [state(i64) @ 0, result(i64) @ 8]
        self.builder.startFunc(fn_decl.name, TypeRegistry.VOID, TypeRegistry.I64, fn_decl.span);
        if (self.builder.func()) |fb| {
            self.current_func = fb;
            self.cleanup_stack.clear();

            // Add params matching the declared signature
            for (fn_decl.params) |param| {
                var param_type = self.resolveTypeNode(param.type_expr);
                param_type = self.chk.safeWrapType(param_type) catch param_type;
                _ = try fb.addParam(param.name, param_type, self.type_reg.sizeOf(param_type));
            }

            // Compute future size: state(8) + result (sizeOf inner_return_type, aligned to 8)
            const result_size = if (inner_return_type != TypeRegistry.VOID) self.type_reg.sizeOf(inner_return_type) else 0;
            const result_words = (result_size + 7) / 8;
            const future_size: i64 = @intCast(8 + result_size);
            // Error unions return a pointer (single i64) to stack-local data.
            // We must dereference and copy before the body's stack frame is gone.
            const inner_info = self.type_reg.get(inner_return_type);
            const is_eu_result = (inner_info == .error_union);

            // Allocate future struct: cot_alloc(metadata=0, size)
            const metadata = try fb.emitConstInt(0, TypeRegistry.I64, fn_decl.span);
            const size_val = try fb.emitConstInt(future_size, TypeRegistry.I64, fn_decl.span);
            var alloc_args = [_]ir.NodeIndex{ metadata, size_val };
            const future_ptr = try fb.emitCall("alloc", &alloc_args, false, TypeRegistry.I64, fn_decl.span);
            const future_local = try fb.addLocalWithSize("__future", TypeRegistry.I64, false, 8);
            _ = try fb.emitStoreLocal(future_local, future_ptr, fn_decl.span);

            // Call body function with same args — eager evaluation
            // Compound params (string/slice) must be decomposed into (ptr, len) at call site
            // to match the body function's Wasm-level parameter count.
            var body_args: [64]ir.NodeIndex = undefined;
            var body_arg_count: usize = 0;
            for (fn_decl.params) |param| {
                var param_type = self.resolveTypeNode(param.type_expr);
                param_type = self.chk.safeWrapType(param_type) catch param_type;
                const local_idx = fb.lookupLocal(param.name) orelse continue;
                const param_info = self.type_reg.get(param_type);
                if (param_type == TypeRegistry.STRING or param_info == .slice) {
                    // Compound: decompose into (ptr, len)
                    const val = try fb.emitLoadLocal(local_idx, param_type, fn_decl.span);
                    body_args[body_arg_count] = try fb.emitSlicePtr(val, TypeRegistry.I64, fn_decl.span);
                    body_arg_count += 1;
                    body_args[body_arg_count] = try fb.emitSliceLen(val, fn_decl.span);
                    body_arg_count += 1;
                } else {
                    body_args[body_arg_count] = try fb.emitLoadLocal(local_idx, TypeRegistry.I64, fn_decl.span);
                    body_arg_count += 1;
                }
            }
            const result = try fb.emitCall(body_name, body_args[0..body_arg_count], false, inner_return_type, fn_decl.span);

            // Store result at future[8..] — compound types store multiple words
            if (inner_return_type != TypeRegistry.VOID) {
                if (is_eu_result) {
                    // Error union: body returns a POINTER to 16-byte stack data.
                    // Dereference and copy both words (tag + payload) into future.
                    for (0..result_words) |w| {
                        const fp = try fb.emitLoadLocal(future_local, TypeRegistry.I64, fn_decl.span);
                        const dst_offset: i64 = @intCast(8 + w * 8);
                        const dst_addr = try fb.emitAddrOffset(fp, dst_offset, TypeRegistry.I64, fn_decl.span);
                        const src_offset: i64 = @intCast(w * 8);
                        const src_addr = try fb.emitAddrOffset(result, src_offset, TypeRegistry.I64, fn_decl.span);
                        const word = try fb.emitPtrLoadValue(src_addr, TypeRegistry.I64, fn_decl.span);
                        _ = try fb.emitPtrStoreValue(dst_addr, word, fn_decl.span);
                    }
                } else {
                    const fp = try fb.emitLoadLocal(future_local, TypeRegistry.I64, fn_decl.span);
                    const result_addr = try fb.emitAddrOffset(fp, 8, TypeRegistry.I64, fn_decl.span);
                    _ = try fb.emitPtrStoreValue(result_addr, result, fn_decl.span);
                }
            }

            // Set state = 1 (DONE) at future[0]
            {
                const fp = try fb.emitLoadLocal(future_local, TypeRegistry.I64, fn_decl.span);
                const one = try fb.emitConstInt(1, TypeRegistry.I64, fn_decl.span);
                _ = try fb.emitPtrStoreValue(fp, one, fn_decl.span);
            }

            // Return future pointer
            const ret_val = try fb.emitLoadLocal(future_local, TypeRegistry.I64, fn_decl.span);
            _ = try fb.emitRet(ret_val, fn_decl.span);
            self.current_func = null;
        }
        try self.builder.endFunc();
    }

    /// Count await expressions in an AST subtree.
    fn countAwaitPoints(self: *Lowerer, idx: NodeIndex) u32 {
        const node = self.tree.getNode(idx) orelse return 0;
        var count: u32 = 0;
        switch (node) {
            .expr => |expr| switch (expr) {
                .await_expr => count += 1,
                .binary => |b| {
                    count += self.countAwaitPoints(b.left);
                    count += self.countAwaitPoints(b.right);
                },
                .unary => |u| count += self.countAwaitPoints(u.operand),
                .call => |c| {
                    count += self.countAwaitPoints(c.callee);
                    for (c.args) |a| count += self.countAwaitPoints(a);
                },
                .if_expr => |ie| {
                    count += self.countAwaitPoints(ie.condition);
                    count += self.countAwaitPoints(ie.then_branch);
                    if (ie.else_branch != null_node) count += self.countAwaitPoints(ie.else_branch);
                },
                .block_expr => |be| {
                    for (be.stmts) |s| count += self.countAwaitPoints(s);
                    if (be.expr != null_node) count += self.countAwaitPoints(be.expr);
                },
                .paren => |p| count += self.countAwaitPoints(p.inner),
                .try_expr => |t| count += self.countAwaitPoints(t.operand),
                else => {},
            },
            .stmt => |stmt| switch (stmt) {
                .expr_stmt => |es| count += self.countAwaitPoints(es.expr),
                .return_stmt => |rs| if (rs.value != null_node) {
                    count += self.countAwaitPoints(rs.value);
                },
                .var_stmt => |vs| if (vs.value != null_node) {
                    count += self.countAwaitPoints(vs.value);
                },
                .assign_stmt => |as_stmt| count += self.countAwaitPoints(as_stmt.value),
                .if_stmt => |is_stmt| {
                    count += self.countAwaitPoints(is_stmt.condition);
                    count += self.countAwaitPoints(is_stmt.then_branch);
                    if (is_stmt.else_branch != null_node) count += self.countAwaitPoints(is_stmt.else_branch);
                },
                .while_stmt => |ws| {
                    count += self.countAwaitPoints(ws.condition);
                    count += self.countAwaitPoints(ws.body);
                },
                .block_stmt => |bs| {
                    for (bs.stmts) |s| count += self.countAwaitPoints(s);
                },
                .defer_stmt => |ds| count += self.countAwaitPoints(ds.expr),
                else => {},
            },
            .decl => {},
        }
        return count;
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
                if (sym.float_const_value) |fvalue| {
                    try self.float_const_values.put(var_decl.name, fvalue);
                    try self.float_const_types.put(var_decl.name, sym.type_idx);
                    return;
                }
            }
            // Float consts: parse literal value and store in float_const_values
            if ((type_idx == TypeRegistry.F32 or type_idx == TypeRegistry.F64) and var_decl.value != null_node) {
                if (self.evalFloatLiteral(var_decl.value)) |fval| {
                    try self.float_const_values.put(var_decl.name, fval);
                    try self.float_const_types.put(var_decl.name, type_idx);
                    return;
                }
            }
            // Comptime block producing structured values (arrays, strings).
            // Store in global_comptime_values — persists across functions (not cleared per-function).
            // Materialized on-demand when accessed in lowerIndex/lowerIdent.
            if (var_decl.value != null_node) {
                const val_node = self.tree.getNode(var_decl.value);
                const val_expr = if (val_node) |n| n.asExpr() else null;
                if (val_expr) |ve| {
                    if (ve == .comptime_block) {
                        if (self.chk.evalComptimeValue(var_decl.value)) |cv| {
                            try self.global_comptime_values.put(var_decl.name, cv);
                            return;
                        }
                    }
                }
            }
        }
        const type_size: u32 = @intCast(self.type_reg.sizeOf(type_idx));
        try self.builder.addGlobal(ir.Global.initWithSize(var_decl.name, type_idx, var_decl.is_const, var_decl.span, type_size));
    }

    fn evalFloatLiteral(self: *Lowerer, idx: NodeIndex) ?f64 {
        const node = self.tree.getNode(idx) orelse return null;
        const expr = node.asExpr() orelse return null;
        return switch (expr) {
            .literal => |lit| switch (lit.kind) {
                .float => std.fmt.parseFloat(f64, lit.value) catch null,
                else => null,
            },
            .paren => |p| self.evalFloatLiteral(p.inner),
            .unary => |un| if (un.op == .sub) blk: {
                const v = self.evalFloatLiteral(un.operand) orelse break :blk null;
                break :blk -v;
            } else null,
            .binary => |bin| blk: {
                const l = self.evalFloatLiteral(bin.left) orelse break :blk null;
                const r = self.evalFloatLiteral(bin.right) orelse break :blk null;
                break :blk switch (bin.op) {
                    .add => l + r,
                    .sub => l - r,
                    .mul => l * r,
                    .quo => l / r,
                    else => null,
                };
            },
            else => null,
        };
    }

    fn lowerStructDecl(self: *Lowerer, struct_decl: ast.StructDecl) !void {
        if (struct_decl.type_params.len > 0) return; // Skip generic struct defs — instantiated on demand
        const struct_type_idx = self.type_reg.lookupByName(struct_decl.name) orelse TypeRegistry.VOID;
        try self.builder.addStruct(.{ .name = struct_decl.name, .type_idx = struct_type_idx, .span = struct_decl.span });
        // Nested declarations: most types (enum, error set, type alias) don't generate
        // code — they're registered during checking. Nested structs need addStruct.
        for (struct_decl.nested_decls) |nested_idx| {
            const nested_node = self.tree.getNode(nested_idx) orelse continue;
            const nested_decl = nested_node.asDecl() orelse continue;
            switch (nested_decl) {
                .struct_decl => |ns| {
                    const qualified = try std.fmt.allocPrint(self.allocator, "{s}_{s}", .{ struct_decl.name, ns.name });
                    const ns_type = self.type_reg.lookupByName(qualified) orelse TypeRegistry.VOID;
                    try self.builder.addStruct(.{ .name = qualified, .type_idx = ns_type, .span = ns.span });
                },
                .fn_decl => |fd| try self.lowerFnDecl(fd),
                else => {},
            }
        }
    }

    fn lowerImplBlock(self: *Lowerer, impl_block: ast.ImplBlock) !void {
        // Generic impl blocks: methods lowered via lowerQueuedGenericFunctions
        if (impl_block.type_params.len > 0) return;
        for (impl_block.methods) |method_idx| {
            const node = self.tree.getNode(method_idx) orelse continue;
            const decl = node.asDecl() orelse continue;
            if (decl == .fn_decl) {
                const synth_name = try std.fmt.allocPrint(self.allocator, "{s}_{s}", .{ impl_block.type_name, decl.fn_decl.name });
                const is_dtor = std.mem.eql(u8, decl.fn_decl.name, "deinit");
                try self.lowerMethodWithName(decl.fn_decl, synth_name, is_dtor);
            }
        }
    }

    fn lowerImplTraitBlock(self: *Lowerer, impl_trait: ast.ImplTraitBlock) !void {
        for (impl_trait.methods) |method_idx| {
            const node = self.tree.getNode(method_idx) orelse continue;
            const decl = node.asDecl() orelse continue;
            if (decl == .fn_decl) {
                const synth_name = try std.fmt.allocPrint(self.allocator, "{s}_{s}", .{ impl_trait.target_type, decl.fn_decl.name });
                const is_dtor = std.mem.eql(u8, decl.fn_decl.name, "deinit");
                try self.lowerMethodWithName(decl.fn_decl, synth_name, is_dtor);
            }
        }
    }

    fn lowerMethodWithName(self: *Lowerer, fn_decl: ast.FnDecl, synth_name: []const u8, is_destructor: bool) !void {
        const return_type = if (fn_decl.return_type != null_node) self.resolveTypeNode(fn_decl.return_type) else TypeRegistry.VOID;
        const uses_sret = self.needsSret(return_type);
        const wasm_return_type = if (uses_sret) TypeRegistry.VOID else return_type;
        self.builder.startFunc(synth_name, TypeRegistry.VOID, wasm_return_type, fn_decl.span);
        if (self.builder.func()) |fb| {
            fb.is_destructor = is_destructor;
            if (uses_sret) fb.sret_return_type = return_type;
            self.current_func = fb;
            self.cleanup_stack.clear();
            if (uses_sret) {
                _ = try fb.addParam("__sret", TypeRegistry.I64, 8);
            }
            for (fn_decl.params) |param| {
                var param_type = self.resolveTypeNode(param.type_expr);
                param_type = self.chk.safeWrapType(param_type) catch param_type;
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
        // Test functions return !void (error union) — Zig pattern: test blocks return errors
        // on assertion failure, runner catches and continues to next test
        const err_void_type = try self.type_reg.makeErrorUnion(TypeRegistry.VOID);
        self.builder.startFunc(test_name, TypeRegistry.VOID, err_void_type, test_decl.span);
        if (self.builder.func()) |fb| {
            self.current_func = fb;
            self.current_test_name = test_decl.name;
            self.cleanup_stack.clear();
            self.comptime_value_vars.clearRetainingCapacity();
            if (test_decl.body != null_node) {
                _ = try self.lowerBlockNode(test_decl.body);
                if (fb.needsTerminator()) {
                    try self.emitCleanups(0);
                    // Return success: error union with tag=0 (no error)
                    const eu_size = self.type_reg.sizeOf(err_void_type);
                    const tmp_local = try fb.addLocalWithSize("__test_ok", err_void_type, false, eu_size);
                    const tag_zero = try fb.emitConstInt(0, TypeRegistry.I64, test_decl.span);
                    _ = try fb.emitStoreLocalField(tmp_local, 0, 0, tag_zero, test_decl.span);
                    const zero_payload = try fb.emitConstInt(0, TypeRegistry.I64, test_decl.span);
                    _ = try fb.emitStoreLocalField(tmp_local, 1, 8, zero_payload, test_decl.span);
                    const ret_ptr = try fb.emitAddrLocal(tmp_local, TypeRegistry.I64, test_decl.span);
                    _ = try fb.emitRet(ret_ptr, test_decl.span);
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

    fn lowerBenchDecl(self: *Lowerer, bench_decl: ast.BenchDecl) !void {
        const bench_name = try self.sanitizeBenchName(bench_decl.name);
        try self.bench_names.append(self.allocator, bench_name);
        try self.bench_display_names.append(self.allocator, bench_decl.name);
        // Bench functions return i64 (matching main convention) to avoid void-return
        // complications in the br_table dispatch pattern
        self.builder.startFunc(bench_name, TypeRegistry.VOID, TypeRegistry.I64, bench_decl.span);
        if (self.builder.func()) |fb| {
            self.current_func = fb;
            self.cleanup_stack.clear();
            if (bench_decl.body != null_node) {
                _ = try self.lowerBlockNode(bench_decl.body);
                if (fb.needsTerminator()) {
                    try self.emitCleanups(0);
                    const zero = try fb.emitConstInt(0, TypeRegistry.I64, bench_decl.span);
                    _ = try fb.emitRet(zero, bench_decl.span);
                }
            }
            self.current_func = null;
        }
        try self.builder.endFunc();
    }

    fn sanitizeBenchName(self: *Lowerer, name: []const u8) ![]const u8 {
        var result = try self.allocator.alloc(u8, 6 + name.len);
        @memcpy(result[0..6], "bench_");
        for (name, 0..) |c, i| {
            result[6 + i] = if ((c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z') or (c >= '0' and c <= '9')) c else '_';
        }
        return result;
    }

    pub fn generateBenchRunner(self: *Lowerer) !void {
        if (self.bench_names.items.len == 0) return;
        const span = Span.init(Pos.zero, Pos.zero);
        self.builder.startFunc("main", TypeRegistry.VOID, TypeRegistry.I64, span);
        if (self.builder.func()) |fb| {
            self.current_func = fb;
            const ptr_type = self.type_reg.makePointer(TypeRegistry.U8) catch TypeRegistry.VOID;

            // Temp local for discarding bench function return values
            const discard_local = try fb.addLocalWithSize("__bench_discard", TypeRegistry.I64, true, 8);
            // Loop counter
            const loop_counter = try fb.addLocalWithSize("__bench_i", TypeRegistry.I64, true, 8);

            for (self.bench_names.items, 0..) |bench_name, bench_idx| {
                const display_name = self.bench_display_names.items[bench_idx];

                // __bench_print_name(name_ptr, name_len)
                {
                    const name_copy = try self.allocator.dupe(u8, display_name);
                    const str_idx = try fb.addStringLiteral(name_copy);
                    const str_slice = try fb.emitConstSlice(str_idx, span);
                    const name_ptr = try fb.emitSlicePtr(str_slice, ptr_type, span);
                    const name_len = try fb.emitSliceLen(str_slice, span);
                    var pn_args = [_]ir.NodeIndex{ name_ptr, name_len };
                    _ = try fb.emitCall("__bench_print_name", &pn_args, false, TypeRegistry.VOID, span);
                }

                // Warmup: call bench_fn() × 3
                for (0..3) |_| {
                    var no_args = [_]ir.NodeIndex{};
                    const r = try fb.emitCall(bench_name, &no_args, false, TypeRegistry.I64, span);
                    _ = try fb.emitStoreLocal(discard_local, r, span);
                }

                // Calibrate: single timed call
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

                // Measure: loop N times
                {
                    var ms_args = [_]ir.NodeIndex{};
                    _ = try fb.emitCall("__bench_measure_start", &ms_args, false, TypeRegistry.VOID, span);
                }

                // i = 0
                {
                    const zero = try fb.emitConstInt(0, TypeRegistry.I64, span);
                    _ = try fb.emitStoreLocal(loop_counter, zero, span);
                }

                // while (i < __bench_get_n()) { bench_fn(); i = i + 1 }
                const loop_cond_block = try fb.newBlock("bench.loop.cond");
                const loop_body_block = try fb.newBlock("bench.loop.body");
                const loop_exit_block = try fb.newBlock("bench.loop.exit");
                _ = try fb.emitJump(loop_cond_block, span);

                // Condition block: i < N ?
                fb.setBlock(loop_cond_block);
                {
                    var gn_args = [_]ir.NodeIndex{};
                    const n_val = try fb.emitCall("__bench_get_n", &gn_args, false, TypeRegistry.I64, span);
                    const i_val = try fb.emitLoadLocal(loop_counter, TypeRegistry.I64, span);
                    const cond = try fb.emitBinary(.lt, i_val, n_val, TypeRegistry.BOOL, span);
                    _ = try fb.emitBranch(cond, loop_body_block, loop_exit_block, span);
                }

                // Body block: bench_fn(); i += 1; jump to cond
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

                // Exit block: continue
                fb.setBlock(loop_exit_block);

                // __bench_measure_end()
                {
                    var me_args = [_]ir.NodeIndex{};
                    _ = try fb.emitCall("__bench_measure_end", &me_args, false, TypeRegistry.VOID, span);
                }
            }

            // __bench_summary(bench_count)
            {
                const count = try fb.emitConstInt(@intCast(self.bench_names.items.len), TypeRegistry.I64, span);
                var sum_args = [_]ir.NodeIndex{count};
                _ = try fb.emitCall("__bench_summary", &sum_args, false, TypeRegistry.VOID, span);
            }

            // Return 0
            const ret_zero = try fb.emitConstInt(0, TypeRegistry.I64, span);
            _ = try fb.emitRet(ret_zero, span);
            self.current_func = null;
        }
        try self.builder.endFunc();
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
            .if_stmt => |i| { return try self.lowerIf(i); },
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
            .destructure_stmt => |ds| { try self.lowerDestructureStmt(ds); return false; },
            .break_stmt => |bs| { try self.lowerBreak(bs.label); return true; },
            .continue_stmt => |cs| { try self.lowerContinue(cs.label); return true; },
            .expr_stmt => |es| { _ = try self.lowerExprNode(es.expr); return false; },
            .defer_stmt => |ds| {
                // Push defer/errdefer as cleanup entry on unified stack (Swift's DeferCleanup pattern)
                // Zig reference: AstGen.zig:3132-3200 — .defer_normal vs .defer_error
                const cleanup = arc.Cleanup{
                    .kind = if (ds.is_errdefer) .errdefer_expr else .defer_expr,
                    .value = @intCast(ds.expr),
                    .type_idx = TypeRegistry.VOID,
                    .state = .active,
                    .local_idx = null,
                    .func_name = null,
                };
                _ = try self.cleanup_stack.push(cleanup);
                return false;
            },
            .bad_stmt => return false,
        }
    }

    fn lowerReturn(self: *Lowerer, ret: ast.ReturnStmt) !void {
        const fb = self.current_func orelse return;

        // Async poll function: store result to state struct, set state to RETURNED, return READY
        if (self.current_async_state_local) |state_local| {
            if (ret.value != null_node) {
                const async_result_type = self.current_async_result_type orelse TypeRegistry.I64;
                const async_result_info = self.type_reg.get(async_result_type);
                const is_eu = (async_result_info == .error_union);

                if (is_eu) {
                    // Error union result — check if return value is error literal or success value
                    const ret_node = self.tree.getNode(ret.value);
                    const is_error_literal = if (ret_node) |n| if (n.asExpr()) |e| e == .error_literal else false else false;

                    if (is_error_literal) {
                        // `return error.X` — lowerExprNode returns pointer to 16-byte EU
                        const eu_ptr = try self.lowerExprNode(ret.value);
                        const num_words: usize = 2; // error union = 16 bytes = 2 words
                        for (0..num_words) |w| {
                            const sp = try fb.emitLoadLocal(state_local, TypeRegistry.I64, ret.span);
                            const dst_offset: i64 = @intCast(8 + w * 8);
                            const dst_addr = try fb.emitAddrOffset(sp, dst_offset, TypeRegistry.I64, ret.span);
                            const src_offset: i64 = @intCast(w * 8);
                            const src_addr = try fb.emitAddrOffset(eu_ptr, src_offset, TypeRegistry.I64, ret.span);
                            const word = try fb.emitPtrLoadValue(src_addr, TypeRegistry.I64, ret.span);
                            _ = try fb.emitPtrStoreValue(dst_addr, word, ret.span);
                        }
                    } else {
                        // Success value — wrap as tag=0, payload=value directly into state[8..]
                        const lowered = try self.lowerExprNode(ret.value);
                        {
                            const sp = try fb.emitLoadLocal(state_local, TypeRegistry.I64, ret.span);
                            const tag_addr = try fb.emitAddrOffset(sp, 8, TypeRegistry.I64, ret.span);
                            const tag_zero = try fb.emitConstInt(0, TypeRegistry.I64, ret.span);
                            _ = try fb.emitPtrStoreValue(tag_addr, tag_zero, ret.span);
                        }
                        {
                            const sp = try fb.emitLoadLocal(state_local, TypeRegistry.I64, ret.span);
                            const payload_addr = try fb.emitAddrOffset(sp, 16, TypeRegistry.I64, ret.span);
                            _ = try fb.emitPtrStoreValue(payload_addr, lowered, ret.span);
                        }
                    }
                } else {
                    const lowered = try self.lowerExprNode(ret.value);
                    // Reload state_ptr from local (Wasm stack doesn't persist across blocks)
                    const sp = try fb.emitLoadLocal(state_local, TypeRegistry.I64, ret.span);
                    // Store result at state[8]
                    const result_addr = try fb.emitAddrOffset(sp, 8, TypeRegistry.I64, ret.span);
                    _ = try fb.emitPtrStoreValue(result_addr, lowered, ret.span);
                }
            }
            // Reload state_ptr again for state update
            const sp2 = try fb.emitLoadLocal(state_local, TypeRegistry.I64, ret.span);
            // Set __state = 1 (RETURNED)
            const one = try fb.emitConstInt(1, TypeRegistry.I64, ret.span);
            _ = try fb.emitPtrStoreValue(sp2, one, ret.span);
            // Return READY (1)
            _ = try fb.emitRet(one, ret.span);
            return;
        }

        var value_node: ?ir.NodeIndex = null;
        // Track whether this return is an error path (for errdefer)
        // Zig reference: errdefer fires on `return error.X` and `try` propagation
        var is_error_return = false;
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
                is_error_return = true;
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
                    // Check if payload is compound (string = ptr+len)
                    const eu_elem = ret_type_info.error_union.elem;
                    if (eu_elem == TypeRegistry.STRING) {
                        // Compound: decompose string into ptr (field 1, offset 8) + len (field 2, offset 16)
                        const ptr_type = self.type_reg.makePointer(TypeRegistry.U8) catch TypeRegistry.VOID;
                        const ptr_val = try fb.emitSlicePtr(lowered, ptr_type, ret.span);
                        const len_val = try fb.emitSliceLen(lowered, ret.span);
                        _ = try fb.emitStoreLocalField(tmp_local, 1, 8, ptr_val, ret.span);
                        _ = try fb.emitStoreLocalField(tmp_local, 2, 16, len_val, ret.span);
                    } else {
                        _ = try fb.emitStoreLocalField(tmp_local, 1, 8, lowered, ret.span);
                    }
                    value_node = try fb.emitAddrLocal(tmp_local, TypeRegistry.I64, ret.span);
                }
            } else if (fb.sret_return_type) |sret_type| {
                // SRET: write return value to caller-allocated buffer via __sret pointer.
                // Zig pattern: firstParamSRet() — callee writes, returns void.
                const sret_size = self.type_reg.sizeOf(sret_type);
                const num_words = sret_size / 8;
                const sret_idx = fb.lookupLocal("__sret").?;
                const sret_ptr = try fb.emitLoadLocal(sret_idx, TypeRegistry.I64, ret.span);

                const ret_expr = if (ret_node) |n| n.asExpr() else null;
                if (ret_expr != null and ret_expr.? == .ident) {
                    // Direct local reference → copy fields from local to SRET
                    if (fb.lookupLocal(ret_expr.?.ident.name)) |src_local| {
                        for (0..num_words) |i| {
                            const offset: i64 = @intCast(i * 8);
                            const field = try fb.emitFieldLocal(src_local, @intCast(i), offset, TypeRegistry.I64, ret.span);
                            _ = try fb.emitStoreField(sret_ptr, @intCast(i), offset, field, ret.span);
                        }
                    }
                } else {
                    // Expression result → lower, store to temp, copy temp fields to SRET
                    const lowered = try self.lowerExprNode(ret.value);
                    if (lowered != ir.null_node) {
                        const tmp_local = try fb.addLocalWithSize("__ret_sret_src", sret_type, false, sret_size);
                        _ = try fb.emitStoreLocal(tmp_local, lowered, ret.span);
                        for (0..num_words) |i| {
                            const offset: i64 = @intCast(i * 8);
                            const field = try fb.emitFieldLocal(tmp_local, @intCast(i), offset, TypeRegistry.I64, ret.span);
                            _ = try fb.emitStoreField(sret_ptr, @intCast(i), offset, field, ret.span);
                        }
                    }
                }

                // Forward ownership before cleanup
                if (ret_node) |node| {
                    if (node.asExpr()) |expr| {
                        if (expr == .ident) {
                            if (fb.lookupLocal(expr.ident.name)) |local_idx| {
                                _ = self.cleanup_stack.disableForLocal(local_idx);
                            }
                        }
                    }
                }

                // Handle defers
                if (self.hasDeferCleanups()) {
                    try self.emitCleanups(0);
                }
                _ = try fb.emitRet(null, ret.span); // return void
                return;
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
                if (is_error_return) {
                    try self.emitCleanupsErrorPath(0);
                } else {
                    try self.emitCleanups(0);
                }
                value_node = try fb.emitLoadLocal(tmp_local, ret_type, ret.span);
                _ = try fb.emitRet(value_node, ret.span);
                return;
            }
        }
        if (is_error_return) {
            try self.emitCleanupsErrorPath(0);
        } else {
            try self.emitCleanups(0);
        }
        _ = try fb.emitRet(value_node, ret.span);
    }

    /// Check if there are any active defer or scope_destroy cleanups on the stack.
    fn hasDeferCleanups(self: *const Lowerer) bool {
        for (self.cleanup_stack.items.items) |cleanup| {
            if (cleanup.isActive() and (cleanup.kind == .defer_expr or cleanup.kind == .errdefer_expr or cleanup.kind == .scope_destroy)) return true;
        }
        return false;
    }

    /// Register a scope_destroy cleanup if the struct type has a free() method.
    /// Called at variable declaration for .{} zero-init and struct literal init.
    /// Rust parallel: schedule_drop(place, DropKind::Value) in scope.rs — registers
    /// a drop obligation when a value is bound to a variable.
    /// Swift parallel: pushCleanup(DestroyValueCleanup) on owned value init.
    /// Zig parallel: implicit defer insertion (AstGen.zig Scope.Defer).
    fn maybeRegisterScopeDestroy(self: *Lowerer, local_idx: ir.LocalIdx, type_idx: TypeIndex) !void {
        const type_info = self.type_reg.get(type_idx);
        if (type_info != .struct_type) return;
        const struct_name = type_info.struct_type.name;

        // Check if this struct has a deinit() method (automatic destructor).
        // Note: free() is manual cleanup — only deinit() triggers scope_destroy.
        if (self.chk.lookupMethod(struct_name, "deinit")) |method_info| {
            // Queue the generic function for lowering (same as lowerMethodCall does)
            if (self.chk.generics.generic_inst_by_name.get(method_info.func_name)) |inst_info| {
                try self.ensureGenericFnQueued(inst_info);
            }
            const cleanup = arc.Cleanup.initScopeDestroy(local_idx, type_idx, method_info.func_name);
            _ = try self.cleanup_stack.push(cleanup);
        }
    }

    /// Check if an expression's root local has an active ARC cleanup.
    /// Walks chains: a.b.c → checks if a has cleanup. Also follows deref.
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
        if (base_expr == .deref) return self.baseHasCleanup(base_expr.deref.operand);
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
    /// Zig errdefer reference: AstGen.zig:3132-3200 — errdefer_expr only runs on error paths.
    fn emitCleanups(self: *Lowerer, target_depth: usize) Error!void {
        return self.emitCleanupsImpl(target_depth, false, true);
    }

    /// Emit cleanups on an error path — runs both defer_expr AND errdefer_expr.
    /// Called when returning an error or propagating via try.
    fn emitCleanupsErrorPath(self: *Lowerer, target_depth: usize) Error!void {
        return self.emitCleanupsImpl(target_depth, true, true);
    }

    fn emitCleanupsImpl(self: *Lowerer, target_depth: usize, is_error_path: bool, pop: bool) Error!void {
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
                        _ = try fb.emitCall("release", &args, false, TypeRegistry.VOID, Span.zero);
                    },
                    .defer_expr => {
                        try self.lowerDeferredNode(@intCast(cleanup.value));
                    },
                    .errdefer_expr => {
                        // Zig semantics: errdefer only fires on error exit paths
                        if (is_error_path) {
                            try self.lowerDeferredNode(@intCast(cleanup.value));
                        }
                    },
                    .scope_destroy => {
                        // Emit free() call on struct at scope exit.
                        // Rust parallel: drop_in_place<T> shim (compiler/rustc_mir_transform/src/elaborate_drops.rs).
                        // Swift parallel: destroy_value SIL instruction emitted by endScope (Scope.cpp).
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

        // Pop the cleanups we just emitted
        if (pop) {
            while (self.cleanup_stack.items.items.len > target_depth) {
                _ = self.cleanup_stack.items.pop();
            }
        }
    }

    /// Emit cleanups from current depth to target WITHOUT popping.
    /// Used for break/continue — other paths still need these cleanups.
    /// Port of Swift's emitBranchAndCleanups pattern.
    fn emitCleanupsNoPop(self: *Lowerer, target_depth: usize) Error!void {
        return self.emitCleanupsImpl(target_depth, false, false);
        // Do NOT pop — other code paths still need these cleanups
    }

    fn lowerLocalVarDecl(self: *Lowerer, var_stmt: ast.VarStmt) !void {
        const fb = self.current_func orelse return;
        var type_idx = TypeRegistry.VOID;
        if (var_stmt.type_expr != null_node) {
            type_idx = self.resolveTypeNode(var_stmt.type_expr);
            // @safe coercion: checker upgrades `var p: Foo = new Foo{...}` to *Foo,
            // but resolveTypeNode returns raw Foo from AST. Match the checker's coercion.
            if (self.chk.safe_mode and var_stmt.value != null_node) {
                const val_type = self.inferExprType(var_stmt.value);
                if (val_type != TypeRegistry.VOID) {
                    const val_info = self.type_reg.get(val_type);
                    if (val_info == .pointer and val_info.pointer.elem == type_idx) {
                        type_idx = val_type;
                    }
                }
            }
        } else if (var_stmt.value != null_node) {
            type_idx = self.inferExprType(var_stmt.value);
        }
        const size = self.type_reg.sizeOf(type_idx);
        const local_idx = try fb.addLocalWithSize(var_stmt.name, type_idx, !var_stmt.is_const, size);

        if (var_stmt.value != null_node) {
            const value_node_ast = self.tree.getNode(var_stmt.value);
            const value_expr = if (value_node_ast) |n| n.asExpr() else null;

            // Track async future → poll name mapping for `await <variable>`.
            // When `const f = async_fn(args)`, store "f" → "async_fn_poll".
            if (value_expr) |ve| {
                if (ve == .call) {
                    const poll_name = self.resolveAsyncPollName(var_stmt.value);
                    if (poll_name) |pn| {
                        self.async_poll_names.put(var_stmt.name, pn) catch {};
                    }
                }
            }

            // Check for undefined literal or .{} zero init - zero memory
            const is_undefined = if (value_expr) |e| (e == .literal and e.literal.kind == .undefined_lit) else false;
            const is_zero_init = if (value_expr) |e| (e == .zero_init) else false;
            if (is_undefined or is_zero_init) {
                const ptr_type = self.type_reg.makePointer(TypeRegistry.U8) catch TypeRegistry.VOID;
                const local_addr = try fb.emitAddrLocal(local_idx, ptr_type, var_stmt.span);
                const size_node = try fb.emitConstInt(@intCast(size), TypeRegistry.I64, var_stmt.span);
                var args = [_]ir.NodeIndex{ local_addr, size_node };
                _ = try fb.emitCall("memset_zero", &args, false, TypeRegistry.VOID, var_stmt.span);

                // Register scope_destroy for .{} (zero_init), not for undefined
                if (is_zero_init) {
                    try self.maybeRegisterScopeDestroy(local_idx, type_idx);
                }
                return;
            }

            // Comptime blocks: evaluate at compile time and emit result directly.
            // This must come before type-specific paths (array, slice, etc.) because
            // comptime blocks produce values, not AST literals that those paths expect.
            const is_comptime_block = if (value_expr) |e| e == .comptime_block else false;
            if (is_comptime_block) {
                if (self.chk.evalComptimeValue(var_stmt.value)) |cv| {
                    // Store comptime value so lowerIndex/lowerIdent can resolve at index sites
                    try self.comptime_value_vars.put(var_stmt.name, cv);
                    if (cv == .array) {
                        // Materialize comptime array into local variable's stack memory.
                        // Element size must match sizeOf(elem_type): 8 for int/bool, 24 for string.
                        // STRING is internally a slice type (ptr+len+cap = 24 bytes).
                        for (cv.array.elements.items, 0..) |elem, i| {
                            const idx_node = try fb.emitConstInt(@intCast(i), TypeRegistry.I64, var_stmt.span);
                            switch (elem) {
                                .int => |v| {
                                    const val_node = try fb.emitConstInt(v, TypeRegistry.I64, var_stmt.span);
                                    _ = try fb.emitStoreIndexLocal(local_idx, idx_node, val_node, 8, var_stmt.span);
                                },
                                .boolean => |b| {
                                    const val_node = try fb.emitConstInt(if (b) 1 else 0, TypeRegistry.I64, var_stmt.span);
                                    _ = try fb.emitStoreIndexLocal(local_idx, idx_node, val_node, 8, var_stmt.span);
                                },
                                .string => |s| {
                                    // String elements: emit as string literal, store with elem_size=24.
                                    // STRING = slice(U8), sizeOf=24 (ptr+len+cap). Store decomposes to ptr@0, len@8.
                                    // Cap@16 left as zero from memset_zero initialization.
                                    const copied = try self.allocator.dupe(u8, s);
                                    const str_idx = try fb.addStringLiteral(copied);
                                    const str_node = try fb.emitConstSlice(str_idx, var_stmt.span);
                                    _ = try fb.emitStoreIndexLocal(local_idx, idx_node, str_node, self.type_reg.sizeOf(TypeRegistry.STRING), var_stmt.span);
                                },
                                else => {},
                            }
                        }
                    } else {
                        const result_node = try self.emitComptimeValue(cv, var_stmt.span);
                        if (result_node != ir.null_node) {
                            // String values need compound decomposition (ptr+len), same as lowerStringInit.
                            // emitStoreLocal does a single store which doesn't decompose STRING → (ptr@0, len@8).
                            if (cv == .string) {
                                const ptr_type = self.type_reg.makePointer(TypeRegistry.U8) catch TypeRegistry.VOID;
                                const ptr_val = try fb.emitSlicePtr(result_node, ptr_type, var_stmt.span);
                                const len_val = try fb.emitSliceLen(result_node, var_stmt.span);
                                _ = try fb.emitStoreLocalField(local_idx, 0, 0, ptr_val, var_stmt.span);
                                _ = try fb.emitStoreLocalField(local_idx, 1, 8, len_val, var_stmt.span);
                            } else {
                                _ = try fb.emitStoreLocal(local_idx, result_node, var_stmt.span);
                            }
                        }
                    }
                    return;
                }
            }

            const is_array = self.type_reg.isArray(type_idx);
            const is_slice = self.type_reg.isSlice(type_idx);
            const is_struct_literal = if (value_expr) |e| e == .struct_init else false;
            const is_tuple_literal = if (value_expr) |e| e == .tuple_literal else false;

            if (type_idx == TypeRegistry.STRING) {
                try self.lowerStringInit(local_idx, var_stmt.value, var_stmt.span);
            } else if (is_slice) {
                try self.lowerSliceInit(local_idx, var_stmt.value, type_idx, var_stmt.span);
            } else if (is_array) {
                try self.lowerArrayInit(local_idx, var_stmt.value, var_stmt.span);
            } else if (is_struct_literal) {
                try self.lowerStructInit(local_idx, var_stmt.value, var_stmt.span);
                // Register scope_destroy for struct literals with free()
                try self.maybeRegisterScopeDestroy(local_idx, type_idx);
            } else if (is_tuple_literal) {
                try self.lowerTupleInit(local_idx, var_stmt.value, var_stmt.span);
            } else if (self.type_reg.get(type_idx) == .union_type) {
                try self.lowerUnionInit(local_idx, var_stmt.value, type_idx, var_stmt.span);
            } else {
                const type_info = self.type_reg.get(type_idx);
                const is_compound_copy = (type_info == .struct_type or type_info == .tuple) and value_expr != null and
                    (value_expr.? == .field_access or value_expr.? == .index or value_expr.? == .deref);
                const is_struct_copy = is_compound_copy;
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
                    // `weak var` skips ARC entirely — no retain, no cleanup.
                    // This breaks reference cycles (parent↔child, delegate patterns).
                    // Reference: Swift's `weak` and `unowned` modifiers.
                    if (!var_stmt.is_weak and self.type_reg.couldBeARC(type_idx)) {
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
                                const retained = try fb.emitCall("retain", &retain_args, false, type_idx, var_stmt.span);
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

    fn lowerDestructureStmt(self: *Lowerer, ds: ast.DestructureStmt) !void {
        const fb = self.current_func orelse return;

        // Infer the tuple type from the RHS
        const tuple_type_idx = self.inferExprType(ds.value);
        const tuple_info = self.type_reg.get(tuple_type_idx);
        if (tuple_info != .tuple) return;
        const tup = tuple_info.tuple;

        // Check if RHS is a tuple literal — can lower each element directly
        const value_node_ast = self.tree.getNode(ds.value);
        const value_expr = if (value_node_ast) |n| n.asExpr() else null;
        const is_tuple_literal = if (value_expr) |e| e == .tuple_literal else false;

        if (is_tuple_literal) {
            // Direct: lower each tuple element to its own local
            const tl = value_expr.?.tuple_literal;
            for (ds.bindings, 0..) |b, i| {
                const elem_type = tup.element_types[i];
                const elem_size = self.type_reg.sizeOf(elem_type);
                const local_idx = try fb.addLocalWithSize(b.name, elem_type, !ds.is_const, elem_size);
                if (elem_type == TypeRegistry.STRING) {
                    try self.lowerStringInit(local_idx, tl.elements[i], ds.span);
                } else {
                    const val = try self.lowerExprNode(tl.elements[i]);
                    if (val != ir.null_node) _ = try fb.emitStoreLocal(local_idx, val, ds.span);
                }
            }
        } else {
            // General: lower RHS to a temp tuple local, then extract each element
            const tuple_size = self.type_reg.sizeOf(tuple_type_idx);
            const temp_idx = try fb.addLocalWithSize("__destruct_tmp", tuple_type_idx, true, tuple_size);

            // Check if RHS is itself a tuple literal expression or a function call
            // Lower the value and store into temp
            const rhs_node_ast = self.tree.getNode(ds.value);
            const rhs_expr = if (rhs_node_ast) |n| n.asExpr() else null;
            const is_rhs_tuple_lit = if (rhs_expr) |e| e == .tuple_literal else false;
            if (is_rhs_tuple_lit) {
                try self.lowerTupleInit(temp_idx, ds.value, ds.span);
            } else {
                const rhs_val = try self.lowerExprNode(ds.value);
                if (rhs_val == ir.null_node) return;
                _ = try fb.emitStoreLocal(temp_idx, rhs_val, ds.span);
            }

            // Extract each element from the temp
            for (ds.bindings, 0..) |b, i| {
                const elem_type = tup.element_types[i];
                const elem_size = self.type_reg.sizeOf(elem_type);
                const local_idx = try fb.addLocalWithSize(b.name, elem_type, !ds.is_const, elem_size);
                const offset: i64 = @intCast(self.type_reg.tupleElementOffset(tuple_type_idx, @intCast(i)));

                if (elem_type == TypeRegistry.STRING) {
                    // Compound decomposition: extract ptr and len from tuple field
                    const ptr_val = try fb.emitFieldLocal(temp_idx, @intCast(i * 2), offset, TypeRegistry.I64, ds.span);
                    const len_val = try fb.emitFieldLocal(temp_idx, @intCast(i * 2 + 1), offset + 8, TypeRegistry.I64, ds.span);
                    _ = try fb.emitStoreLocalField(local_idx, 0, 0, ptr_val, ds.span);
                    _ = try fb.emitStoreLocalField(local_idx, 1, 8, len_val, ds.span);
                } else {
                    const elem_val = try fb.emitFieldLocal(temp_idx, @intCast(i), offset, elem_type, ds.span);
                    _ = try fb.emitStoreLocal(local_idx, elem_val, ds.span);
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
                    _ = try fb.emitCall("memset_zero", &args, false, TypeRegistry.VOID, span);
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
        const struct_type_idx: TypeIndex = if (struct_init.type_name.len == 0) blk: {
            if (self.chk.expr_types.get(value_idx)) |resolved| break :blk resolved;
            return;
        } else if (struct_init.type_args.len > 0)
            self.resolveGenericTypeName(struct_init.type_name, struct_init.type_args)
        else
            self.type_reg.lookupByName(struct_init.type_name) orelse return;
        if (struct_type_idx == TypeRegistry.VOID) return;
        const type_info = self.type_reg.get(struct_type_idx);
        const struct_type = if (type_info == .struct_type) type_info.struct_type else return;

        // WasmGC path: ALL structs are GC objects — no stack allocation
        // Reference: Kotlin/Dart WasmGC strategy — no dual representation
        if (self.target.isWasmGC()) {
            const type_name = if (struct_init.type_args.len > 0) struct_type.name else struct_init.type_name;
            var field_values = try self.allocator.alloc(ir.NodeIndex, struct_type.fields.len);
            for (field_values) |*fv| fv.* = ir.null_node;
            for (struct_init.fields) |field_init| {
                for (struct_type.fields, 0..) |struct_field, i| {
                    if (std.mem.eql(u8, struct_field.name, field_init.name)) {
                        field_values[i] = try self.lowerExprNode(field_init.value);
                        break;
                    }
                }
            }
            for (struct_type.fields, 0..) |struct_field, i| {
                if (field_values[i] == ir.null_node) {
                    if (struct_field.default_value != null_node) {
                        field_values[i] = try self.lowerExprNode(struct_field.default_value);
                    } else {
                        field_values[i] = try fb.emitConstInt(0, struct_field.type_idx, span);
                    }
                }
            }
            const gc_ref = try fb.emitGcStructNew(type_name, field_values, struct_type_idx, span);
            _ = try fb.emitStoreLocal(local_idx, gc_ref, span);
            return;
        }

        // ARC path (unchanged): store fields to local via offset
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
        // Emit default values for omitted fields (Zig: Sema structInit default_field_values)
        for (struct_type.fields, 0..) |struct_field, i| {
            if (struct_field.default_value == null_node) continue;
            var provided = false;
            for (struct_init.fields) |field_init| {
                if (std.mem.eql(u8, struct_field.name, field_init.name)) { provided = true; break; }
            }
            if (provided) continue;
            const field_idx: u32 = @intCast(i);
            const field_offset: i64 = @intCast(struct_field.offset);
            const value_node_ir = try self.lowerExprNode(struct_field.default_value);
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
        }
    }

    /// Lower tuple literal directly into a target local.
    /// Same pattern as lowerStructInit — stores each element at its computed offset.
    fn lowerTupleInit(self: *Lowerer, local_idx: ir.LocalIdx, value_idx: NodeIndex, span: Span) !void {
        const fb = self.current_func orelse return;
        const value_node = self.tree.getNode(value_idx) orelse return;
        const value_expr = value_node.asExpr() orelse return;
        if (value_expr != .tuple_literal) return;
        const tl = value_expr.tuple_literal;
        // Infer tuple type from checker
        const tuple_type_idx = self.inferExprType(value_idx);
        for (tl.elements, 0..) |elem_idx, i| {
            const value_ir = try self.lowerExprNode(elem_idx);
            const offset: i64 = @intCast(self.type_reg.tupleElementOffset(tuple_type_idx, @intCast(i)));
            _ = try fb.emitStoreLocalField(local_idx, @intCast(i), offset, value_ir, span);
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

    /// Lower string interpolation: "text${expr}text${expr}text"
    /// Each text segment becomes a const string, each expr segment is evaluated.
    /// Integer expressions are converted via cot_int_to_string.
    /// All segments are chained with cot_string_concat.
    fn lowerStringInterp(self: *Lowerer, si: ast.StringInterp) Error!ir.NodeIndex {
        const fb = self.current_func orelse return ir.null_node;
        const ptr_type = try self.type_reg.makePointer(TypeRegistry.U8);

        // Convert each segment to a string IR node
        var parts = std.ArrayListUnmanaged(ir.NodeIndex){};
        defer parts.deinit(self.allocator);

        for (si.segments) |seg| {
            switch (seg) {
                .text => |text| {
                    // Process escape sequences in text
                    var buf: [4096]u8 = undefined;
                    const unescaped = unescapeString(text, &buf);
                    if (unescaped.len == 0) continue;
                    const copied = try self.allocator.dupe(u8, unescaped);
                    const str_idx = try fb.addStringLiteral(copied);
                    const str_node = try fb.emitConstSlice(str_idx, si.span);
                    try parts.append(self.allocator, str_node);
                },
                .expr => |expr_idx| {
                    const expr_type = self.inferExprType(expr_idx);
                    const expr_val = try self.lowerExprNode(expr_idx);

                    if (expr_type == TypeRegistry.STRING) {
                        // String expression: use directly
                        try parts.append(self.allocator, expr_val);
                    } else {
                        // Integer expression: convert via cot_int_to_string
                        // Allocate 21-byte stack buffer for digit conversion
                        const buf_local = try fb.addLocalWithSize("__interp_buf", TypeRegistry.I64, true, 24); // 24 bytes (aligned)
                        const buf_addr = try fb.emitAddrLocal(buf_local, TypeRegistry.I64, si.span);
                        var its_args = [_]ir.NodeIndex{ expr_val, buf_addr };
                        const str_len = try fb.emitCall("int_to_string", &its_args, false, TypeRegistry.I64, si.span);
                        // String starts at buf_addr + 21 - str_len
                        // str_ptr = buf_addr + 21 - str_len
                        const twenty_one = try fb.emitConstInt(21, TypeRegistry.I64, si.span);
                        const offset = try fb.emitBinary(.sub, twenty_one, str_len, TypeRegistry.I64, si.span);
                        const str_ptr = try fb.emitBinary(.add, buf_addr, offset, TypeRegistry.I64, si.span);
                        // Build string_header(str_ptr, str_len)
                        const str_node = try fb.emit(ir.Node.init(.{ .string_header = .{ .ptr = str_ptr, .len = str_len } }, TypeRegistry.STRING, si.span));
                        try parts.append(self.allocator, str_node);
                    }
                },
            }
        }

        if (parts.items.len == 0) {
            // Empty interpolation — return empty string
            const empty = try self.allocator.dupe(u8, "");
            const str_idx = try fb.addStringLiteral(empty);
            return try fb.emitConstSlice(str_idx, si.span);
        }

        // Chain all parts with cot_string_concat
        var result = parts.items[0];
        for (parts.items[1..]) |part| {
            const r_ptr = try fb.emitSlicePtr(result, ptr_type, si.span);
            const r_len = try fb.emitSliceLen(result, si.span);
            const p_ptr = try fb.emitSlicePtr(part, ptr_type, si.span);
            const p_len = try fb.emitSliceLen(part, si.span);
            var concat_args = [_]ir.NodeIndex{ r_ptr, r_len, p_ptr, p_len };
            const new_ptr = try fb.emitCall("string_concat", &concat_args, false, TypeRegistry.I64, si.span);
            const new_len = try fb.emitBinary(.add, r_len, p_len, TypeRegistry.I64, si.span);
            result = try fb.emit(ir.Node.init(.{ .string_header = .{ .ptr = new_ptr, .len = new_len } }, TypeRegistry.STRING, si.span));
        }

        return result;
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
        // Reference: references/go/src/runtime/slice.go lines 16-20
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
                    // ARC: Release old value before storing new if local has cleanup
                    // WasmGC: skip retain/release — GC handles lifetimes
                    if (!self.target.isWasmGC() and self.cleanup_stack.hasCleanupForLocal(local_idx)) {
                        const local_type = fb.locals.items[local_idx].type_idx;
                        const old_value = try fb.emitLoadLocal(local_idx, local_type, assign.span);
                        var release_args = [_]ir.NodeIndex{old_value};
                        _ = try fb.emitCall("release", &release_args, false, TypeRegistry.VOID, assign.span);

                        // Check if RHS is +1 (owned) or +0 (borrowed)
                        // Reference: Swift SILGenAssign.cpp — assignment consumes +1, retains +0
                        const rhs_node = self.tree.getNode(assign.value);
                        const rhs_expr = if (rhs_node) |n| n.asExpr() else null;
                        const is_owned = if (rhs_expr) |e| (e == .new_expr or e == .call) else false;
                        if (!is_owned) {
                            // +0 value (borrowed): retain before storing
                            var retain_args = [_]ir.NodeIndex{value_node};
                            const retained = try fb.emitCall("retain", &retain_args, false, local_type, assign.span);
                            _ = try fb.emitStoreLocal(local_idx, retained, assign.span);
                            self.cleanup_stack.updateValueForLocal(local_idx, retained);
                        } else {
                            // +1 value (owned): store directly, no retain needed
                            _ = try fb.emitStoreLocal(local_idx, value_node, assign.span);
                            self.cleanup_stack.updateValueForLocal(local_idx, value_node);
                        }
                    } else {
                        const local_type = fb.locals.items[local_idx].type_idx;
                        if (local_type == TypeRegistry.STRING) {
                            // Compound reassignment: decompose into ptr + len
                            // Must match lowerStringInit pattern (line ~1130)
                            const ptr_type = self.type_reg.makePointer(TypeRegistry.U8) catch TypeRegistry.VOID;
                            const ptr_val = try fb.emitSlicePtr(value_node, ptr_type, assign.span);
                            const len_val = try fb.emitSliceLen(value_node, assign.span);
                            _ = try fb.emitStoreLocalField(local_idx, 0, 0, ptr_val, assign.span);
                            _ = try fb.emitStoreLocalField(local_idx, 1, 8, len_val, assign.span);
                        } else if (self.type_reg.isSlice(local_type)) {
                            // Slice compound reassignment: ptr + len + cap
                            // Must match lowerSliceInit pattern (line ~1141)
                            const slice_info = self.type_reg.get(local_type);
                            const elem_type = if (slice_info == .slice) slice_info.slice.elem else TypeRegistry.U8;
                            const ptr_type = self.type_reg.makePointer(elem_type) catch TypeRegistry.VOID;
                            const ptr_val = try fb.emitSlicePtr(value_node, ptr_type, assign.span);
                            const len_val = try fb.emitSliceLen(value_node, assign.span);
                            const cap_val = try fb.emitSliceCap(value_node, assign.span);
                            _ = try fb.emitStoreLocalField(local_idx, 0, 0, ptr_val, assign.span);
                            _ = try fb.emitStoreLocalField(local_idx, 1, 8, len_val, assign.span);
                            _ = try fb.emitStoreLocalField(local_idx, 2, 16, cap_val, assign.span);
                        } else {
                            _ = try fb.emitStoreLocal(local_idx, value_node, assign.span);
                        }
                    }
                } else if (self.builder.lookupGlobal(id.name)) |g| {
                    _ = try fb.emitGlobalStore(g.idx, id.name, value_node, assign.span);
                }
            },
            .field_access => |fa| try self.lowerFieldAssign(fa, value_node, assign.value, assign.span),
            .index => |idx| try self.lowerIndexAssign(idx, value_node, assign.value, assign.span),
            .deref => |d| {
                const ptr_node = try self.lowerExprNode(d.operand);
                // ARC: release old value at *ptr before storing new (skip for WasmGC)
                // Reference: Swift SILGenLValue.cpp — assign into lvalue releases old, consumes +1 / retains +0
                if (!self.target.isWasmGC()) {
                    const ptr_type_idx = self.inferExprType(d.operand);
                    const ptr_type_info = self.type_reg.get(ptr_type_idx);
                    if (ptr_type_info == .pointer) {
                        const pointee_type = ptr_type_info.pointer.elem;
                        if (self.type_reg.couldBeARC(pointee_type) and self.baseHasCleanup(d.operand)) {
                            const old_val = try fb.emitPtrLoadValue(ptr_node, pointee_type, assign.span);
                            var release_args = [_]ir.NodeIndex{old_val};
                            _ = try fb.emitCall("release", &release_args, false, TypeRegistry.VOID, assign.span);

                            // Retain new value if borrowed (+0)
                            const rhs_node = self.tree.getNode(assign.value);
                            const rhs_expr = if (rhs_node) |n| n.asExpr() else null;
                            const is_owned = if (rhs_expr) |e| (e == .new_expr or e == .call) else false;
                            if (!is_owned) {
                                var retain_args = [_]ir.NodeIndex{value_node};
                                const retained = try fb.emitCall("retain", &retain_args, false, pointee_type, assign.span);
                                _ = try fb.emitPtrStoreValue(ptr_node, retained, assign.span);
                                return;
                            }
                        }
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

        // WasmGC path: use gc_struct_set instead of offset-based stores
        if (self.target.isWasmGC()) {
            for (struct_type.fields, 0..) |field, i| {
                if (std.mem.eql(u8, field.name, fa.field)) {
                    const base_val = try self.lowerExprNode(fa.base);
                    _ = try fb.emitGcStructSet(base_val, struct_type.name, @intCast(i), value_node, span);
                    return;
                }
            }
            return;
        }

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
                        _ = try fb.emitCall("release", &release_args, false, TypeRegistry.VOID, span);

                        // Retain new value if borrowed (+0)
                        const rhs_node = self.tree.getNode(rhs_ast);
                        const rhs_expr = if (rhs_node) |n| n.asExpr() else null;
                        const is_owned = if (rhs_expr) |e| (e == .new_expr or e == .call) else false;
                        if (!is_owned) {
                            var retain_args = [_]ir.NodeIndex{value_node};
                            const retained = try fb.emitCall("retain", &retain_args, false, field.type_idx, span);
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
                } else if (base_expr == .field_access) {
                    // Nested struct field assign: o.inner.val = 42
                    // Resolve the base field chain to an address, then store to the final field.
                    const base_addr = try self.resolveStructFieldAddr(fa.base);
                    if (base_addr != ir.null_node) {
                        _ = try fb.emitStoreFieldValue(base_addr, field_idx, field_offset, value_node, span);
                    }
                }
                break;
            }
        }
    }

    /// Resolve a struct field access expression to its memory address.
    /// Follows Go's recursive s.addr() pattern for ODOT:
    ///   addr(o.inner) = addr(o) + offset(inner)
    /// This handles arbitrary nesting: o.a.b.c resolves to base_addr + offset(a) + offset(b).
    fn resolveStructFieldAddr(self: *Lowerer, node_idx: NodeIndex) !ir.NodeIndex {
        const fb = self.current_func orelse return ir.null_node;
        const node = self.tree.getNode(node_idx) orelse return ir.null_node;
        const expr = node.asExpr() orelse return ir.null_node;

        if (expr == .ident) {
            // Base case: local variable — return its address
            if (fb.lookupLocal(expr.ident.name)) |local_idx| {
                const local_type = fb.locals.items[local_idx].type_idx;
                const ptr_type = self.type_reg.makePointer(local_type) catch TypeRegistry.VOID;
                return try fb.emitAddrLocal(local_idx, ptr_type, expr.ident.span);
            }
            if (self.builder.lookupGlobal(expr.ident.name)) |g| {
                const ptr_type = self.type_reg.makePointer(g.global.type_idx) catch TypeRegistry.VOID;
                return try fb.emitAddrGlobal(g.idx, expr.ident.name, ptr_type, expr.ident.span);
            }
            return ir.null_node;
        }

        if (expr == .field_access) {
            const fa = expr.field_access;
            const base_type_idx = self.inferExprType(fa.base);
            const base_type = self.type_reg.get(base_type_idx);

            const struct_type = switch (base_type) {
                .struct_type => |st| st,
                .pointer => |ptr| blk: {
                    const elem = self.type_reg.get(ptr.elem);
                    if (elem == .struct_type) break :blk elem.struct_type;
                    return ir.null_node;
                },
                else => return ir.null_node,
            };

            // Recursively get base address (Go: p := s.addr(n.X))
            const base_addr = if (base_type == .pointer)
                try self.lowerExprNode(fa.base) // pointer base: load the pointer value
            else
                try self.resolveStructFieldAddr(fa.base); // struct base: recurse

            if (base_addr == ir.null_node) return ir.null_node;

            // Find field offset and add it (Go: OpOffPtr with n.Offset())
            for (struct_type.fields) |field| {
                if (std.mem.eql(u8, field.name, fa.field)) {
                    const field_offset: i64 = @intCast(field.offset);
                    if (field_offset == 0) return base_addr;
                    const field_ptr_type = self.type_reg.makePointer(field.type_idx) catch TypeRegistry.VOID;
                    return try fb.emitAddrOffset(base_addr, field_offset, field_ptr_type, fa.span);
                }
            }
            return ir.null_node;
        }

        return ir.null_node;
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
                    _ = try fb.emitCall("release", &release_args, false, TypeRegistry.VOID, span);

                    // Retain new value if borrowed (+0)
                    const rhs_node = self.tree.getNode(rhs_ast);
                    const rhs_expr = if (rhs_node) |n| n.asExpr() else null;
                    const is_owned = if (rhs_expr) |e| (e == .new_expr or e == .call) else false;
                    const store_val = if (!is_owned) blk: {
                        var retain_args = [_]ir.NodeIndex{value_node};
                        break :blk try fb.emitCall("retain", &retain_args, false, elem_type, span);
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

    fn lowerIf(self: *Lowerer, if_stmt: ast.IfStmt) !bool {
        // Optional unwrap: if expr |val| { ... } — Zig payload capture pattern
        if (if_stmt.capture.len > 0) return try self.lowerIfOptional(if_stmt);
        // Comptime const-fold: eliminate dead branches for platform conditionals
        if (self.chk.evalConstExpr(if_stmt.condition)) |cond_val| {
            if (cond_val != 0) {
                return try self.lowerBlockNode(if_stmt.then_branch);
            } else if (if_stmt.else_branch != null_node) {
                return try self.lowerBlockNode(if_stmt.else_branch);
            }
            return false;
        }
        const fb = self.current_func orelse return false;
        const then_block = try fb.newBlock("then");
        const else_block = if (if_stmt.else_branch != null_node) try fb.newBlock("else") else null;
        const merge_block = try fb.newBlock("if.end");
        const cond_node = try self.lowerExprNode(if_stmt.condition);
        if (cond_node == ir.null_node) return false;
        _ = try fb.emitBranch(cond_node, then_block, else_block orelse merge_block, if_stmt.span);
        fb.setBlock(then_block);
        if (!try self.lowerBlockNode(if_stmt.then_branch)) _ = try fb.emitJump(merge_block, if_stmt.span);
        if (else_block) |eb| {
            fb.setBlock(eb);
            if (!try self.lowerBlockNode(if_stmt.else_branch)) _ = try fb.emitJump(merge_block, if_stmt.span);
        }
        fb.setBlock(merge_block);
        return false;
    }

    /// Lower if-optional: if expr |val| { ... } else { ... }
    /// Same pattern as catch |err| capture (lower.zig:4220-4228).
    fn lowerIfOptional(self: *Lowerer, if_stmt: ast.IfStmt) !bool {
        const fb = self.current_func orelse return false;

        // Evaluate the optional expression
        const opt_val = try self.lowerExprNode(if_stmt.condition);
        if (opt_val == ir.null_node) return false;

        // Check if non-null: val != null
        const null_val = try fb.emit(ir.Node.init(.const_null, TypeRegistry.UNTYPED_NULL, if_stmt.span));
        const is_non_null = try fb.emitBinary(.ne, opt_val, null_val, TypeRegistry.BOOL, if_stmt.span);

        const then_block = try fb.newBlock("if.opt.then");
        const else_block = if (if_stmt.else_branch != null_node) try fb.newBlock("if.opt.else") else null;
        const merge_block = try fb.newBlock("if.opt.end");
        _ = try fb.emitBranch(is_non_null, then_block, else_block orelse merge_block, if_stmt.span);

        // Then block: unwrap and bind capture variable
        fb.setBlock(then_block);
        const scope_depth = fb.markScopeEntry();
        const opt_type_idx = self.inferExprType(if_stmt.condition);
        const opt_info = self.type_reg.get(opt_type_idx);
        const elem_type = if (opt_info == .optional) opt_info.optional.elem else opt_type_idx;
        const unwrapped = try fb.emitUnary(.optional_unwrap, opt_val, elem_type, if_stmt.span);
        const capture_local = try fb.addLocalWithSize(if_stmt.capture, elem_type, false, self.type_reg.sizeOf(elem_type));
        _ = try fb.emitStoreLocal(capture_local, unwrapped, if_stmt.span);
        if (!try self.lowerBlockNode(if_stmt.then_branch)) _ = try fb.emitJump(merge_block, if_stmt.span);
        fb.restoreScope(scope_depth);

        // Else block
        if (else_block) |eb| {
            fb.setBlock(eb);
            if (!try self.lowerBlockNode(if_stmt.else_branch)) _ = try fb.emitJump(merge_block, if_stmt.span);
        }

        fb.setBlock(merge_block);
        return false;
    }

    fn lowerWhile(self: *Lowerer, while_stmt: ast.WhileStmt) !void {
        // Optional capture: while (expr) |val| { ... } — Zig payload capture pattern
        if (while_stmt.capture.len > 0) return try self.lowerWhileOptional(while_stmt);
        const fb = self.current_func orelse return;
        const cond_block = try fb.newBlock("while.cond");
        const body_block = try fb.newBlock("while.body");
        const exit_block = try fb.newBlock("while.end");
        // Continue expression block (Zig: evaluated after body, before re-checking condition)
        const cont_block = if (while_stmt.continue_expr != null_node) try fb.newBlock("while.cont") else cond_block;
        _ = try fb.emitJump(cond_block, while_stmt.span);
        fb.setBlock(cond_block);
        const cond_node = try self.lowerExprNode(while_stmt.condition);
        if (cond_node == ir.null_node) return;
        _ = try fb.emitBranch(cond_node, body_block, exit_block, while_stmt.span);
        try self.loop_stack.append(self.allocator, .{ .cond_block = cont_block, .exit_block = exit_block, .cleanup_depth = self.cleanup_stack.getScopeDepth(), .label = while_stmt.label });
        fb.setBlock(body_block);
        if (!try self.lowerBlockNode(while_stmt.body)) _ = try fb.emitJump(cont_block, while_stmt.span);
        // Emit continue expression block
        if (while_stmt.continue_expr != null_node) {
            fb.setBlock(cont_block);
            try self.lowerContinueExpr(while_stmt.continue_expr);
            _ = try fb.emitJump(cond_block, while_stmt.span);
        }
        _ = self.loop_stack.pop();
        fb.setBlock(exit_block);
    }

    /// Lower while-optional: while (expr) |val| { ... }
    /// Pattern: evaluate optional, store to local, check non-null, unwrap into capture, loop.
    /// Reference: Zig AstGen.zig whileExpr with payload capture.
    fn lowerWhileOptional(self: *Lowerer, while_stmt: ast.WhileStmt) !void {
        const fb = self.current_func orelse return;
        const cond_block = try fb.newBlock("while.opt.cond");
        const body_block = try fb.newBlock("while.opt.body");
        const exit_block = try fb.newBlock("while.opt.end");
        const cont_block = if (while_stmt.continue_expr != null_node) try fb.newBlock("while.opt.cont") else cond_block;

        // Allocate a local to hold the optional result across blocks
        const opt_type_idx = self.inferExprType(while_stmt.condition);
        const opt_info = self.type_reg.get(opt_type_idx);
        const elem_type = if (opt_info == .optional) opt_info.optional.elem else opt_type_idx;
        const opt_local = try fb.addLocalWithSize("__while_opt", opt_type_idx, true, self.type_reg.sizeOf(opt_type_idx));

        _ = try fb.emitJump(cond_block, while_stmt.span);
        fb.setBlock(cond_block);

        // Evaluate the optional expression and store to local
        const opt_val = try self.lowerExprNode(while_stmt.condition);
        if (opt_val == ir.null_node) return;
        _ = try fb.emitStoreLocal(opt_local, opt_val, while_stmt.span);

        // Check if non-null: val != null
        const null_val = try fb.emit(ir.Node.init(.const_null, TypeRegistry.UNTYPED_NULL, while_stmt.span));
        const is_non_null = try fb.emitBinary(.ne, opt_val, null_val, TypeRegistry.BOOL, while_stmt.span);
        _ = try fb.emitBranch(is_non_null, body_block, exit_block, while_stmt.span);

        // Body block: reload from local, unwrap and bind capture variable
        try self.loop_stack.append(self.allocator, .{ .cond_block = cont_block, .exit_block = exit_block, .cleanup_depth = self.cleanup_stack.getScopeDepth(), .label = while_stmt.label });
        fb.setBlock(body_block);
        const scope_depth = fb.markScopeEntry();
        const opt_reload = try fb.emitLoadLocal(opt_local, opt_type_idx, while_stmt.span);
        const unwrapped = try fb.emitUnary(.optional_unwrap, opt_reload, elem_type, while_stmt.span);
        const capture_local = try fb.addLocalWithSize(while_stmt.capture, elem_type, false, self.type_reg.sizeOf(elem_type));
        _ = try fb.emitStoreLocal(capture_local, unwrapped, while_stmt.span);
        if (!try self.lowerBlockNode(while_stmt.body)) _ = try fb.emitJump(cont_block, while_stmt.span);
        fb.restoreScope(scope_depth);

        // Continue expression block
        if (while_stmt.continue_expr != null_node) {
            fb.setBlock(cont_block);
            try self.lowerContinueExpr(while_stmt.continue_expr);
            _ = try fb.emitJump(cond_block, while_stmt.span);
        }

        _ = self.loop_stack.pop();
        fb.setBlock(exit_block);
    }

    /// Lower a continue expression (could be a statement like assignment, or just an expression)
    fn lowerContinueExpr(self: *Lowerer, node_idx: NodeIndex) !void {
        const node = self.tree.getNode(node_idx) orelse return;
        if (node.asStmt()) |stmt| {
            _ = try self.lowerStmt(stmt);
        } else {
            _ = try self.lowerExprNode(node_idx);
        }
    }

    fn lowerFor(self: *Lowerer, for_stmt: ast.ForStmt) !void {
        const fb = self.current_func orelse return;

        // Inline for: unroll at compile time — Zig AstGen.zig:6863
        if (for_stmt.is_inline) {
            try self.lowerInlineFor(for_stmt);
            return;
        }

        // Handle numeric range: for i in start..end { }
        if (for_stmt.isRange()) {
            try self.lowerForRange(for_stmt);
            return;
        }

        const iter_type = self.inferExprType(for_stmt.iterable);
        const iter_info = self.type_reg.get(iter_type);

        // Map iteration has different structure — dispatch to dedicated handler
        // After monomorphization, Map(K,V) is a struct_type with name "Map(...)".
        // Look up generic inst info to get K and V type args.
        if (iter_info == .map) {
            try self.lowerForMap(for_stmt, iter_type, iter_info.map);
            return;
        }
        if (iter_info == .struct_type and std.mem.startsWith(u8, iter_info.struct_type.name, "Map(")) {
            if (checker.parseMapTypeArgs(iter_info.struct_type.name)) |args| {
                try self.lowerForMap(for_stmt, iter_type, .{ .key = args[0], .value = args[1] });
                return;
            }
        }

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

        try self.loop_stack.append(self.allocator, .{ .cond_block = incr_block, .exit_block = exit_block, .cleanup_depth = self.cleanup_stack.getScopeDepth(), .label = for_stmt.label });

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
    /// Inline for: unroll loop body at compile time with comptime-known range or comptime array.
    /// Each iteration binds the loop variable as a const in const_values.
    fn lowerInlineFor(self: *Lowerer, for_stmt: ast.ForStmt) !void {
        if (for_stmt.isRange()) {
            const start_val = self.chk.evalConstExpr(for_stmt.range_start) orelse return;
            const end_val = self.chk.evalConstExpr(for_stmt.range_end) orelse return;
            var i = start_val;
            while (i < end_val) : (i += 1) {
                // Bind iteration variable as comptime const
                try self.const_values.put(for_stmt.binding, i);
                _ = try self.lowerBlockNode(for_stmt.body);
            }
            // Clean up binding after unrolling
            _ = self.const_values.remove(for_stmt.binding);
            return;
        }
        // Comptime array iteration: inline for field in @typeInfo(T).fields
        const iter_val = self.chk.evalComptimeValue(for_stmt.iterable) orelse return;
        if (iter_val != .array) return;
        for (iter_val.array.elements.items, 0..) |elem, idx| {
            // Store comptime value for binding resolution
            try self.comptime_value_vars.put(for_stmt.binding, elem);
            // For int elements, also store in const_values for direct integer access
            if (elem.asInt()) |v| try self.const_values.put(for_stmt.binding, v);
            if (for_stmt.index_binding) |ib| try self.const_values.put(ib, @intCast(idx));
            _ = try self.lowerBlockNode(for_stmt.body);
        }
        _ = self.comptime_value_vars.remove(for_stmt.binding);
        _ = self.const_values.remove(for_stmt.binding);
        if (for_stmt.index_binding) |ib| _ = self.const_values.remove(ib);
    }

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
            .label = for_stmt.label,
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

    /// Lower `for k, v in map { }` — iterates occupied slots in hash table.
    /// Reference: Zig array_hash_map.zig:749-770 (Iterator with next() returning ?Entry).
    /// Go range.go:241-270 (desugars to mapIterStart/mapIterNext).
    /// Map struct layout: keys(0), values(8), states(16), count(24), capacity(32).
    /// States: 0=empty, 1=occupied, 2=tombstone.
    fn lowerForMap(self: *Lowerer, for_stmt: ast.ForStmt, iter_type: TypeIndex, map_info: types.MapType) !void {
        const fb = self.current_func orelse return;
        _ = iter_type;
        const key_type = map_info.key;
        const val_type = map_info.value;
        const key_size: u32 = self.type_reg.sizeOf(key_type);
        const val_size: u32 = self.type_reg.sizeOf(val_type);

        // Get the map local
        const iter_node = self.tree.getNode(for_stmt.iterable) orelse return;
        const iter_expr = iter_node.asExpr() orelse return;
        if (iter_expr != .ident) return;
        const map_local = fb.lookupLocal(iter_expr.ident.name) orelse return;

        // Read map fields into locals BEFORE the loop (same pattern as lowerFor for arrays/slices).
        // We use emitFieldLocal to read from the struct, then store into simple i64 locals.
        const idx_name = try std.fmt.allocPrint(self.allocator, "__map_idx_{d}", .{self.temp_counter});
        self.temp_counter += 1;
        const idx_local = try fb.addLocalWithSize(idx_name, TypeRegistry.I64, true, 8);
        const zero = try fb.emitConstInt(0, TypeRegistry.I64, for_stmt.span);
        _ = try fb.emitStoreLocal(idx_local, zero, for_stmt.span);

        // Use m.count for total entries (optimization: early exit when found_count reaches count)
        // But loop 0..capacity is correct for hash table iteration, checking states.
        // Read capacity from the struct into a simple local
        const cap_name = try std.fmt.allocPrint(self.allocator, "__map_cap_{d}", .{self.temp_counter});
        self.temp_counter += 1;
        const cap_local = try fb.addLocalWithSize(cap_name, TypeRegistry.I64, false, 8);
        const cap_field = try fb.emitFieldLocal(map_local, 4, 32, TypeRegistry.I64, for_stmt.span);
        _ = try fb.emitStoreLocal(cap_local, cap_field, for_stmt.span);

        // Read keys/values/states pointers into locals
        const keys_name = try std.fmt.allocPrint(self.allocator, "__map_keys_{d}", .{self.temp_counter});
        self.temp_counter += 1;
        const keys_local = try fb.addLocalWithSize(keys_name, TypeRegistry.I64, false, 8);
        const keys_field = try fb.emitFieldLocal(map_local, 0, 0, TypeRegistry.I64, for_stmt.span);
        _ = try fb.emitStoreLocal(keys_local, keys_field, for_stmt.span);

        const vals_name = try std.fmt.allocPrint(self.allocator, "__map_vals_{d}", .{self.temp_counter});
        self.temp_counter += 1;
        const vals_local = try fb.addLocalWithSize(vals_name, TypeRegistry.I64, false, 8);
        const vals_field = try fb.emitFieldLocal(map_local, 1, 8, TypeRegistry.I64, for_stmt.span);
        _ = try fb.emitStoreLocal(vals_local, vals_field, for_stmt.span);

        const states_name = try std.fmt.allocPrint(self.allocator, "__map_states_{d}", .{self.temp_counter});
        self.temp_counter += 1;
        const states_local = try fb.addLocalWithSize(states_name, TypeRegistry.I64, false, 8);
        const states_field = try fb.emitFieldLocal(map_local, 2, 16, TypeRegistry.I64, for_stmt.span);
        _ = try fb.emitStoreLocal(states_local, states_field, for_stmt.span);

        // Create loop blocks — same structure as lowerForRange
        const cond_block = try fb.newBlock("map.cond");
        const body_block = try fb.newBlock("map.body");
        const incr_block = try fb.newBlock("map.incr");
        const exit_block = try fb.newBlock("map.end");

        _ = try fb.emitJump(cond_block, for_stmt.span);

        // Condition: idx < capacity
        fb.setBlock(cond_block);
        const idx_cond = try fb.emitLoadLocal(idx_local, TypeRegistry.I64, for_stmt.span);
        const cap_cond = try fb.emitLoadLocal(cap_local, TypeRegistry.I64, for_stmt.span);
        const cond = try fb.emitBinary(.lt, idx_cond, cap_cond, TypeRegistry.BOOL, for_stmt.span);
        _ = try fb.emitBranch(cond, body_block, exit_block, for_stmt.span);

        // Push loop context for break/continue
        try self.loop_stack.append(self.allocator, .{
            .cond_block = incr_block,
            .exit_block = exit_block,
            .cleanup_depth = self.cleanup_stack.getScopeDepth(),
            .label = for_stmt.label,
        });

        // Body: check state[idx] == 1, if so bind key/value and execute user body
        fb.setBlock(body_block);
        const cur_idx = try fb.emitLoadLocal(idx_local, TypeRegistry.I64, for_stmt.span);

        // Read state at current index: *(states + idx * 8)
        const states_base = try fb.emitLoadLocal(states_local, TypeRegistry.I64, for_stmt.span);
        const state_val = try fb.emitIndexValue(states_base, cur_idx, 8, TypeRegistry.I64, for_stmt.span);
        const one = try fb.emitConstInt(1, TypeRegistry.I64, for_stmt.span);
        const is_occupied = try fb.emitBinary(.eq, state_val, one, TypeRegistry.BOOL, for_stmt.span);

        // Branch: if occupied, go to occupied block; else skip to increment
        const occupied_block = try fb.newBlock("map.occupied");
        _ = try fb.emitBranch(is_occupied, occupied_block, incr_block, for_stmt.span);

        // Occupied block: load key and value, execute body
        fb.setBlock(occupied_block);
        const idx_load = try fb.emitLoadLocal(idx_local, TypeRegistry.I64, for_stmt.span);

        // Load key from keys[idx]
        const keys_base = try fb.emitLoadLocal(keys_local, TypeRegistry.I64, for_stmt.span);
        const key_val = try fb.emitIndexValue(keys_base, idx_load, @intCast(key_size), key_type, for_stmt.span);

        // Load value from values[idx]
        const vals_base = try fb.emitLoadLocal(vals_local, TypeRegistry.I64, for_stmt.span);
        const val_val = try fb.emitIndexValue(vals_base, idx_load, @intCast(val_size), val_type, for_stmt.span);

        // Bind value (binding = second name = value)
        const val_local = try fb.addLocalWithSize(for_stmt.binding, val_type, false, val_size);
        _ = try fb.emitStoreLocal(val_local, val_val, for_stmt.span);

        // Bind key (index_binding = first name = key)
        if (for_stmt.index_binding) |key_binding| {
            const key_local = try fb.addLocalWithSize(key_binding, key_type, false, key_size);
            _ = try fb.emitStoreLocal(key_local, key_val, for_stmt.span);
        }

        // Execute loop body
        if (!try self.lowerBlockNode(for_stmt.body)) {
            _ = try fb.emitJump(incr_block, for_stmt.span);
        }

        // Increment: idx += 1
        fb.setBlock(incr_block);
        const idx_before = try fb.emitLoadLocal(idx_local, TypeRegistry.I64, for_stmt.span);
        const inc = try fb.emitConstInt(1, TypeRegistry.I64, for_stmt.span);
        const idx_after = try fb.emitBinary(.add, idx_before, inc, TypeRegistry.I64, for_stmt.span);
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
        // Pass node index for anonymous struct init resolution
        if (expr == .struct_init and expr.struct_init.type_name.len == 0) {
            return try self.lowerStructInitExpr(expr.struct_init, idx);
        }
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
            .await_expr => |ae| return try self.lowerAwaitExpr(ae),
            .catch_expr => |ce| return try self.lowerCatchExpr(ce),
            .error_literal => |el| return try self.lowerErrorLiteral(el),
            .builtin_call => |bc| return try self.lowerBuiltinCall(bc),
            .switch_expr => |se| return try self.lowerSwitchExpr(se),
            .struct_init => |si| return try self.lowerStructInitExpr(si, null),
            .tuple_literal => |tl| return try self.lowerTupleLiteral(tl),
            .new_expr => |ne| return try self.lowerNewExpr(ne),
            .closure_expr => |ce| return try self.lowerClosureExpr(ce),
            .string_interp => |si| return try self.lowerStringInterp(si),
            .comptime_block => |cb| {
                // Zig Sema pattern: comptime { body } must evaluate at compile time.
                // For blocks with statements, set up comptime context and evaluate
                const body_node = self.tree.getNode(cb.body) orelse return try fb.emitTrap(cb.span);
                const body_expr = body_node.asExpr() orelse return try fb.emitTrap(cb.span);
                if (body_expr == .block_expr) {
                    const blk = body_expr.block_expr;
                    const old_vars = self.chk.comptime_vars;
                    const new_vars = std.StringHashMap(checker.ComptimeValue).init(self.allocator);
                    self.chk.comptime_vars = new_vars;
                    defer {
                        if (self.chk.comptime_vars) |*cv| cv.deinit();
                        self.chk.comptime_vars = old_vars;
                    }
                    if (self.chk.evalComptimeBlock(blk.stmts, blk.expr)) |cv| {
                        return try self.emitComptimeValue(cv, cb.span);
                    }
                    return try fb.emitTrap(cb.span);
                }
                // Single expression — try evalComptimeValue, then legacy paths
                if (self.chk.evalComptimeValue(cb.body)) |cv| {
                    return try self.emitComptimeValue(cv, cb.span);
                }
                if (self.chk.evalConstExpr(cb.body)) |val| {
                    return try fb.emitConstInt(val, TypeRegistry.I64, cb.span);
                }
                if (self.chk.evalConstString(cb.body)) |str| {
                    const copied = try self.allocator.dupe(u8, str);
                    const str_idx = try fb.addStringLiteral(copied);
                    return try fb.emitConstSlice(str_idx, cb.span);
                }
                // Checker should have caught this; emit trap as defensive fallback
                return try fb.emitTrap(cb.span);
            },
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
            .unreachable_lit => {
                _ = try fb.emitTrap(lit.span);
                const dead_block = try fb.newBlock("unreachable.dead");
                fb.setBlock(dead_block);
                return ir.null_node;
            },
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
        // Check comptime value vars first (for inline for over comptime arrays)
        if (self.lookupComptimeValue(ident.name)) |cv| {
            return try self.emitComptimeValue(cv, ident.span);
        }
        if (self.const_values.get(ident.name)) |value| return try fb.emitConstInt(value, TypeRegistry.I64, ident.span);
        if (self.float_const_values.get(ident.name)) |fvalue| {
            const ftype = self.float_const_types.get(ident.name) orelse TypeRegistry.F64;
            return try fb.emitConstFloat(fvalue, ftype, ident.span);
        }
        if (fb.lookupLocal(ident.name)) |local_idx| {
            const local_type = fb.locals.items[local_idx].type_idx;
            if (self.type_reg.isArray(local_type)) return try fb.emitAddrLocal(local_idx, local_type, ident.span);
            return try fb.emitLoadLocal(local_idx, local_type, ident.span);
        }
        if (self.chk.scope.lookup(ident.name)) |sym| {
            if (sym.kind == .function) return try self.createFuncValue(ident.name, sym.type_idx, ident.span);
            if (sym.kind == .constant) {
                if (sym.const_value) |value| return try fb.emitConstInt(value, TypeRegistry.I64, ident.span);
                if (sym.float_const_value) |fvalue| return try fb.emitConstFloat(fvalue, sym.type_idx, ident.span);
            }
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
            const new_ptr = try fb.emitCall("string_concat", &args, false, TypeRegistry.I64, bin.span);

            // Compute new_len = len1 + len2
            const new_len = try fb.emitBinary(.add, len1, len2, TypeRegistry.I64, bin.span);

            // Create string_header(new_ptr, new_len) - this creates a string_make in SSA
            return try fb.emit(ir.Node.init(.{ .string_header = .{ .ptr = new_ptr, .len = new_len } }, TypeRegistry.STRING, bin.span));
        }
        // String equality: decompose to ptr/len and call cot_string_eq
        // Same pattern as @assertEq string handling (line 3528)
        if ((bin.op == .eql or bin.op == .neq) and self.inferExprType(bin.left) == TypeRegistry.STRING) {
            const ptr_type = try self.type_reg.makePointer(TypeRegistry.U8);
            const l_ptr = try fb.emitSlicePtr(left, ptr_type, bin.span);
            const l_len = try fb.emitSliceLen(left, bin.span);
            const r_ptr = try fb.emitSlicePtr(right, ptr_type, bin.span);
            const r_len = try fb.emitSliceLen(right, bin.span);
            var eq_args = [_]ir.NodeIndex{ l_ptr, l_len, r_ptr, r_len };
            const eq_result = try fb.emitCall("string_eq", &eq_args, false, TypeRegistry.I64, bin.span);
            const zero = try fb.emitConstInt(0, TypeRegistry.I64, bin.span);
            if (bin.op == .eql) {
                return try fb.emitBinary(.ne, eq_result, zero, TypeRegistry.BOOL, bin.span);
            } else {
                return try fb.emitBinary(.eq, eq_result, zero, TypeRegistry.BOOL, bin.span);
            }
        }
        if (bin.op == .kw_orelse) {
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
        const result = try fb.emitBinary(tokenToBinaryOp(bin.op), left, right, result_type, bin.span);

        // Div-by-zero check for integer division/modulo.
        // Reference: Go ssa.go — OpDiv64/OpMod64 check for zero divisor.
        // NOTE: Narrow type overflow checks were removed — Go's Wasm backend
        // does NOT emit runtime overflow checks (ssa.go, rewriteWasm.go).
        // Zig does comptime overflow in Sema.zig, not runtime Wasm checks.
        const result_info = self.type_reg.get(result_type);
        const is_integer_type = result_info == .basic and result_info.basic.isInteger();
        if (!self.release_mode and isDivOp(bin.op) and is_integer_type) {
            const zero = try fb.emitConstInt(0, TypeRegistry.I64, bin.span);
            const rhs_nz = try fb.emitBinary(.ne, right, zero, TypeRegistry.BOOL, bin.span);
            const ok_block = try fb.newBlock("divzero.ok");
            const fail_block = try fb.newBlock("divzero.fail");
            _ = try fb.emitBranch(rhs_nz, ok_block, fail_block, bin.span);
            fb.setBlock(fail_block);
            _ = try fb.emitTrap(bin.span);
            fb.setBlock(ok_block);
        }

        return result;
    }

    fn isDivOp(op: Token) bool {
        return switch (op) {
            .quo, .rem => true,
            else => false,
        };
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
        // Runtime safety: null unwrap check in debug mode — Zig Sema.zig:26482
        if (un.op == .question and !self.release_mode) {
            const null_val = try fb.emit(ir.Node.init(.const_null, TypeRegistry.UNTYPED_NULL, un.span));
            const is_non_null = try fb.emitBinary(.ne, operand, null_val, TypeRegistry.BOOL, un.span);
            const ok_block = try fb.newBlock("unwrap.ok");
            const fail_block = try fb.newBlock("unwrap.fail");
            _ = try fb.emitBranch(is_non_null, ok_block, fail_block, un.span);
            fb.setBlock(fail_block);
            _ = try fb.emitTrap(un.span);
            fb.setBlock(ok_block);
        }
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

        // Check if base is a comptime value var (from inline for over comptime arrays)
        if (self.resolveComptimeFieldAccess(fa)) |cv| return try self.emitComptimeValue(cv, fa.span);
        // Check if base evaluates to a comptime value (e.g., @typeInfo(T).fields.len)
        if (fa.base != ast.null_node) {
            if (self.chk.evalComptimeValue(fa.base)) |base_cv| {
                const field_val: ?comptime_mod.ComptimeValue = switch (base_cv) {
                    .type_info => |ti| blk: {
                        if (std.mem.eql(u8, fa.field, "name")) break :blk comptime_mod.ComptimeValue{ .string = ti.name };
                        if (std.mem.eql(u8, fa.field, "fields")) break :blk comptime_mod.ComptimeValue{ .array = .{
                            .elements = ti.fields,
                            .elem_type_name = "EnumField",
                        } };
                        break :blk null;
                    },
                    .enum_field => |ef| blk: {
                        if (std.mem.eql(u8, fa.field, "name")) break :blk comptime_mod.ComptimeValue{ .string = ef.name };
                        if (std.mem.eql(u8, fa.field, "value")) break :blk comptime_mod.ComptimeValue{ .int = ef.value };
                        break :blk null;
                    },
                    .array => |arr| blk: {
                        if (std.mem.eql(u8, fa.field, "len")) break :blk comptime_mod.ComptimeValue{ .int = @intCast(arr.elements.items.len) };
                        break :blk null;
                    },
                    else => null,
                };
                if (field_val) |fv| return try self.emitComptimeValue(fv, fa.span);
            }
        }

        // Shorthand enum variant: .variant (base == null_node, resolved by checker)
        if (fa.base == ast.null_node) {
            if (self.current_switch_enum_type != types.invalid_type) {
                const enum_info = self.type_reg.get(self.current_switch_enum_type);
                if (enum_info == .enum_type) {
                    for (enum_info.enum_type.variants) |variant| {
                        if (std.mem.eql(u8, variant.name, fa.field)) return try fb.emitConstInt(variant.value, self.current_switch_enum_type, fa.span);
                    }
                }
            }
            return ir.null_node;
        }

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

        // Tuple element access: t.0, t.1, etc.
        if (base_type == .tuple) {
            const idx = std.fmt.parseInt(u32, fa.field, 10) catch return ir.null_node;
            const tup = base_type.tuple;
            if (idx >= tup.element_types.len) return ir.null_node;
            const offset: i64 = @intCast(self.type_reg.tupleElementOffset(base_type_idx, idx));
            const elem_type = tup.element_types[idx];

            // Check if base is an ident (can use local field load)
            const base_node_ast2 = self.tree.getNode(fa.base);
            const base_expr2 = if (base_node_ast2) |n| n.asExpr() else null;
            if (base_expr2 != null and base_expr2.? == .ident) {
                if (fb.lookupLocal(base_expr2.?.ident.name)) |lidx| {
                    return try fb.emitFieldLocal(lidx, idx, offset, elem_type, fa.span);
                }
            }
            const base_val = try self.lowerExprNode(fa.base);
            return try fb.emitFieldValue(base_val, idx, offset, elem_type, fa.span);
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

        // WasmGC path: use gc_struct_get instead of offset-based loads
        if (self.target.isWasmGC()) {
            const base_val = try self.lowerExprNode(fa.base);
            return try fb.emitGcStructGet(base_val, struct_type.name, field_idx, field_type, fa.span);
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

        // Nested struct field access: o.inner.val
        // Resolve the base field chain to an address, then load from it.
        if (base_expr == .field_access) {
            const base_addr = try self.resolveStructFieldAddr(fa.base);
            if (base_addr != ir.null_node) {
                return try fb.emitFieldValue(base_addr, field_idx, field_offset, field_type, fa.span);
            }
        }

        const base_val = try self.lowerExprNode(fa.base);

        // For struct value expressions (call results, etc.), store to a temp local first
        // so field_local decomposition works correctly on native (off_ptr+load on a
        // call result value is invalid — the result is SSA values, not a memory address).
        if (!base_is_pointer and (base_type == .struct_type or base_type == .tuple)) {
            const type_size = self.type_reg.sizeOf(base_type_idx);
            const tmp_local = try fb.addLocalWithSize("__field_tmp", base_type_idx, false, @intCast(type_size));
            _ = try fb.emitStoreLocal(tmp_local, base_val, fa.span);
            return try fb.emitFieldLocal(tmp_local, field_idx, field_offset, field_type, fa.span);
        }

        return try fb.emitFieldValue(base_val, field_idx, field_offset, field_type, fa.span);
    }

    fn lowerIndex(self: *Lowerer, idx: ast.Index) Error!ir.NodeIndex {
        const fb = self.current_func orelse return ir.null_node;

        // Comptime array indexing: resolve from lowerer's comptime_value_vars map.
        // If base is a known comptime array and index is comptime-known, emit element directly.
        // If index is runtime, materialize the array and emit indexed load.
        {
            const base_node = self.tree.getNode(idx.base);
            const base_expr = if (base_node) |n| n.asExpr() else null;
            if (base_expr) |be| {
                if (be == .ident) {
                    if (self.lookupComptimeValue(be.ident.name)) |base_cv| {
                        if (base_cv == .array) {
                            // Try comptime index first (direct element emission)
                            if (self.chk.evalComptimeValue(idx.idx)) |iv| {
                                if (iv.asInt()) |i_val| {
                                    if (i_val >= 0 and i_val < @as(i64, @intCast(base_cv.array.elements.items.len))) {
                                        const elem = base_cv.array.elements.items[@intCast(i_val)];
                                        return try self.emitComptimeValue(elem, idx.span);
                                    }
                                }
                            }
                            // Dynamic index: materialize array, then emit indexed load.
                            const base_ptr = try self.emitComptimeArray(base_cv.array, idx.span);
                            const index_node = try self.lowerExprNode(idx.idx);
                            const first = base_cv.array.elements.items[0];
                            const elem_size: u32 = if (first == .string) @intCast(self.type_reg.sizeOf(TypeRegistry.STRING)) else 8;
                            const elem_type: TypeIndex = if (first == .string) TypeRegistry.STRING else TypeRegistry.I64;
                            return try fb.emitIndexValue(base_ptr, index_node, elem_size, elem_type, idx.span);
                        }
                    }
                }
            }
        }

        var base_type_idx = self.inferExprType(idx.base);
        while (self.type_reg.get(base_type_idx) == .pointer) base_type_idx = self.type_reg.get(base_type_idx).pointer.elem;
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
                    // Runtime safety: string bounds check — Zig Sema.zig:26482
                    if (!self.release_mode) {
                        const len_node = try fb.emitSliceLen(str_val, idx.span);
                        try self.emitBoundsCheck(fb, index_node, len_node, idx.span);
                    }
                    const ptr_type = self.type_reg.makePointer(TypeRegistry.U8) catch TypeRegistry.I64;
                    const ptr_val = try fb.emitSlicePtr(str_val, ptr_type, idx.span);
                    return try fb.emitIndexValue(ptr_val, index_node, elem_size, elem_type, idx.span);
                }
            }
            const base_val = try self.lowerExprNode(idx.base);
            // Runtime safety: string bounds check — Zig Sema.zig:26482
            if (!self.release_mode) {
                const len_node = try fb.emitSliceLen(base_val, idx.span);
                try self.emitBoundsCheck(fb, index_node, len_node, idx.span);
            }
            const ptr_type = self.type_reg.makePointer(TypeRegistry.U8) catch TypeRegistry.I64;
            const ptr_val = try fb.emitSlicePtr(base_val, ptr_type, idx.span);
            return try fb.emitIndexValue(ptr_val, index_node, elem_size, elem_type, idx.span);
        }

        const index_node = try self.lowerExprNode(idx.idx);

        // Runtime safety: bounds check in debug mode — Zig Sema.zig:26482
        if (!self.release_mode) {
            if (base_type == .array) {
                const len_node = try fb.emitConstInt(@intCast(base_type.array.length), TypeRegistry.I64, idx.span);
                try self.emitBoundsCheck(fb, index_node, len_node, idx.span);
            }
        }

        const base_node = self.tree.getNode(idx.base) orelse return ir.null_node;
        const base_expr = base_node.asExpr() orelse return ir.null_node;

        if (base_expr == .ident) {
            if (fb.lookupLocal(base_expr.ident.name)) |local_idx| {
                const local = fb.locals.items[local_idx];
                const local_type = self.type_reg.get(local.type_idx);
                if (local_type == .slice) {
                    const slice_val = try fb.emitLoadLocal(local_idx, local.type_idx, idx.span);
                    // Runtime safety: slice bounds check — Zig Sema.zig:26482
                    if (!self.release_mode) {
                        const len_node = try fb.emitSliceLen(slice_val, idx.span);
                        try self.emitBoundsCheck(fb, index_node, len_node, idx.span);
                    }
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

        // String slicing: s[a..b] → new string with ptr+start, len=end-start
        if (base_type_idx == TypeRegistry.STRING) {
            return try self.lowerStringSlice(se);
        }

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

    /// String slicing: s[start..end] produces a new string (ptr+start, end-start).
    /// Like Go string slicing — zero-copy view into the same backing memory.
    fn lowerStringSlice(self: *Lowerer, se: ast.SliceExpr) Error!ir.NodeIndex {
        const fb = self.current_func orelse return ir.null_node;

        // Load the base string (compound: ptr + len)
        const base_node = self.tree.getNode(se.base) orelse return ir.null_node;
        const base_expr = base_node.asExpr() orelse return ir.null_node;

        var str_val: ir.NodeIndex = ir.null_node;
        if (base_expr == .ident) {
            if (fb.lookupLocal(base_expr.ident.name)) |local_idx| {
                str_val = try fb.emitLoadLocal(local_idx, TypeRegistry.STRING, se.span);
            }
        }
        if (str_val == ir.null_node) {
            str_val = try self.lowerExprNode(se.base);
        }

        // Extract ptr and len from the string
        const ptr_type = self.type_reg.makePointer(TypeRegistry.U8) catch TypeRegistry.I64;
        const str_ptr = try fb.emitSlicePtr(str_val, ptr_type, se.span);
        const str_len = try fb.emitSliceLen(str_val, se.span);

        // Compute start (default 0) and end (default len)
        var start_val: ir.NodeIndex = undefined;
        if (se.start != null_node) {
            start_val = try self.lowerExprNode(se.start);
        } else {
            start_val = try fb.emitConstInt(0, TypeRegistry.I64, se.span);
        }

        var end_val: ir.NodeIndex = undefined;
        if (se.end != null_node) {
            end_val = try self.lowerExprNode(se.end);
        } else {
            end_val = str_len;
        }

        // Runtime safety: slice bounds check — Go OpIsSliceInBounds (index <= length)
        // Slice end can equal length (exclusive upper bound).
        if (!self.release_mode) {
            try self.emitSliceBoundsCheck(fb, start_val, str_len, se.span);
            try self.emitSliceBoundsCheck(fb, end_val, str_len, se.span);
        }

        // new_ptr = str_ptr + start
        const new_ptr = try fb.emitBinary(.add, str_ptr, start_val, TypeRegistry.I64, se.span);
        // new_len = end - start
        const new_len = try fb.emitBinary(.sub, end_val, start_val, TypeRegistry.I64, se.span);

        // Construct string compound value: store ptr and len into result local
        const result_local = try fb.addLocalWithSize("__str_slice", TypeRegistry.STRING, false, 16);
        _ = try fb.emitStoreLocalField(result_local, 0, 0, new_ptr, se.span);
        _ = try fb.emitStoreLocalField(result_local, 1, 8, new_len, se.span);
        return try fb.emitLoadLocal(result_local, TypeRegistry.STRING, se.span);
    }

    fn lowerStructInitExpr(self: *Lowerer, si: ast.StructInit, node_idx: ?NodeIndex) Error!ir.NodeIndex {
        const fb = self.current_func orelse return ir.null_node;
        const struct_type_idx: TypeIndex = if (si.type_name.len == 0) blk: {
            // Anonymous struct literal: resolve from expr_types (set by checker)
            if (node_idx) |nidx| {
                if (self.chk.expr_types.get(nidx)) |resolved| break :blk resolved;
            }
            return ir.null_node;
        } else if (si.type_args.len > 0)
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
        // Emit default values for omitted fields (Zig: Sema structInit default_field_values)
        for (struct_type.fields, 0..) |struct_field, i| {
            if (struct_field.default_value == null_node) continue;
            var provided = false;
            for (si.fields) |field_init| {
                if (std.mem.eql(u8, struct_field.name, field_init.name)) { provided = true; break; }
            }
            if (provided) continue;
            const field_idx: u32 = @intCast(i);
            const field_offset: i64 = @intCast(struct_field.offset);
            const value_node = try self.lowerExprNode(struct_field.default_value);
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
        }
        return try fb.emitLoadLocal(temp_idx, struct_type_idx, si.span);
    }

    /// Lower tuple literal: (a, b, c) → allocate stack local, store each element at computed offset
    /// Rust: tuples are anonymous structs. Go: MakeTuple SSA op. Cot: stack local with offsets.
    fn lowerTupleLiteral(self: *Lowerer, tl: ast.TupleLiteral) Error!ir.NodeIndex {
        const fb = self.current_func orelse return ir.null_node;
        // Infer the tuple type from the checker
        var elem_types = std.ArrayListUnmanaged(TypeIndex){};
        defer elem_types.deinit(self.allocator);
        for (tl.elements) |elem_idx| {
            const et = self.inferExprType(elem_idx);
            try elem_types.append(self.allocator, et);
        }
        const tuple_type_idx = self.type_reg.makeTuple(elem_types.items) catch return ir.null_node;
        const size = self.type_reg.sizeOf(tuple_type_idx);
        const temp_idx = try fb.addLocalWithSize("__tuple_tmp", tuple_type_idx, true, size);

        for (tl.elements, 0..) |elem_idx, i| {
            const value_node = try self.lowerExprNode(elem_idx);
            const offset: i64 = @intCast(self.type_reg.tupleElementOffset(tuple_type_idx, @intCast(i)));
            _ = try fb.emitStoreLocalField(temp_idx, @intCast(i), offset, value_node, tl.span);
        }
        return try fb.emitLoadLocal(temp_idx, tuple_type_idx, tl.span);
    }

    /// Lower heap allocation expression: new Type { field: value, ... }
    /// ARC path: cot_alloc call + field stores via linear memory.
    /// WasmGC path: gc_struct_new with field values (GC-managed object).
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

        // WasmGC path: emit gc_struct_new with all field values
        // Reference: Kotlin/Wasm WasmGC codegen — all structs are GC objects
        if (self.target.isWasmGC()) {
            const type_name = if (ne.type_args.len > 0) struct_type.name else ne.type_name;
            // Collect field values in struct field order
            var field_values = try self.allocator.alloc(ir.NodeIndex, struct_type.fields.len);
            // Initialize to null_node
            for (field_values) |*fv| fv.* = ir.null_node;
            for (ne.fields) |field_init| {
                for (struct_type.fields, 0..) |struct_field, i| {
                    if (std.mem.eql(u8, struct_field.name, field_init.name)) {
                        field_values[i] = try self.lowerExprNode(field_init.value);
                        break;
                    }
                }
            }
            // For any uninitialized fields, use default value or zero
            for (struct_type.fields, 0..) |struct_field, i| {
                if (field_values[i] == ir.null_node) {
                    if (struct_field.default_value != null_node) {
                        field_values[i] = try self.lowerExprNode(struct_field.default_value);
                    } else {
                        field_values[i] = try fb.emitConstInt(0, struct_field.type_idx, ne.span);
                    }
                }
            }
            return try fb.emitGcStructNew(type_name, field_values, struct_type_idx, ne.span);
        }

        // ARC path (unchanged): cot_alloc + field stores via linear memory
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
        const alloc_result = try fb.emitCall("alloc", &alloc_args, false, ptr_type, ne.span);

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
        // Emit default values for omitted fields (Zig: Sema structInit default_field_values)
        for (struct_type.fields) |struct_field| {
            if (struct_field.default_value == null_node) continue;
            var provided = false;
            for (ne.fields) |field_init| {
                if (std.mem.eql(u8, struct_field.name, field_init.name)) { provided = true; break; }
            }
            if (provided) continue;
            const field_offset: i64 = @intCast(struct_field.offset);
            const value_node = try self.lowerExprNode(struct_field.default_value);
            const field_type = self.type_reg.get(struct_field.type_idx);
            const is_string_field = struct_field.type_idx == TypeRegistry.STRING or field_type == .slice;
            if (is_string_field) {
                const u8_ptr_type = try self.type_reg.makePointer(TypeRegistry.U8);
                const ptr_ir = try fb.emitSlicePtr(value_node, u8_ptr_type, ne.span);
                const len_ir = try fb.emitSliceLen(value_node, ne.span);
                const ptr_addr = try fb.emitAddrOffset(ptr_node, field_offset, ptr_type, ne.span);
                _ = try fb.emitPtrStoreValue(ptr_addr, ptr_ir, ne.span);
                const len_addr = try fb.emitAddrOffset(ptr_node, field_offset + 8, ptr_type, ne.span);
                _ = try fb.emitPtrStoreValue(len_addr, len_ir, ne.span);
            } else {
                const field_addr = try fb.emitAddrOffset(ptr_node, field_offset, ptr_type, ne.span);
                _ = try fb.emitPtrStoreValue(field_addr, value_node, ne.span);
            }
        }

        // Constructor sugar: call init() method after allocation
        if (ne.is_constructor) {
            const type_name = if (ne.type_args.len > 0) struct_type.name else ne.type_name;
            if (self.chk.lookupMethod(type_name, "init")) |init_method| {
                var init_args = std.ArrayListUnmanaged(ir.NodeIndex){};
                defer init_args.deinit(self.allocator);
                // First arg is self (the pointer)
                const self_ptr = try fb.emitLoadLocal(temp_idx, ptr_type, ne.span);
                try init_args.append(self.allocator, self_ptr);
                // Remaining args from constructor_args (with compound decomposition)
                const func_type = self.type_reg.get(init_method.func_type);
                const init_params = if (func_type == .func) func_type.func.params else &[_]types.FuncParam{};
                for (ne.constructor_args, 0..) |arg_idx, arg_i| {
                    const arg_node = try self.lowerExprNode(arg_idx);
                    if (arg_node == ir.null_node) continue;
                    // Check if param needs compound decomposition (string/slice)
                    const param_idx = arg_i + 1; // +1 for self
                    var param_is_compound = false;
                    if (param_idx < init_params.len) {
                        const param_type = self.type_reg.get(init_params[param_idx].type_idx);
                        param_is_compound = (param_type == .slice) or (init_params[param_idx].type_idx == TypeRegistry.STRING);
                    }
                    if (param_is_compound) {
                        const p = try fb.emitSlicePtr(arg_node, TypeRegistry.I64, ne.span);
                        const l = try fb.emitSliceLen(arg_node, ne.span);
                        try init_args.append(self.allocator, p);
                        try init_args.append(self.allocator, l);
                    } else {
                        try init_args.append(self.allocator, arg_node);
                    }
                }
                _ = try fb.emitCall(init_method.func_name, init_args.items, false, TypeRegistry.VOID, ne.span);
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
        const alloc_result = try fb.emitCall("alloc", &alloc_args, false, TypeRegistry.I64, span);

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
        const alloc_result = try fb.emitCall("alloc", &alloc_args, false, TypeRegistry.I64, ce.span);

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
                .destructure_stmt => |ds| {
                    try self.detectCaptures(ds.value, parent_fb, captures);
                },
                .break_stmt, .continue_stmt, .bad_stmt => {},
            },
            .decl => {},
        }
    }

    fn lowerIfExpr(self: *Lowerer, if_expr: ast.IfExpr) Error!ir.NodeIndex {
        // Optional unwrap expression: if expr |val| { val * 2 } else { -1 }
        if (if_expr.capture.len > 0) return try self.lowerIfOptionalExpr(if_expr);
        // Comptime const-fold: if condition is known at compile time, only emit the taken branch.
        // This eliminates dead branches for @targetOs() == "linux" etc.
        if (self.chk.evalConstExpr(if_expr.condition)) |cond_val| {
            if (cond_val != 0) return self.lowerExprNode(if_expr.then_branch);
            if (if_expr.else_branch != null_node) return self.lowerExprNode(if_expr.else_branch);
            return ir.null_node;
        }
        const fb = self.current_func orelse return ir.null_node;
        const cond = try self.lowerExprNode(if_expr.condition);
        if (cond == ir.null_node) return ir.null_node;
        const then_val = try self.lowerExprNode(if_expr.then_branch);
        if (then_val == ir.null_node) return ir.null_node;
        const else_val = if (if_expr.else_branch != null_node) try self.lowerExprNode(if_expr.else_branch) else ir.null_node;
        return try fb.emitSelect(cond, then_val, else_val, self.inferExprType(if_expr.then_branch), if_expr.span);
    }

    /// Lower if-optional expression: if expr |val| { val * 2 } else { -1 }
    /// Same pattern as catch expr with capture.
    fn lowerIfOptionalExpr(self: *Lowerer, if_expr: ast.IfExpr) Error!ir.NodeIndex {
        const fb = self.current_func orelse return ir.null_node;

        // Evaluate the optional expression
        const opt_val = try self.lowerExprNode(if_expr.condition);
        if (opt_val == ir.null_node) return ir.null_node;

        // Check if non-null
        const null_val = try fb.emit(ir.Node.init(.const_null, TypeRegistry.UNTYPED_NULL, if_expr.span));
        const is_non_null = try fb.emitBinary(.ne, opt_val, null_val, TypeRegistry.BOOL, if_expr.span);

        // Determine result type
        const result_type = self.inferExprType(if_expr.then_branch);
        const result_size = self.type_reg.sizeOf(result_type);
        const result_local = try fb.addLocalWithSize("__if_opt_result", result_type, false, result_size);

        const then_block = try fb.newBlock("if.opt.then");
        const else_block = try fb.newBlock("if.opt.else");
        const merge_block = try fb.newBlock("if.opt.end");
        _ = try fb.emitBranch(is_non_null, then_block, else_block, if_expr.span);

        // Then block: unwrap and bind capture, evaluate then-branch
        fb.setBlock(then_block);
        const scope_depth = fb.markScopeEntry();
        const opt_type_idx = self.inferExprType(if_expr.condition);
        const opt_info = self.type_reg.get(opt_type_idx);
        const elem_type = if (opt_info == .optional) opt_info.optional.elem else opt_type_idx;
        const unwrapped = try fb.emitUnary(.optional_unwrap, opt_val, elem_type, if_expr.span);
        const capture_local = try fb.addLocalWithSize(if_expr.capture, elem_type, false, self.type_reg.sizeOf(elem_type));
        _ = try fb.emitStoreLocal(capture_local, unwrapped, if_expr.span);
        const then_val = try self.lowerExprNode(if_expr.then_branch);
        _ = try fb.emitStoreLocal(result_local, then_val, if_expr.span);
        fb.restoreScope(scope_depth);
        _ = try fb.emitJump(merge_block, if_expr.span);

        // Else block
        fb.setBlock(else_block);
        if (if_expr.else_branch != null_node) {
            const else_val = try self.lowerExprNode(if_expr.else_branch);
            _ = try fb.emitStoreLocal(result_local, else_val, if_expr.span);
        }
        _ = try fb.emitJump(merge_block, if_expr.span);

        // Merge: load result
        fb.setBlock(merge_block);
        return try fb.emitLoadLocal(result_local, result_type, if_expr.span);
    }

    fn lowerSwitchExpr(self: *Lowerer, se: ast.SwitchExpr) Error!ir.NodeIndex {
        const result_type = if (se.cases.len > 0) self.inferExprType(se.cases[0].body) else if (se.else_body != null_node) self.inferExprType(se.else_body) else TypeRegistry.VOID;
        // Union switches with captures always use statement form (need block scoping for captures)
        const subject_type = self.inferExprType(se.subject);
        const is_union = self.type_reg.get(subject_type) == .union_type;
        if (result_type == TypeRegistry.VOID or (is_union and self.hasCapture(se))) return try self.lowerSwitchStatement(se);
        // Switch arms with calls must use block-based lowering (branches, not selects).
        // Select-based lowering eagerly evaluates ALL arm bodies — wrong for side effects.
        // Reference: Zig, Go, Cranelift all use conditional branches for switch.
        if (self.switchArmsHaveCalls(se)) return try self.lowerSwitchValueBlocks(se, result_type);
        return try self.lowerSwitchAsSelect(se, result_type);
    }

    fn hasCapture(self: *Lowerer, se: ast.SwitchExpr) bool {
        _ = self;
        for (se.cases) |case| {
            if (case.capture.len > 0) return true;
        }
        return false;
    }

    /// Check if any switch arm body contains a function call (direct or method).
    /// Calls have side effects — select-based lowering would eagerly evaluate ALL arms.
    fn switchArmsHaveCalls(self: *Lowerer, se: ast.SwitchExpr) bool {
        for (se.cases) |case| {
            if (self.exprMayHaveSideEffects(case.body)) return true;
        }
        if (se.else_body != null_node) {
            if (self.exprMayHaveSideEffects(se.else_body)) return true;
        }
        return false;
    }

    /// Conservative check: does this expression potentially have side effects?
    /// Returns true for calls, blocks (may contain calls), and other non-pure expressions.
    fn exprMayHaveSideEffects(self: *Lowerer, idx: ast.NodeIndex) bool {
        const node = self.tree.getNode(idx) orelse return false;
        const expr = node.asExpr() orelse {
            // Statement bodies (blocks) may have side effects
            return true;
        };
        return switch (expr) {
            .call => true,
            .builtin_call => true,
            .new_expr => true,
            .block_expr => true,
            .if_expr => |ie| {
                return self.exprMayHaveSideEffects(ie.then_branch) or
                    (ie.else_branch != null_node and self.exprMayHaveSideEffects(ie.else_branch));
            },
            .switch_expr => true,
            // Pure expressions
            .literal, .ident, .field_access, .error_literal, .binary, .unary, .index, .slice_expr => false,
            // Conservative: unknown expression types assumed side-effectful
            else => true,
        };
    }

    fn lowerSwitchStatement(self: *Lowerer, se: ast.SwitchExpr) Error!ir.NodeIndex {
        const fb = self.current_func orelse return ir.null_node;
        const subject_type = self.inferExprType(se.subject);
        const subject_info = self.type_reg.get(subject_type);
        const is_union = subject_info == .union_type;
        // Zig Sema pattern: set current enum type for .variant shorthand resolution
        const old_switch_enum = self.current_switch_enum_type;
        if (subject_info == .enum_type) self.current_switch_enum_type = subject_type;
        defer self.current_switch_enum_type = old_switch_enum;

        if (is_union) return try self.lowerUnionSwitch(se, subject_type, subject_info.union_type);

        const subject = try self.lowerExprNode(se.subject);
        const merge_block = try fb.newBlock("switch.end");

        // String switch: pre-extract ptr/len for content comparison (same as lowerBinary line 3060)
        const is_string_switch = subject_type == TypeRegistry.STRING;
        var subject_ptr: ir.NodeIndex = ir.null_node;
        var subject_len: ir.NodeIndex = ir.null_node;
        if (is_string_switch) {
            const ptr_type = try self.type_reg.makePointer(TypeRegistry.U8);
            subject_ptr = try fb.emitSlicePtr(subject, ptr_type, se.span);
            subject_len = try fb.emitSliceLen(subject, se.span);
        }

        var i: usize = 0;
        while (i < se.cases.len) : (i += 1) {
            const case = se.cases[i];
            var case_cond: ir.NodeIndex = ir.null_node;

            if (case.is_range and case.patterns.len >= 2) {
                // Range pattern: subject >= start AND subject <= end
                // Zig: 1...10 uses range checks. Rust: 1..=10 same.
                const range_start = try self.lowerExprNode(case.patterns[0]);
                const range_end = try self.lowerExprNode(case.patterns[1]);
                const ge_cond = try fb.emitBinary(.ge, subject, range_start, TypeRegistry.BOOL, se.span);
                const le_cond = try fb.emitBinary(.le, subject, range_end, TypeRegistry.BOOL, se.span);
                case_cond = try fb.emitBinary(.@"and", ge_cond, le_cond, TypeRegistry.BOOL, se.span);
            } else {
                for (case.patterns) |pattern_idx| {
                    // Error literal pattern: resolve to variant index for comparison
                    const pattern_val = try self.resolveErrorPatternOrLower(pattern_idx, se.span);
                    const pattern_cond = if (is_string_switch) blk: {
                        // String comparison: decompose pattern and call cot_string_eq
                        const ptr_type = try self.type_reg.makePointer(TypeRegistry.U8);
                        const p_ptr = try fb.emitSlicePtr(pattern_val, ptr_type, se.span);
                        const p_len = try fb.emitSliceLen(pattern_val, se.span);
                        var eq_args = [_]ir.NodeIndex{ subject_ptr, subject_len, p_ptr, p_len };
                        const eq_result = try fb.emitCall("string_eq", &eq_args, false, TypeRegistry.I64, se.span);
                        const zero = try fb.emitConstInt(0, TypeRegistry.I64, se.span);
                        break :blk try fb.emitBinary(.ne, eq_result, zero, TypeRegistry.BOOL, se.span);
                    } else try fb.emitBinary(.eq, subject, pattern_val, TypeRegistry.BOOL, se.span);
                    case_cond = if (case_cond == ir.null_node) pattern_cond else try fb.emitBinary(.@"or", case_cond, pattern_cond, TypeRegistry.BOOL, se.span);
                }
            }

            // Rust: match guard — additional condition AND'd with pattern match
            if (case.guard != ast.null_node and case_cond != ir.null_node) {
                const guard_cond = try self.lowerExprNode(case.guard);
                case_cond = try fb.emitBinary(.@"and", case_cond, guard_cond, TypeRegistry.BOOL, se.span);
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

    /// Resolve an error literal pattern to its globally unique variant index, or lower normally.
    /// Uses global error table (Zig pattern: each error value has unique integer).
    fn resolveErrorPatternOrLower(self: *Lowerer, pattern_idx: ast.NodeIndex, span: Span) Error!ir.NodeIndex {
        const fb = self.current_func orelse return ir.null_node;
        const pattern_node = self.tree.getNode(pattern_idx) orelse return ir.null_node;
        const pattern_expr = pattern_node.asExpr() orelse return try self.lowerExprNode(pattern_idx);
        if (pattern_expr != .error_literal) return try self.lowerExprNode(pattern_idx);
        const el = pattern_expr.error_literal;
        const error_idx = self.getGlobalErrorIndex(el.error_name);
        return try fb.emitConstInt(error_idx, TypeRegistry.I64, span);
    }

    /// Get globally unique index for an error variant name. Assigns new index on first use.
    /// Zig reference: each error value has a unique integer, @intFromError returns it.
    fn getGlobalErrorIndex(self: *Lowerer, name: []const u8) i64 {
        if (self.global_error_table.get(name)) |idx| return idx;
        const idx = self.next_error_idx;
        self.next_error_idx += 1;
        self.global_error_table.put(name, idx) catch {};
        return idx;
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

                // Copy payload from union (after 8-byte tag) to capture local.
                // Use i64-chunk copy to handle nested structs, compound types (string),
                // and any payload size correctly.
                const num_chunks = (payload_size + 7) / 8;
                if (num_chunks <= 1) {
                    const payload_val = try fb.emitFieldLocal(subject_local, 1, 8, payload_type, se.span);
                    _ = try fb.emitStoreLocal(capture_local, payload_val, se.span);
                } else {
                    for (0..num_chunks) |ci| {
                        const union_offset: i64 = 8 + @as(i64, @intCast(ci * 8));
                        const cap_offset: i64 = @intCast(ci * 8);
                        const chunk_val = try fb.emitFieldLocal(subject_local, @intCast(1 + ci), union_offset, TypeRegistry.I64, se.span);
                        _ = try fb.emitStoreLocalField(capture_local, @intCast(ci), cap_offset, chunk_val, se.span);
                    }
                }

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

    /// Block-based switch expression lowering: each arm in its own block, result stored in local.
    /// Used when arms contain function calls or other side effects.
    /// Follows Zig/Go/Cranelift pattern: conditional branches, only one arm executes.
    fn lowerSwitchValueBlocks(self: *Lowerer, se: ast.SwitchExpr, result_type: TypeIndex) Error!ir.NodeIndex {
        const fb = self.current_func orelse return ir.null_node;
        const old_switch_enum = self.current_switch_enum_type;
        const subject_type = self.inferExprType(se.subject);
        const subject_info = self.type_reg.get(subject_type);
        if (subject_info == .enum_type) self.current_switch_enum_type = subject_type;
        defer self.current_switch_enum_type = old_switch_enum;

        const subject = try self.lowerExprNode(se.subject);
        const merge_block = try fb.newBlock("switch.end");

        // Result local: each arm stores its value here, merge block loads it
        const result_size = self.type_reg.sizeOf(result_type);
        const result_local = try fb.addLocalWithSize("__switch_result", result_type, true, result_size);

        // String switch: pre-extract ptr/len
        const is_string_switch = subject_type == TypeRegistry.STRING;
        var subject_ptr: ir.NodeIndex = ir.null_node;
        var subject_len: ir.NodeIndex = ir.null_node;
        if (is_string_switch) {
            const ptr_type = try self.type_reg.makePointer(TypeRegistry.U8);
            subject_ptr = try fb.emitSlicePtr(subject, ptr_type, se.span);
            subject_len = try fb.emitSliceLen(subject, se.span);
        }

        var i: usize = 0;
        while (i < se.cases.len) : (i += 1) {
            const case = se.cases[i];
            var case_cond: ir.NodeIndex = ir.null_node;

            if (case.is_range and case.patterns.len >= 2) {
                const range_start = try self.lowerExprNode(case.patterns[0]);
                const range_end = try self.lowerExprNode(case.patterns[1]);
                const ge_cond = try fb.emitBinary(.ge, subject, range_start, TypeRegistry.BOOL, se.span);
                const le_cond = try fb.emitBinary(.le, subject, range_end, TypeRegistry.BOOL, se.span);
                case_cond = try fb.emitBinary(.@"and", ge_cond, le_cond, TypeRegistry.BOOL, se.span);
            } else {
                for (case.patterns) |pattern_idx| {
                    const pattern_val = try self.resolveErrorPatternOrLower(pattern_idx, se.span);
                    const pattern_cond = if (is_string_switch) blk: {
                        const ptr_type = try self.type_reg.makePointer(TypeRegistry.U8);
                        const p_ptr = try fb.emitSlicePtr(pattern_val, ptr_type, se.span);
                        const p_len = try fb.emitSliceLen(pattern_val, se.span);
                        var eq_args = [_]ir.NodeIndex{ subject_ptr, subject_len, p_ptr, p_len };
                        const eq_result = try fb.emitCall("string_eq", &eq_args, false, TypeRegistry.I64, se.span);
                        const zero = try fb.emitConstInt(0, TypeRegistry.I64, se.span);
                        break :blk try fb.emitBinary(.ne, eq_result, zero, TypeRegistry.BOOL, se.span);
                    } else try fb.emitBinary(.eq, subject, pattern_val, TypeRegistry.BOOL, se.span);
                    case_cond = if (case_cond == ir.null_node) pattern_cond else try fb.emitBinary(.@"or", case_cond, pattern_cond, TypeRegistry.BOOL, se.span);
                }
            }

            if (case.guard != ast.null_node and case_cond != ir.null_node) {
                const guard_cond = try self.lowerExprNode(case.guard);
                case_cond = try fb.emitBinary(.@"and", case_cond, guard_cond, TypeRegistry.BOOL, se.span);
            }

            const case_block = try fb.newBlock("switch.case");
            const next_block = if (i + 1 < se.cases.len) try fb.newBlock("switch.next") else if (se.else_body != null_node) try fb.newBlock("switch.else") else merge_block;
            if (case_cond != ir.null_node) _ = try fb.emitBranch(case_cond, case_block, next_block, se.span);
            fb.setBlock(case_block);
            const case_val = try self.lowerExprNode(case.body);
            if (case_val != ir.null_node) _ = try fb.emitStoreLocal(result_local, case_val, se.span);
            _ = try fb.emitJump(merge_block, se.span);
            fb.setBlock(next_block);
        }

        // Else arm
        if (se.else_body != null_node) {
            const else_val = try self.lowerExprNode(se.else_body);
            if (else_val != ir.null_node) _ = try fb.emitStoreLocal(result_local, else_val, se.span);
            _ = try fb.emitJump(merge_block, se.span);
        }
        fb.setBlock(merge_block);
        return try fb.emitLoadLocal(result_local, result_type, se.span);
    }

    fn lowerSwitchAsSelect(self: *Lowerer, se: ast.SwitchExpr, result_type: TypeIndex) Error!ir.NodeIndex {
        const fb = self.current_func orelse return ir.null_node;
        // Zig Sema pattern: set current enum type for .variant shorthand resolution
        const old_switch_enum = self.current_switch_enum_type;
        const sel_subject_type = self.inferExprType(se.subject);
        if (self.type_reg.get(sel_subject_type) == .enum_type) self.current_switch_enum_type = sel_subject_type;
        defer self.current_switch_enum_type = old_switch_enum;
        const subject = try self.lowerExprNode(se.subject);

        // String switch: pre-extract ptr/len for content comparison (same as lowerBinary line 3060)
        const is_string_switch = sel_subject_type == TypeRegistry.STRING;
        var subject_ptr: ir.NodeIndex = ir.null_node;
        var subject_len: ir.NodeIndex = ir.null_node;
        if (is_string_switch) {
            const ptr_type = try self.type_reg.makePointer(TypeRegistry.U8);
            subject_ptr = try fb.emitSlicePtr(subject, ptr_type, se.span);
            subject_len = try fb.emitSliceLen(subject, se.span);
        }

        var result = if (se.else_body != null_node) try self.lowerExprNode(se.else_body) else try fb.emitConstNull(result_type, se.span);
        // noreturn else arm (e.g., else => unreachable): use a placeholder that gets overwritten
        if (result == ir.null_node) result = try fb.emitConstInt(0, result_type, se.span);
        var i: usize = se.cases.len;
        while (i > 0) {
            i -= 1;
            const case = se.cases[i];
            var case_cond: ir.NodeIndex = ir.null_node;

            if (case.is_range and case.patterns.len >= 2) {
                // Range pattern in expression form
                const range_start = try self.lowerExprNode(case.patterns[0]);
                const range_end = try self.lowerExprNode(case.patterns[1]);
                const ge_cond = try fb.emitBinary(.ge, subject, range_start, TypeRegistry.BOOL, se.span);
                const le_cond = try fb.emitBinary(.le, subject, range_end, TypeRegistry.BOOL, se.span);
                case_cond = try fb.emitBinary(.@"and", ge_cond, le_cond, TypeRegistry.BOOL, se.span);
            } else {
                for (case.patterns) |pattern_idx| {
                    const pattern_val = try self.resolveErrorPatternOrLower(pattern_idx, se.span);
                    const pattern_cond = if (is_string_switch) blk: {
                        // String comparison: decompose pattern and call cot_string_eq
                        const ptr_type = try self.type_reg.makePointer(TypeRegistry.U8);
                        const p_ptr = try fb.emitSlicePtr(pattern_val, ptr_type, se.span);
                        const p_len = try fb.emitSliceLen(pattern_val, se.span);
                        var eq_args = [_]ir.NodeIndex{ subject_ptr, subject_len, p_ptr, p_len };
                        const eq_result = try fb.emitCall("string_eq", &eq_args, false, TypeRegistry.I64, se.span);
                        const zero = try fb.emitConstInt(0, TypeRegistry.I64, se.span);
                        break :blk try fb.emitBinary(.ne, eq_result, zero, TypeRegistry.BOOL, se.span);
                    } else try fb.emitBinary(.eq, subject, pattern_val, TypeRegistry.BOOL, se.span);
                    case_cond = if (case_cond == ir.null_node) pattern_cond else try fb.emitBinary(.@"or", case_cond, pattern_cond, TypeRegistry.BOOL, se.span);
                }
            }

            // Guard in expression form
            if (case.guard != ast.null_node and case_cond != ir.null_node) {
                const guard_cond = try self.lowerExprNode(case.guard);
                case_cond = try fb.emitBinary(.@"and", case_cond, guard_cond, TypeRegistry.BOOL, se.span);
            }

            const case_val = try self.lowerExprNode(case.body);
            // noreturn arms (unreachable, @trap) return null_node — skip select,
            // control never reaches the merge point from this arm
            if (case_val == ir.null_node) continue;
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
                    .enum_type => |et| et.name,
                    .pointer => |p| blk: {
                        const elem = self.type_reg.get(p.elem);
                        if (elem == .struct_type) break :blk elem.struct_type.name;
                        if (elem == .enum_type) break :blk elem.enum_type.name;
                        break :blk null;
                    },
                    .basic => |bk| bk.name(),
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
                    const gen_info = self.chk.generics.generic_functions.get(gen_name) orelse {
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
                        .tree = gen_info.tree,
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
                // SRET for generic function calls
                if (self.needsSret(ret_type)) {
                    const ret_size = self.type_reg.sizeOf(ret_type);
                    const sret_local = try fb.addLocalWithSize("__sret_tmp", ret_type, false, ret_size);
                    const sret_addr = try fb.emitAddrLocal(sret_local, TypeRegistry.I64, call.span);
                    var sret_args = std.ArrayListUnmanaged(ir.NodeIndex){};
                    defer sret_args.deinit(self.allocator);
                    try sret_args.append(self.allocator, sret_addr);
                    try sret_args.appendSlice(self.allocator, gen_args.items);
                    _ = try fb.emitCall(inst_info.concrete_name, sret_args.items, false, TypeRegistry.VOID, call.span);
                    return try fb.emitLoadLocal(sret_local, ret_type, call.span);
                }
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

            // Determine parameter type for ABI decomposition
            var param_is_pointer = false;
            var param_is_compound = false; // slice or string — passed as (ptr, len)
            if (param_types) |params| {
                if (arg_i < params.len) {
                    const param_type = self.type_reg.get(params[arg_i].type_idx);
                    param_is_pointer = (param_type == .pointer);
                    param_is_compound = (param_type == .slice) or (params[arg_i].type_idx == TypeRegistry.STRING);
                }
            }

            // Check if string literal passed to pointer param
            if (ast_expr == .literal and ast_expr.literal.kind == .string) {
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

            // @safe auto-ref: when passing a struct value to a *Struct param
            // (safeWrapType wraps struct params to pointers), auto-ref the value.
            if (self.chk.safe_mode and param_is_pointer) {
                if (param_types) |params| {
                    if (arg_i < params.len) {
                        const param_info = self.type_reg.get(params[arg_i].type_idx);
                        if (param_info == .pointer) {
                            const arg_type = self.inferExprType(arg_idx);
                            const arg_info = self.type_reg.get(arg_type);
                            if (arg_info == .struct_type and self.type_reg.isAssignable(arg_type, param_info.pointer.elem)) {
                                // Store struct value in temp local, then take its address
                                const tmp_local = try fb.addLocalWithSize("__safe_ref", arg_type, true, self.type_reg.sizeOf(arg_type));
                                _ = try fb.emitStoreLocal(tmp_local, arg_node, call.span);
                                const ptr_type = self.type_reg.makePointer(arg_type) catch TypeRegistry.I64;
                                arg_node = try fb.emitAddrLocal(tmp_local, ptr_type, call.span);
                            }
                        }
                    }
                }
            }

            // Go pattern: decompose compound types (slice/string) into (ptr, len)
            // at call sites. The callee SSA builder reconstructs via slice_make.
            // Reference: Go's OSPTR()/OLEN() in walk/builtin.go
            if (param_is_compound) {
                const ptr_val = try fb.emitSlicePtr(arg_node, TypeRegistry.I64, call.span);
                const len_val = try fb.emitSliceLen(arg_node, call.span);
                try args.append(self.allocator, ptr_val);
                try args.append(self.allocator, len_val);
            } else {
                try args.append(self.allocator, arg_node);
            }
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

        // SRET: caller allocates temp, passes address as hidden first arg
        // Zig pattern: firstParamSRet() — caller allocates, callee writes
        if (self.needsSret(return_type)) {
            const ret_size = self.type_reg.sizeOf(return_type);
            const sret_local = try fb.addLocalWithSize("__sret_tmp", return_type, false, ret_size);
            const sret_addr = try fb.emitAddrLocal(sret_local, TypeRegistry.I64, call.span);
            // Prepend SRET address as first argument
            var sret_args = std.ArrayListUnmanaged(ir.NodeIndex){};
            defer sret_args.deinit(self.allocator);
            try sret_args.append(self.allocator, sret_addr);
            try sret_args.appendSlice(self.allocator, args.items);
            // Call returns void; result is in sret_local
            _ = try fb.emitCall(func_name, sret_args.items, false, TypeRegistry.VOID, call.span);
            return try fb.emitLoadLocal(sret_local, return_type, call.span);
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
        // Zig pattern: collect-then-process to avoid iterator invalidation.
        // lowerGenericFnInstance's re-check can trigger new instantiations that
        // modify generic_inst_by_name, so we snapshot pending names each iteration.
        var emitted = std.StringHashMap(void).init(self.allocator);
        defer emitted.deinit();
        var pending = std.ArrayListUnmanaged(checker.GenericInstInfo){};
        defer pending.deinit(self.allocator);
        var made_progress = true;
        while (made_progress) {
            made_progress = false;
            pending.clearRetainingCapacity();
            // Snapshot: collect all inst_infos that need lowering
            var it = self.chk.generics.generic_inst_by_name.valueIterator();
            while (it.next()) |inst_info| {
                if (!self.lowered_generics.contains(inst_info.concrete_name)) continue;
                if (emitted.contains(inst_info.concrete_name)) continue;
                try pending.append(self.allocator, inst_info.*);
            }
            // Process collected snapshot (safe — no iterator active)
            for (pending.items) |inst_info| {
                if (emitted.contains(inst_info.concrete_name)) continue;
                try emitted.put(inst_info.concrete_name, {});
                try self.lowerGenericFnInstance(inst_info);
                made_progress = true;
            }
        }
    }

    /// Lower one concrete generic function as a top-level function.
    fn lowerGenericFnInstance(self: *Lowerer, inst_info: checker.GenericInstInfo) !void {
        // Swap to the defining file's AST for cross-file generic resolution
        const saved_tree = self.tree;
        const saved_chk_tree = self.chk.tree;
        const saved_safe_mode = self.chk.safe_mode;
        self.tree = inst_info.tree;
        self.chk.tree = inst_info.tree;
        // Use the defining file's @safe mode, not the calling file's
        if (inst_info.tree.file) |file| self.chk.safe_mode = file.safe_mode;
        defer {
            self.tree = saved_tree;
            self.chk.tree = saved_chk_tree;
            self.chk.safe_mode = saved_safe_mode;
        }

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
        //
        // Zig pattern: each instantiation gets completely fresh analysis state.
        // Fresh Sema per analysis (Zig: Zcu/PerThread.zig:2856-2883).
        // Shared ZIR (our AST) is read-only. Analysis results are per-instance.
        const saved_expr_types = self.chk.expr_types;
        self.chk.expr_types = std.AutoHashMap(ir.NodeIndex, TypeIndex).init(self.allocator);
        defer {
            self.chk.expr_types.deinit();
            self.chk.expr_types = saved_expr_types;
        }
        self.chk.type_substitution = sub_map;
        self.chk.checkFnDeclWithName(f, inst_info.generic_node, inst_info.concrete_name) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
        };
        self.chk.type_substitution = null;

        // Lower the concrete function body with type substitution active
        self.type_substitution = sub_map;
        defer self.type_substitution = null;

        const return_type = self.resolveTypeNode(f.return_type);
        const uses_sret = self.needsSret(return_type);
        const wasm_return_type = if (uses_sret) TypeRegistry.VOID else return_type;
        self.builder.startFunc(inst_info.concrete_name, TypeRegistry.VOID, wasm_return_type, f.span);
        if (self.builder.func()) |fb| {
            if (uses_sret) fb.sret_return_type = return_type;
            fb.is_destructor = std.mem.eql(u8, f.name, "deinit");
            self.current_func = fb;
            self.cleanup_stack.clear();
            if (uses_sret) {
                _ = try fb.addParam("__sret", TypeRegistry.I64, 8);
            }
            for (f.params) |param| {
                var param_type = self.resolveTypeNode(param.type_expr);
                param_type = self.chk.safeWrapType(param_type) catch param_type;
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

        // Queue generic impl method for deferred lowering (like ensureGenericFnQueued for regular generics)
        if (self.chk.generics.generic_inst_by_name.get(method_info.func_name)) |inst_info| {
            try self.ensureGenericFnQueued(inst_info);
        }

        var args = std.ArrayListUnmanaged(ir.NodeIndex){};
        defer args.deinit(self.allocator);

        const base_type_idx = self.inferExprType(fa.base);
        const base_type = self.type_reg.get(base_type_idx);

        // Prepare receiver
        const receiver_val = blk: {
            if (method_info.receiver_is_ptr) {
                // WasmGC: struct refs are NOT memory pointers. Pass the ref directly.
                // Reference: Kotlin/Dart WasmGC — method self is a struct ref, not address.
                if (self.target.isWasmGC() and (base_type == .struct_type or
                    (base_type == .pointer and self.type_reg.get(base_type.pointer.elem) == .struct_type)))
                {
                    break :blk try self.lowerExprNode(fa.base);
                }
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

            // Determine parameter type for ABI decomposition (offset by 1 for self)
            var param_is_pointer = false;
            var param_is_compound = false; // slice or string — passed as (ptr, len)
            if (param_types) |params| {
                const param_idx = arg_i + 1; // +1 for self receiver
                if (param_idx < params.len) {
                    const param_type = self.type_reg.get(params[param_idx].type_idx);
                    param_is_pointer = (param_type == .pointer);
                    param_is_compound = (param_type == .slice) or (params[param_idx].type_idx == TypeRegistry.STRING);
                }
            }

            if (ast_expr == .literal and ast_expr.literal.kind == .string) {
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

            // @safe auto-ref: when passing a struct value to a *Struct param
            if (self.chk.safe_mode and param_is_pointer) {
                if (param_types) |params| {
                    const param_idx = arg_i + 1; // +1 for self receiver
                    if (param_idx < params.len) {
                        const param_info = self.type_reg.get(params[param_idx].type_idx);
                        if (param_info == .pointer) {
                            const arg_type = self.inferExprType(arg_idx);
                            const arg_info = self.type_reg.get(arg_type);
                            if (arg_info == .struct_type and self.type_reg.isAssignable(arg_type, param_info.pointer.elem)) {
                                const tmp_local = try fb.addLocalWithSize("__safe_ref", arg_type, true, self.type_reg.sizeOf(arg_type));
                                _ = try fb.emitStoreLocal(tmp_local, arg_node, call.span);
                                const ptr_type = self.type_reg.makePointer(arg_type) catch TypeRegistry.I64;
                                arg_node = try fb.emitAddrLocal(tmp_local, ptr_type, call.span);
                            }
                        }
                    }
                }
            }

            // Go pattern: decompose compound types (slice/string) into (ptr, len)
            // at call sites. Reference: Go's OSPTR()/OLEN() in walk/builtin.go
            if (param_is_compound) {
                const ptr_val = try fb.emitSlicePtr(arg_node, TypeRegistry.I64, call.span);
                const len_val = try fb.emitSliceLen(arg_node, call.span);
                try args.append(self.allocator, ptr_val);
                try args.append(self.allocator, len_val);
            } else {
                try args.append(self.allocator, arg_node);
            }
        }

        const func_type = self.type_reg.get(method_info.func_type);
        const return_type = if (func_type == .func) func_type.func.return_type else TypeRegistry.VOID;
        // SRET for method calls returning large types
        if (self.needsSret(return_type)) {
            const ret_size = self.type_reg.sizeOf(return_type);
            const sret_local = try fb.addLocalWithSize("__sret_tmp", return_type, false, ret_size);
            const sret_addr = try fb.emitAddrLocal(sret_local, TypeRegistry.I64, call.span);
            var sret_args = std.ArrayListUnmanaged(ir.NodeIndex){};
            defer sret_args.deinit(self.allocator);
            try sret_args.append(self.allocator, sret_addr);
            try sret_args.appendSlice(self.allocator, args.items);
            _ = try fb.emitCall(method_info.func_name, sret_args.items, false, TypeRegistry.VOID, call.span);
            return try fb.emitLoadLocal(sret_local, return_type, call.span);
        }
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
        const new_cap_grow = try fb.emitCall("nextslicecap", &cap_args, false, TypeRegistry.I64, call.span);
        // Go reference: runtime/slice.go growslice (lines 178-287)
        var grow_args = [_]ir.NodeIndex{ old_ptr, old_len, new_cap_grow, elem_size_val };
        const new_ptr_grow = try fb.emitCall("growslice", &grow_args, false, TypeRegistry.I64, call.span);
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
            _ = try fb.emitCall(if (fd == 2) "eprint_int" else "print_int", &print_args, false, TypeRegistry.VOID, call.span);
        } else {
            const str_val = try self.lowerExprNode(call.args[0]);
            const ptr_val = try fb.emitSlicePtr(str_val, ptr_type, call.span);
            const len_val = try fb.emitSliceLen(str_val, call.span);
            const fd_val = try fb.emitConstInt(fd, TypeRegistry.I64, call.span);
            var write_args = [_]ir.NodeIndex{ fd_val, ptr_val, len_val };
            _ = try fb.emitCall("write", &write_args, false, TypeRegistry.I64, call.span);
        }

        if (is_println) {
            const nl_idx = try fb.addStringLiteral("\n");
            const nl_str = try fb.emit(ir.Node.init(.{ .const_slice = .{ .string_index = nl_idx } }, TypeRegistry.STRING, call.span));
            const nl_ptr = try fb.emitSlicePtr(nl_str, ptr_type, call.span);
            const nl_len = try fb.emitConstInt(1, TypeRegistry.I64, call.span);
            const fd_val = try fb.emitConstInt(fd, TypeRegistry.I64, call.span);
            var nl_args = [_]ir.NodeIndex{ fd_val, nl_ptr, nl_len };
            _ = try fb.emitCall("write", &nl_args, false, TypeRegistry.I64, call.span);
        }
        return ir.null_node;
    }

    fn lowerBuiltinCall(self: *Lowerer, bc: ast.BuiltinCall) Error!ir.NodeIndex {
        const fb = self.current_func orelse return ir.null_node;
        switch (bc.kind) {
            .size_of => {
                const type_idx = self.resolveTypeNode(bc.type_arg);
                return try fb.emitConstInt(@intCast(self.type_reg.sizeOf(type_idx)), TypeRegistry.I64, bc.span);
            },
            .enum_len => {
                // Ref: Zig @typeInfo(.Enum).fields.len — comptime variant count
                const type_idx = self.resolveTypeNode(bc.type_arg);
                const info = self.type_reg.get(type_idx);
                const count: i64 = if (info == .enum_type) @intCast(info.enum_type.variants.len) else 0;
                return try fb.emitConstInt(count, TypeRegistry.I64, bc.span);
            },
            .align_of => {
                const type_idx = self.resolveTypeNode(bc.type_arg);
                return try fb.emitConstInt(@intCast(self.type_reg.alignOf(type_idx)), TypeRegistry.I64, bc.span);
            },
            .int_cast => {
                const target_type = self.resolveTypeNode(bc.type_arg);
                const value = try self.lowerExprNode(bc.args[0]);
                return try fb.emitIntCast(value, target_type, bc.span);
            },
            .float_cast => {
                // Ref: Zig @floatCast — float-to-float conversion (f64→f32 demote, f32→f64 promote)
                const target_type = self.resolveTypeNode(bc.type_arg);
                const value = try self.lowerExprNode(bc.args[0]);
                return try fb.emitIntCast(value, target_type, bc.span);
            },
            .float_from_int => {
                const target_type = self.resolveTypeNode(bc.type_arg);
                const value = try self.lowerExprNode(bc.args[0]);
                return try fb.emitIntCast(value, target_type, bc.span);
            },
            .int_from_float => {
                // @intFromFloat(val) — convert float to i64 (truncate toward zero)
                // Wasm: i64.trunc_f64_s (0xB0)
                const value = try self.lowerExprNode(bc.args[0]);
                return try fb.emitIntCast(value, TypeRegistry.I64, bc.span);
            },
            .ptr_cast => {
                const target_type = self.resolveTypeNode(bc.type_arg);
                const value = try self.lowerExprNode(bc.args[0]);
                return try fb.emitPtrCast(value, target_type, bc.span);
            },
            .int_to_ptr => {
                const target_type = self.resolveTypeNode(bc.type_arg);
                const value = try self.lowerExprNode(bc.args[0]);
                return try fb.emitIntToPtr(value, target_type, bc.span);
            },
            .ptr_to_int => {
                const value = try self.lowerExprNode(bc.args[0]);
                return try fb.emitPtrToInt(value, TypeRegistry.I64, bc.span);
            },
            .string => {
                const ptr_val = try self.lowerExprNode(bc.args[0]);
                const len_val = try self.lowerExprNode(bc.args[1]);
                return try fb.emit(ir.Node.init(.{ .string_header = .{ .ptr = ptr_val, .len = len_val } }, TypeRegistry.STRING, bc.span));
            },
            .assert => {
                const cond = try self.lowerExprNode(bc.args[0]);
                const then_block = try fb.newBlock("assert.ok");
                const fail_block = try fb.newBlock("assert.fail");
                _ = try fb.emitBranch(cond, then_block, fail_block, bc.span);
                fb.setBlock(fail_block);
                if (self.current_test_name != null) {
                    const ret_type = fb.return_type;
                    const eu_size = self.type_reg.sizeOf(ret_type);
                    const tmp_local = try fb.addLocalWithSize("__assert_err", ret_type, false, eu_size);
                    const tag_one = try fb.emitConstInt(1, TypeRegistry.I64, bc.span);
                    _ = try fb.emitStoreLocalField(tmp_local, 0, 0, tag_one, bc.span);
                    const payload = try fb.emitConstInt(0, TypeRegistry.I64, bc.span);
                    _ = try fb.emitStoreLocalField(tmp_local, 1, 8, payload, bc.span);
                    const err_ret = try fb.emitAddrLocal(tmp_local, TypeRegistry.I64, bc.span);
                    _ = try fb.emitRet(err_ret, bc.span);
                } else {
                    const msg = try self.allocator.dupe(u8, "assertion failed\n");
                    const msg_idx = try fb.addStringLiteral(msg);
                    const msg_str = try fb.emitConstSlice(msg_idx, bc.span);
                    const ptr_type = try self.type_reg.makePointer(TypeRegistry.U8);
                    const msg_ptr = try fb.emitSlicePtr(msg_str, ptr_type, bc.span);
                    const msg_len = try fb.emitSliceLen(msg_str, bc.span);
                    const fd_val = try fb.emitConstInt(2, TypeRegistry.I64, bc.span);
                    var write_args = [_]ir.NodeIndex{ fd_val, msg_ptr, msg_len };
                    _ = try fb.emitCall("write", &write_args, false, TypeRegistry.I64, bc.span);
                    _ = try fb.emitTrap(bc.span);
                }
                fb.setBlock(then_block);
                return ir.null_node;
            },
            .assert_eq => {
                const left = try self.lowerExprNode(bc.args[0]);
                const right = try self.lowerExprNode(bc.args[1]);
                const left_type = self.inferExprType(bc.args[0]);
                const is_string = left_type == TypeRegistry.STRING;

                // Store values to locals BEFORE branch (br_table can't carry values across blocks)
                var left_local: ir.LocalIdx = undefined;
                var right_local: ir.LocalIdx = undefined;
                var l_ptr_local: ir.LocalIdx = undefined;
                var l_len_local: ir.LocalIdx = undefined;
                var r_ptr_local: ir.LocalIdx = undefined;
                var r_len_local: ir.LocalIdx = undefined;

                if (self.current_test_name != null) {
                    if (is_string) {
                        const ptr_type = try self.type_reg.makePointer(TypeRegistry.U8);
                        const l_p = try fb.emitSlicePtr(left, ptr_type, bc.span);
                        const l_l = try fb.emitSliceLen(left, bc.span);
                        const r_p = try fb.emitSlicePtr(right, ptr_type, bc.span);
                        const r_l = try fb.emitSliceLen(right, bc.span);
                        l_ptr_local = try fb.addLocalWithSize("__fail_lptr", TypeRegistry.I64, false, 8);
                        _ = try fb.emitStoreLocal(l_ptr_local, l_p, bc.span);
                        l_len_local = try fb.addLocalWithSize("__fail_llen", TypeRegistry.I64, false, 8);
                        _ = try fb.emitStoreLocal(l_len_local, l_l, bc.span);
                        r_ptr_local = try fb.addLocalWithSize("__fail_rptr", TypeRegistry.I64, false, 8);
                        _ = try fb.emitStoreLocal(r_ptr_local, r_p, bc.span);
                        r_len_local = try fb.addLocalWithSize("__fail_rlen", TypeRegistry.I64, false, 8);
                        _ = try fb.emitStoreLocal(r_len_local, r_l, bc.span);
                    } else {
                        left_local = try fb.addLocalWithSize("__fail_left", TypeRegistry.I64, false, 8);
                        _ = try fb.emitStoreLocal(left_local, left, bc.span);
                        right_local = try fb.addLocalWithSize("__fail_right", TypeRegistry.I64, false, 8);
                        _ = try fb.emitStoreLocal(right_local, right, bc.span);
                    }
                }

                const cond = if (is_string) blk: {
                    const ptr_type = try self.type_reg.makePointer(TypeRegistry.U8);
                    const l_ptr = try fb.emitSlicePtr(left, ptr_type, bc.span);
                    const l_len = try fb.emitSliceLen(left, bc.span);
                    const r_ptr = try fb.emitSlicePtr(right, ptr_type, bc.span);
                    const r_len = try fb.emitSliceLen(right, bc.span);
                    var eq_args = [_]ir.NodeIndex{ l_ptr, l_len, r_ptr, r_len };
                    const eq_result = try fb.emitCall("string_eq", &eq_args, false, TypeRegistry.I64, bc.span);
                    break :blk try fb.emitBinary(.ne, eq_result, try fb.emitConstInt(0, TypeRegistry.I64, bc.span), TypeRegistry.BOOL, bc.span);
                } else try fb.emitBinary(.eq, left, right, TypeRegistry.BOOL, bc.span);
                const then_block = try fb.newBlock("assertEq.ok");
                const fail_block = try fb.newBlock("assertEq.fail");
                _ = try fb.emitBranch(cond, then_block, fail_block, bc.span);
                fb.setBlock(fail_block);
                if (self.current_test_name != null) {
                    // Reload values from locals and store in globals (Deno pattern: expected vs received)
                    const is_str_val = try fb.emitConstInt(if (is_string) @as(i64, 1) else 0, TypeRegistry.I64, bc.span);
                    if (is_string) {
                        const lp = try fb.emitLoadLocal(l_ptr_local, TypeRegistry.I64, bc.span);
                        const rp = try fb.emitLoadLocal(r_ptr_local, TypeRegistry.I64, bc.span);
                        const ll = try fb.emitLoadLocal(l_len_local, TypeRegistry.I64, bc.span);
                        const rl = try fb.emitLoadLocal(r_len_local, TypeRegistry.I64, bc.span);
                        var store_args = [_]ir.NodeIndex{ lp, rp, is_str_val, ll, rl };
                        _ = try fb.emitCall("__test_store_fail_values", &store_args, false, TypeRegistry.VOID, bc.span);
                    } else {
                        const lv = try fb.emitLoadLocal(left_local, TypeRegistry.I64, bc.span);
                        const rv = try fb.emitLoadLocal(right_local, TypeRegistry.I64, bc.span);
                        const zero_len = try fb.emitConstInt(0, TypeRegistry.I64, bc.span);
                        var store_args = [_]ir.NodeIndex{ lv, rv, is_str_val, zero_len, zero_len };
                        _ = try fb.emitCall("__test_store_fail_values", &store_args, false, TypeRegistry.VOID, bc.span);
                    }
                    const ret_type = fb.return_type;
                    const eu_size = self.type_reg.sizeOf(ret_type);
                    const tmp_local = try fb.addLocalWithSize("__assertEq_err", ret_type, false, eu_size);
                    const tag_one = try fb.emitConstInt(1, TypeRegistry.I64, bc.span);
                    _ = try fb.emitStoreLocalField(tmp_local, 0, 0, tag_one, bc.span);
                    const payload = try fb.emitConstInt(0, TypeRegistry.I64, bc.span);
                    _ = try fb.emitStoreLocalField(tmp_local, 1, 8, payload, bc.span);
                    const err_ret = try fb.emitAddrLocal(tmp_local, TypeRegistry.I64, bc.span);
                    _ = try fb.emitRet(err_ret, bc.span);
                } else {
                    const msg = try self.allocator.dupe(u8, "assertEq failed\n");
                    const msg_idx = try fb.addStringLiteral(msg);
                    const msg_str = try fb.emitConstSlice(msg_idx, bc.span);
                    const ptr_type = try self.type_reg.makePointer(TypeRegistry.U8);
                    const msg_ptr = try fb.emitSlicePtr(msg_str, ptr_type, bc.span);
                    const msg_len = try fb.emitSliceLen(msg_str, bc.span);
                    const fd_val = try fb.emitConstInt(2, TypeRegistry.I64, bc.span);
                    var write_args = [_]ir.NodeIndex{ fd_val, msg_ptr, msg_len };
                    _ = try fb.emitCall("write", &write_args, false, TypeRegistry.I64, bc.span);
                    _ = try fb.emitTrap(bc.span);
                }
                fb.setBlock(then_block);
                return ir.null_node;
            },
            .ptr_of => {
                const str_val = try self.lowerExprNode(bc.args[0]);
                const ptr_type = try self.type_reg.makePointer(TypeRegistry.U8);
                const ptr_val = try fb.emitSlicePtr(str_val, ptr_type, bc.span);
                return try fb.emitPtrToInt(ptr_val, TypeRegistry.I64, bc.span);
            },
            .len_of => {
                const str_val = try self.lowerExprNode(bc.args[0]);
                return try fb.emitSliceLen(str_val, bc.span);
            },
            .trap => {
                _ = try fb.emitTrap(bc.span);
                const dead_block = try fb.newBlock("trap.dead");
                fb.setBlock(dead_block);
                return ir.null_node;
            },
            .compile_error => {
                return try fb.emitTrap(bc.span);
            },
            .embed_file => {
                // Extract path from string literal argument
                const arg_node = self.tree.getNode(bc.args[0]) orelse return ir.null_node;
                const arg_expr = arg_node.asExpr() orelse return ir.null_node;
                if (arg_expr != .literal or arg_expr.literal.kind != .string) return ir.null_node;
                const raw_path = arg_expr.literal.value;
                // Strip quotes
                const rel_path = if (raw_path.len >= 2 and raw_path[0] == '"' and raw_path[raw_path.len - 1] == '"')
                    raw_path[1 .. raw_path.len - 1]
                else
                    raw_path;

                // Resolve relative to source file directory
                const source_dir = if (self.tree.file) |f|
                    std.fs.path.dirname(f.filename) orelse "."
                else
                    ".";
                const full_path = try std.fs.path.join(self.allocator, &.{ source_dir, rel_path });

                // Read file at compile time
                const file_content = std.fs.cwd().readFileAlloc(self.allocator, full_path, 10 * 1024 * 1024) catch {
                    self.err.errorWithCode(bc.span.start, .e300, "cannot read embedded file");
                    return ir.null_node;
                };
                const str_idx = try fb.addStringLiteral(file_content);
                return try fb.emitConstSlice(str_idx, bc.span);
            },
            .target_os => {
                const str = try self.allocator.dupe(u8, self.target.os.name());
                const str_idx = try fb.addStringLiteral(str);
                return try fb.emitConstSlice(str_idx, bc.span);
            },
            .target_arch => {
                const str = try self.allocator.dupe(u8, self.target.arch.name());
                const str_idx = try fb.addStringLiteral(str);
                return try fb.emitConstSlice(str_idx, bc.span);
            },
            .target => {
                const str = try self.allocator.dupe(u8, self.target.name());
                const str_idx = try fb.addStringLiteral(str);
                return try fb.emitConstSlice(str_idx, bc.span);
            },
            .abs => {
                const arg = try self.lowerExprNode(bc.args[0]);
                return fb.emit(ir.Node.init(.{ .unary = .{ .op = .abs, .operand = arg } }, TypeRegistry.F64, bc.span));
            },
            .ceil => {
                const arg = try self.lowerExprNode(bc.args[0]);
                return fb.emit(ir.Node.init(.{ .unary = .{ .op = .ceil, .operand = arg } }, TypeRegistry.F64, bc.span));
            },
            .floor => {
                const arg = try self.lowerExprNode(bc.args[0]);
                return fb.emit(ir.Node.init(.{ .unary = .{ .op = .floor, .operand = arg } }, TypeRegistry.F64, bc.span));
            },
            .trunc => {
                const arg = try self.lowerExprNode(bc.args[0]);
                return fb.emit(ir.Node.init(.{ .unary = .{ .op = .trunc_float, .operand = arg } }, TypeRegistry.F64, bc.span));
            },
            .round => {
                const arg = try self.lowerExprNode(bc.args[0]);
                return fb.emit(ir.Node.init(.{ .unary = .{ .op = .nearest, .operand = arg } }, TypeRegistry.F64, bc.span));
            },
            .sqrt => {
                const arg = try self.lowerExprNode(bc.args[0]);
                return fb.emit(ir.Node.init(.{ .unary = .{ .op = .sqrt, .operand = arg } }, TypeRegistry.F64, bc.span));
            },
            .fmin => {
                const a = try self.lowerExprNode(bc.args[0]);
                const b = try self.lowerExprNode(bc.args[1]);
                return fb.emit(ir.Node.init(.{ .binary = .{ .op = .fmin, .left = a, .right = b } }, TypeRegistry.F64, bc.span));
            },
            .fmax => {
                const a = try self.lowerExprNode(bc.args[0]);
                const b = try self.lowerExprNode(bc.args[1]);
                return fb.emit(ir.Node.init(.{ .binary = .{ .op = .fmax, .left = a, .right = b } }, TypeRegistry.F64, bc.span));
            },
            .has_field => {
                // @hasField(T, "name") — comptime bool, resolved at compile time
                const type_idx = self.resolveTypeNode(bc.type_arg);
                const name_str = self.getStringLiteral(bc.args[0]) orelse return fb.emitConstInt(0, TypeRegistry.BOOL, bc.span);
                const info = self.type_reg.get(type_idx);
                if (info == .struct_type) {
                    for (info.struct_type.fields) |sf| {
                        if (std.mem.eql(u8, sf.name, name_str)) return fb.emitConstInt(1, TypeRegistry.BOOL, bc.span);
                    }
                    return fb.emitConstInt(0, TypeRegistry.BOOL, bc.span);
                }
                if (info == .enum_type) {
                    for (info.enum_type.variants) |v| {
                        if (std.mem.eql(u8, v.name, name_str)) return fb.emitConstInt(1, TypeRegistry.BOOL, bc.span);
                    }
                    return fb.emitConstInt(0, TypeRegistry.BOOL, bc.span);
                }
                if (info == .union_type) {
                    for (info.union_type.variants) |v| {
                        if (std.mem.eql(u8, v.name, name_str)) return fb.emitConstInt(1, TypeRegistry.BOOL, bc.span);
                    }
                    return fb.emitConstInt(0, TypeRegistry.BOOL, bc.span);
                }
                return fb.emitConstInt(0, TypeRegistry.BOOL, bc.span);
            },
            .type_of => {
                // @TypeOf(expr) — no runtime effect, type resolved at checker level
                return ir.null_node;
            },
            .field => {
                // @field(value, "name") — comptime field access, delegate to lowerFieldAccess
                const name_str = self.getStringLiteral(bc.args[1]) orelse return ir.null_node;
                return self.lowerFieldAccess(.{ .base = bc.args[0], .field = name_str, .span = bc.span });
            },
            // @intFromEnum(e) — Zig Sema.zig:8420: enum value IS its backing integer
            .int_from_enum => {
                // Enums are already integers at runtime — identity operation
                return try self.lowerExprNode(bc.args[0]);
            },
            // @enumFromInt(T, i) — Zig Sema.zig:8480: integer → enum, identity at runtime
            .enum_from_int => {
                // Enums are integers at runtime — identity operation (type changes in checker)
                return try self.lowerExprNode(bc.args[0]);
            },
            // @tagName(val) — Zig Sema.zig:20487: build string table, index by enum value
            .tag_name => {
                const arg = try self.lowerExprNode(bc.args[0]);
                const arg_type = self.inferExprType(bc.args[0]);
                const info = self.type_reg.get(arg_type);
                if (info == .enum_type) {
                    // Build if-chain: if val == 0 return "variant0" elif val == 1 return "variant1" ...
                    // Zig uses string table + index; we use if-chain (simpler, same semantics)
                    const variants = info.enum_type.variants;
                    if (variants.len == 0) {
                        const empty = try fb.addStringLiteral(try self.allocator.dupe(u8, ""));
                        return fb.emitConstSlice(empty, bc.span);
                    }
                    // Store arg to local (br_table can't carry values across blocks)
                    const arg_local = try fb.addLocalWithSize("__tag_val", TypeRegistry.I64, false, 8);
                    _ = try fb.emitStoreLocal(arg_local, arg, bc.span);
                    // Result local for string (ptr + len = 16 bytes)
                    const result_local = try fb.addLocalWithSize("__tag_result", TypeRegistry.STRING, false, 16);
                    // Build if-chain for each variant
                    const merge_block = try fb.newBlock("tagname.merge");
                    for (variants) |v| {
                        const val_node = try fb.emitLoadLocal(arg_local, TypeRegistry.I64, bc.span);
                        const variant_val = try fb.emitConstInt(v.value, TypeRegistry.I64, bc.span);
                        const cmp = try fb.emitBinary(.eq, val_node, variant_val, TypeRegistry.BOOL, bc.span);
                        const match_block = try fb.newBlock("tagname.match");
                        const next_block = try fb.newBlock("tagname.next");
                        _ = try fb.emitBranch(cmp, match_block, next_block, bc.span);
                        fb.setBlock(match_block);
                        const str = try fb.addStringLiteral(try self.allocator.dupe(u8, v.name));
                        const str_val = try fb.emitConstSlice(str, bc.span);
                        _ = try fb.emitStoreLocalField(result_local, 0, 0, try fb.emitSlicePtr(str_val, try self.type_reg.makePointer(TypeRegistry.U8), bc.span), bc.span);
                        _ = try fb.emitStoreLocalField(result_local, 1, 8, try fb.emitSliceLen(str_val, bc.span), bc.span);
                        _ = try fb.emitJump(merge_block, bc.span);
                        fb.setBlock(next_block);
                    }
                    // Default: empty string for unknown values
                    const empty = try fb.addStringLiteral(try self.allocator.dupe(u8, ""));
                    const empty_val = try fb.emitConstSlice(empty, bc.span);
                    _ = try fb.emitStoreLocalField(result_local, 0, 0, try fb.emitSlicePtr(empty_val, try self.type_reg.makePointer(TypeRegistry.U8), bc.span), bc.span);
                    _ = try fb.emitStoreLocalField(result_local, 1, 8, try fb.emitSliceLen(empty_val, bc.span), bc.span);
                    _ = try fb.emitJump(merge_block, bc.span);
                    fb.setBlock(merge_block);
                    return fb.emitLoadLocal(result_local, TypeRegistry.STRING, bc.span);
                }
                if (info == .union_type) {
                    // Tagged union: extract tag (field 0), then same if-chain pattern
                    const tag_val = try self.lowerFieldAccess(.{ .base = bc.args[0], .field = "__tag", .span = bc.span });
                    const tag_local = try fb.addLocalWithSize("__utag_val", TypeRegistry.I64, false, 8);
                    _ = try fb.emitStoreLocal(tag_local, tag_val, bc.span);
                    const result_local = try fb.addLocalWithSize("__utag_result", TypeRegistry.STRING, false, 16);
                    const merge_block = try fb.newBlock("utagname.merge");
                    for (info.union_type.variants, 0..) |v, i| {
                        const val_node = try fb.emitLoadLocal(tag_local, TypeRegistry.I64, bc.span);
                        const variant_val = try fb.emitConstInt(@intCast(i), TypeRegistry.I64, bc.span);
                        const cmp = try fb.emitBinary(.eq, val_node, variant_val, TypeRegistry.BOOL, bc.span);
                        const match_block = try fb.newBlock("utagname.match");
                        const next_block = try fb.newBlock("utagname.next");
                        _ = try fb.emitBranch(cmp, match_block, next_block, bc.span);
                        fb.setBlock(match_block);
                        const str = try fb.addStringLiteral(try self.allocator.dupe(u8, v.name));
                        const str_val = try fb.emitConstSlice(str, bc.span);
                        _ = try fb.emitStoreLocalField(result_local, 0, 0, try fb.emitSlicePtr(str_val, try self.type_reg.makePointer(TypeRegistry.U8), bc.span), bc.span);
                        _ = try fb.emitStoreLocalField(result_local, 1, 8, try fb.emitSliceLen(str_val, bc.span), bc.span);
                        _ = try fb.emitJump(merge_block, bc.span);
                        fb.setBlock(next_block);
                    }
                    const empty = try fb.addStringLiteral(try self.allocator.dupe(u8, ""));
                    const empty_val = try fb.emitConstSlice(empty, bc.span);
                    _ = try fb.emitStoreLocalField(result_local, 0, 0, try fb.emitSlicePtr(empty_val, try self.type_reg.makePointer(TypeRegistry.U8), bc.span), bc.span);
                    _ = try fb.emitStoreLocalField(result_local, 1, 8, try fb.emitSliceLen(empty_val, bc.span), bc.span);
                    _ = try fb.emitJump(merge_block, bc.span);
                    fb.setBlock(merge_block);
                    return fb.emitLoadLocal(result_local, TypeRegistry.STRING, bc.span);
                }
                // Fallback: empty string
                const empty = try fb.addStringLiteral(try self.allocator.dupe(u8, ""));
                return fb.emitConstSlice(empty, bc.span);
            },
            // @errorName(err) — Zig Sema.zig:20375: error value → name string
            // Build if-chain like @tagName: compare value against each variant index
            .error_name => {
                const arg = try self.lowerExprNode(bc.args[0]);
                const arg_type = self.inferExprType(bc.args[0]);
                var error_set_type = arg_type;
                const info = self.type_reg.get(arg_type);
                // For error unions, extract the error set type
                if (info == .error_union) {
                    error_set_type = info.error_union.error_set;
                }
                const es_info = self.type_reg.get(error_set_type);
                if (es_info == .error_set and es_info.error_set.variants.len > 0) {
                    const variants = es_info.error_set.variants;
                    // Store arg to local (br_table can't carry values across blocks)
                    const arg_local = try fb.addLocalWithSize("__err_val", TypeRegistry.I64, false, 8);
                    _ = try fb.emitStoreLocal(arg_local, arg, bc.span);
                    const result_local = try fb.addLocalWithSize("__err_result", TypeRegistry.STRING, false, 16);
                    const merge_block = try fb.newBlock("errname.merge");
                    for (variants, 0..) |name, i| {
                        const val_node = try fb.emitLoadLocal(arg_local, TypeRegistry.I64, bc.span);
                        const variant_val = try fb.emitConstInt(@intCast(i), TypeRegistry.I64, bc.span);
                        const cmp = try fb.emitBinary(.eq, val_node, variant_val, TypeRegistry.BOOL, bc.span);
                        const match_block = try fb.newBlock("errname.match");
                        const next_block = try fb.newBlock("errname.next");
                        _ = try fb.emitBranch(cmp, match_block, next_block, bc.span);
                        fb.setBlock(match_block);
                        const str = try fb.addStringLiteral(try self.allocator.dupe(u8, name));
                        const str_val = try fb.emitConstSlice(str, bc.span);
                        _ = try fb.emitStoreLocalField(result_local, 0, 0, try fb.emitSlicePtr(str_val, try self.type_reg.makePointer(TypeRegistry.U8), bc.span), bc.span);
                        _ = try fb.emitStoreLocalField(result_local, 1, 8, try fb.emitSliceLen(str_val, bc.span), bc.span);
                        _ = try fb.emitJump(merge_block, bc.span);
                        fb.setBlock(next_block);
                    }
                    // Default: "error" for unknown values
                    const fallback = try fb.addStringLiteral(try self.allocator.dupe(u8, "error"));
                    const fallback_val = try fb.emitConstSlice(fallback, bc.span);
                    _ = try fb.emitStoreLocalField(result_local, 0, 0, try fb.emitSlicePtr(fallback_val, try self.type_reg.makePointer(TypeRegistry.U8), bc.span), bc.span);
                    _ = try fb.emitStoreLocalField(result_local, 1, 8, try fb.emitSliceLen(fallback_val, bc.span), bc.span);
                    _ = try fb.emitJump(merge_block, bc.span);
                    fb.setBlock(merge_block);
                    return fb.emitLoadLocal(result_local, TypeRegistry.STRING, bc.span);
                }
                // Fallback for unresolved error sets
                const fallback = try fb.addStringLiteral(try self.allocator.dupe(u8, "error"));
                return fb.emitConstSlice(fallback, bc.span);
            },
            // @intFromBool(b) — Zig Sema.zig:20341: bool → integer, identity at Wasm level
            .int_from_bool => {
                // Bools are already 0/1 integers at Wasm level — identity operation
                return try self.lowerExprNode(bc.args[0]);
            },
            // @bitCast(T, val) — Zig Sema.zig:30554: reinterpret bits, no conversion
            .bit_cast => {
                const target_type = self.resolveTypeNode(bc.type_arg);
                const value = try self.lowerExprNode(bc.args[0]);
                // Check if this is an f64 <-> i64 reinterpret (Wasm has specific ops)
                const target_info = self.type_reg.get(target_type);
                const source_type = self.inferExprType(bc.args[0]);
                const source_info = self.type_reg.get(source_type);
                if ((target_info == .basic and target_info.basic == .f64_type) and
                    (source_info == .basic and (source_info.basic == .i64_type or source_info.basic == .u64_type)))
                {
                    // i64 → f64: Wasm f64.reinterpret_i64 (0xBF)
                    return fb.emit(ir.Node.init(.{ .unary = .{ .op = .f64_reinterpret_i64, .operand = value } }, TypeRegistry.F64, bc.span));
                }
                if ((target_info == .basic and (target_info.basic == .i64_type or target_info.basic == .u64_type)) and
                    (source_info == .basic and source_info.basic == .f64_type))
                {
                    // f64 → i64: Wasm i64.reinterpret_f64 (0xBD)
                    return fb.emit(ir.Node.init(.{ .unary = .{ .op = .i64_reinterpret_f64, .operand = value } }, TypeRegistry.I64, bc.span));
                }
                // For same-size integer types, identity operation
                return value;
            },
            // @truncate(T, val) — Zig Sema.zig:22882: narrow to smaller integer via masking
            .truncate => {
                const target_type = self.resolveTypeNode(bc.type_arg);
                const value = try self.lowerExprNode(bc.args[0]);
                // Emit AND with appropriate mask for target bit width
                const target_info = self.type_reg.get(target_type);
                const mask: i64 = if (target_info == .basic) switch (target_info.basic) {
                    .u8_type => 0xFF,
                    .u16_type => 0xFFFF,
                    .u32_type, .i32_type => 0xFFFFFFFF,
                    else => return value, // i64/u64 — no truncation needed
                } else return value;
                const mask_node = try fb.emitConstInt(mask, TypeRegistry.I64, bc.span);
                return try fb.emitBinary(.bit_and, value, mask_node, TypeRegistry.I64, bc.span);
            },
            // @as(T, val) — Zig Sema.zig:9659: explicit type coercion, delegates to intCast
            .as => {
                // @as is a pure type annotation — identity at Wasm level (all values are i64)
                // Ref: Zig @as is type coercion with no codegen
                _ = self.resolveTypeNode(bc.type_arg);
                return try self.lowerExprNode(bc.args[0]);
            },
            // @offsetOf(T, "field") — Zig Sema.zig:23060: always comptime, byte offset
            .offset_of => {
                const type_idx = self.resolveTypeNode(bc.type_arg);
                const name_str = self.getStringLiteral(bc.args[0]) orelse return fb.emitConstInt(0, TypeRegistry.I64, bc.span);
                const info = self.type_reg.get(type_idx);
                if (info == .struct_type) {
                    var offset: u32 = 0;
                    for (info.struct_type.fields) |sf| {
                        if (std.mem.eql(u8, sf.name, name_str)) {
                            return fb.emitConstInt(@intCast(offset), TypeRegistry.I64, bc.span);
                        }
                        offset += self.type_reg.sizeOf(sf.type_idx);
                    }
                }
                return fb.emitConstInt(0, TypeRegistry.I64, bc.span);
            },
            // @min(a, b) — Zig Sema.zig:24678: if a < b then a else b
            // Uses unsigned comparison for unsigned integer types (Ref: Zig peerType)
            .min => {
                const a = try self.lowerExprNode(bc.args[0]);
                const b = try self.lowerExprNode(bc.args[1]);
                // Determine comparison op based on arg types
                const a_type = self.inferExprType(bc.args[0]);
                const a_info = self.type_reg.get(a_type);
                const use_unsigned = a_info == .basic and a_info.basic.isUnsigned();
                const cmp_op: ir.BinaryOp = if (use_unsigned) .lt_u else .lt;
                // Store to locals (br_table can't carry values)
                const a_local = try fb.addLocalWithSize("__min_a", TypeRegistry.I64, false, 8);
                _ = try fb.emitStoreLocal(a_local, a, bc.span);
                const b_local = try fb.addLocalWithSize("__min_b", TypeRegistry.I64, false, 8);
                _ = try fb.emitStoreLocal(b_local, b, bc.span);
                const result_local = try fb.addLocalWithSize("__min_r", TypeRegistry.I64, false, 8);
                const a_val = try fb.emitLoadLocal(a_local, TypeRegistry.I64, bc.span);
                const b_val = try fb.emitLoadLocal(b_local, TypeRegistry.I64, bc.span);
                const cmp = try fb.emitBinary(cmp_op, a_val, b_val, TypeRegistry.BOOL, bc.span);
                const then_block = try fb.newBlock("min.a");
                const else_block = try fb.newBlock("min.b");
                const merge_block = try fb.newBlock("min.merge");
                _ = try fb.emitBranch(cmp, then_block, else_block, bc.span);
                fb.setBlock(then_block);
                _ = try fb.emitStoreLocal(result_local, try fb.emitLoadLocal(a_local, TypeRegistry.I64, bc.span), bc.span);
                _ = try fb.emitJump(merge_block, bc.span);
                fb.setBlock(else_block);
                _ = try fb.emitStoreLocal(result_local, try fb.emitLoadLocal(b_local, TypeRegistry.I64, bc.span), bc.span);
                _ = try fb.emitJump(merge_block, bc.span);
                fb.setBlock(merge_block);
                return fb.emitLoadLocal(result_local, TypeRegistry.I64, bc.span);
            },
            // @max(a, b) — Zig Sema.zig:24678: if a > b then a else b
            .max => {
                const a = try self.lowerExprNode(bc.args[0]);
                const b = try self.lowerExprNode(bc.args[1]);
                const a_type = self.inferExprType(bc.args[0]);
                const a_info = self.type_reg.get(a_type);
                const use_unsigned = a_info == .basic and a_info.basic.isUnsigned();
                const cmp_op: ir.BinaryOp = if (use_unsigned) .gt_u else .gt;
                const a_local = try fb.addLocalWithSize("__max_a", TypeRegistry.I64, false, 8);
                _ = try fb.emitStoreLocal(a_local, a, bc.span);
                const b_local = try fb.addLocalWithSize("__max_b", TypeRegistry.I64, false, 8);
                _ = try fb.emitStoreLocal(b_local, b, bc.span);
                const result_local = try fb.addLocalWithSize("__max_r", TypeRegistry.I64, false, 8);
                const a_val = try fb.emitLoadLocal(a_local, TypeRegistry.I64, bc.span);
                const b_val = try fb.emitLoadLocal(b_local, TypeRegistry.I64, bc.span);
                const cmp = try fb.emitBinary(cmp_op, a_val, b_val, TypeRegistry.BOOL, bc.span);
                const then_block = try fb.newBlock("max.a");
                const else_block = try fb.newBlock("max.b");
                const merge_block = try fb.newBlock("max.merge");
                _ = try fb.emitBranch(cmp, then_block, else_block, bc.span);
                fb.setBlock(then_block);
                _ = try fb.emitStoreLocal(result_local, try fb.emitLoadLocal(a_local, TypeRegistry.I64, bc.span), bc.span);
                _ = try fb.emitJump(merge_block, bc.span);
                fb.setBlock(else_block);
                _ = try fb.emitStoreLocal(result_local, try fb.emitLoadLocal(b_local, TypeRegistry.I64, bc.span), bc.span);
                _ = try fb.emitJump(merge_block, bc.span);
                fb.setBlock(merge_block);
                return fb.emitLoadLocal(result_local, TypeRegistry.I64, bc.span);
            },
            // @alignCast(alignment, ptr) — identity at runtime, debug check would go here
            .align_cast => {
                return try self.lowerExprNode(bc.args[0]);
            },
            // @constCast(ptr) — identity, type-system only (Zig Sema.zig)
            .const_cast => {
                return try self.lowerExprNode(bc.args[0]);
            },
            // @arcRetain(val), @arcRelease(val) — conditional ARC builtins.
            // Resolved at monomorphization: emit cot_retain/cot_release only when
            // the argument type is ARC-managed. No-op for non-ARC types (i64, etc).
            // Reference: Swift value witness tables — destroy/copy resolved per-type.
            // In Cot, monomorphization makes the concrete type known at compile time.
            .arc_retain => {
                const arg = try self.lowerExprNode(bc.args[0]);
                const arg_type = self.inferExprType(bc.args[0]);
                if (self.type_reg.couldBeARC(arg_type)) {
                    var args = [_]ir.NodeIndex{arg};
                    return try fb.emitCall("retain", &args, false, arg_type, bc.span);
                }
                return arg;
            },
            .arc_release => {
                const arg = try self.lowerExprNode(bc.args[0]);
                const arg_type = self.inferExprType(bc.args[0]);
                if (self.type_reg.couldBeARC(arg_type)) {
                    var args = [_]ir.NodeIndex{arg};
                    _ = try fb.emitCall("release", &args, false, TypeRegistry.VOID, bc.span);
                }
                return ir.null_node;
            },
            // @panic("message") — Zig @panic: write file:line + message to stderr, then exit(2)
            // Reference: Zig std/debug.zig panic(), Go runtime.gopanic
            // Output: "file.cot:42: panic: user message\n"
            .panic => {
                const fd_arg = try fb.emitConstInt(2, TypeRegistry.I64, bc.span); // stderr

                // Emit file:line prefix (compile-time embedded, Zig pattern)
                const pos = self.err.src.position(bc.span.start);
                const loc_str = try std.fmt.allocPrint(self.allocator, "{s}:{d}: panic: ", .{ pos.filename, pos.line });
                const loc_idx = try fb.addStringLiteral(loc_str);
                const loc_val = try fb.emitConstSlice(loc_idx, bc.span);
                var loc_args = [_]ir.NodeIndex{ fd_arg, loc_val };
                _ = try fb.emitCall("write", &loc_args, true, TypeRegistry.I64, bc.span);

                if (bc.args[0] != null_node) {
                    const msg_arg = try self.lowerExprNode(bc.args[0]);
                    var write_args = [_]ir.NodeIndex{ fd_arg, msg_arg };
                    _ = try fb.emitCall("write", &write_args, true, TypeRegistry.I64, bc.span);
                }

                // Newline
                const nl_idx = try fb.addStringLiteral(try self.allocator.dupe(u8, "\n"));
                const nl_val = try fb.emitConstSlice(nl_idx, bc.span);
                var nl_args = [_]ir.NodeIndex{ fd_arg, nl_val };
                _ = try fb.emitCall("write", &nl_args, true, TypeRegistry.I64, bc.span);

                // Exit with code 2 (Go crash exit code)
                const exit_code = try fb.emitConstInt(2, TypeRegistry.I64, bc.span);
                var exit_args = [_]ir.NodeIndex{exit_code};
                _ = try fb.emitCall("exit", &exit_args, false, TypeRegistry.VOID, bc.span);

                _ = try fb.emitTrap(bc.span); // unreachable after exit
                const dead_block = try fb.newBlock("panic.dead");
                fb.setBlock(dead_block);
                return ir.null_node;
            },
            // @ctz(val) — Wasm i64.ctz (0x7A)
            // Reference: Wasm spec, Zig @ctz
            .ctz => {
                const val_arg = try self.lowerExprNode(bc.args[0]);
                return try fb.emitUnary(.ctz, val_arg, TypeRegistry.I64, bc.span);
            },
            // @clz(val) — Wasm i64.clz (0x79)
            // Reference: Wasm spec, Zig @clz
            .clz => {
                const val_arg = try self.lowerExprNode(bc.args[0]);
                return try fb.emitUnary(.clz, val_arg, TypeRegistry.I64, bc.span);
            },
            // @popCount(val) — Wasm i64.popcnt (0x7B)
            // Reference: Wasm spec, Zig @popCount
            .pop_count => {
                const val_arg = try self.lowerExprNode(bc.args[0]);
                return try fb.emitUnary(.popcnt, val_arg, TypeRegistry.I64, bc.span);
            },
            .type_name => {
                // @typeName(T) — comptime string
                const str = self.chk.evalConstString(bc.type_arg) orelse blk: {
                    // Resolve type and get name
                    const type_idx = self.resolveTypeNode(bc.type_arg);
                    break :blk self.type_reg.typeName(type_idx);
                };
                const copied = try self.allocator.dupe(u8, str);
                const str_idx = try fb.addStringLiteral(copied);
                return try fb.emitConstSlice(str_idx, bc.span);
            },
            .enum_name => {
                // @enumName(T, index) — comptime string
                if (self.chk.evalComptimeValue(bc.args[0])) |cv| {
                    if (cv.asInt()) |idx_val| {
                        const type_idx = self.resolveTypeNode(bc.type_arg);
                        const info = self.type_reg.get(type_idx);
                        if (info == .enum_type) {
                            for (info.enum_type.variants) |v| {
                                if (v.value == idx_val) {
                                    const copied = try self.allocator.dupe(u8, v.name);
                                    const str_idx = try fb.addStringLiteral(copied);
                                    return try fb.emitConstSlice(str_idx, bc.span);
                                }
                            }
                        }
                    }
                }
                return ir.null_node;
            },
            .type_info => {
                // @typeInfo(T) — only meaningful in comptime context.
                // If used as standalone expression, evaluate via evalComptimeValue and emit result.
                if (self.chk.evalComptimeValue(bc.args[0])) |cv| {
                    return try self.emitComptimeValue(cv, bc.span);
                }
                return ir.null_node;
            },
        }
    }

    /// Resolve field access on a comptime value variable.
    /// Returns the field's ComptimeValue if base is a comptime binding, null otherwise.
    fn resolveComptimeFieldAccess(self: *Lowerer, fa: ast.FieldAccess) ?comptime_mod.ComptimeValue {
        if (fa.base == ast.null_node) return null;
        const base_node = self.tree.getNode(fa.base) orelse return null;
        const base_expr = base_node.asExpr() orelse return null;
        if (base_expr != .ident) return null;
        const cv = self.lookupComptimeValue(base_expr.ident.name) orelse return null;
        return switch (cv) {
            .enum_field => |ef| {
                if (std.mem.eql(u8, fa.field, "name")) return comptime_mod.ComptimeValue{ .string = ef.name };
                if (std.mem.eql(u8, fa.field, "value")) return comptime_mod.ComptimeValue{ .int = ef.value };
                return null;
            },
            .type_info => |ti| {
                if (std.mem.eql(u8, fa.field, "name")) return comptime_mod.ComptimeValue{ .string = ti.name };
                if (std.mem.eql(u8, fa.field, "fields")) return comptime_mod.ComptimeValue{ .array = .{
                    .elements = ti.fields,
                    .elem_type_name = "EnumField",
                } };
                return null;
            },
            .array => |arr| {
                if (std.mem.eql(u8, fa.field, "len")) return comptime_mod.ComptimeValue{ .int = @intCast(arr.elements.items.len) };
                return null;
            },
            else => null,
        };
    }

    /// Emit a ComptimeValue as IR nodes.
    /// Converts structured comptime values (int, string, bool, array) to runtime IR.
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

    /// Emit a comptime array as static data in linear memory.
    /// Allocates heap memory and stores each element at the correct byte offset.
    /// Element size: 8 for int/bool, sizeOf(STRING)=24 for string (slice: ptr+len+cap).
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
                    // String: emit as literal, store ptr@offset and len@offset+8
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

    /// Extract string literal value from a node, stripping surrounding quotes.
    fn getStringLiteral(self: *Lowerer, idx: NodeIndex) ?[]const u8 {
        const node = self.tree.getNode(idx) orelse return null;
        const expr = node.asExpr() orelse return null;
        if (expr != .literal or expr.literal.kind != .string) return null;
        const v = expr.literal.value;
        if (v.len >= 2 and v[0] == '"' and v[v.len - 1] == '"') return v[1 .. v.len - 1];
        return v;
    }

    /// Look up a comptime value by name — checks function-local vars first, then globals.
    fn lookupComptimeValue(self: *Lowerer, name: []const u8) ?comptime_mod.ComptimeValue {
        return self.comptime_value_vars.get(name) orelse self.global_comptime_values.get(name);
    }

    /// Emit a bounds check: if index >= length, print Go-style panic message and exit.
    /// Go: runtime/panic.go goPanicIndex — OpIsInBounds: "index out of range [N] with length M"
    /// Zig: Sema.zig:26482 — addSafetyCheck pattern (branch + cold panic path).
    fn emitBoundsCheck(self: *Lowerer, fb: *ir.FuncBuilder, index_node: ir.NodeIndex, length_node: ir.NodeIndex, span: Span) !void {
        // OpIsInBounds: index < length (strict — for element access)
        try self.emitBoundsCheckImpl(fb, index_node, length_node, .lt, span);
    }

    /// Emit a slice bounds check: if index > length, panic.
    /// Go: runtime/panic.go goPanicSliceAlen — OpIsSliceInBounds: index <= length
    /// Slice end index CAN equal length (exclusive upper bound).
    fn emitSliceBoundsCheck(self: *Lowerer, fb: *ir.FuncBuilder, index_node: ir.NodeIndex, length_node: ir.NodeIndex, span: Span) !void {
        // OpIsSliceInBounds: index <= length (non-strict — for slice bounds)
        try self.emitBoundsCheckImpl(fb, index_node, length_node, .le, span);
    }

    fn emitBoundsCheckImpl(self: *Lowerer, fb: *ir.FuncBuilder, index_node: ir.NodeIndex, length_node: ir.NodeIndex, cmp_op: ir.BinaryOp, span: Span) !void {
        const in_bounds = try fb.emitBinary(cmp_op, index_node, length_node, TypeRegistry.BOOL, span);
        const ok_block = try fb.newBlock("bounds.ok");
        const fail_block = try fb.newBlock("bounds.fail");
        _ = try fb.emitBranch(in_bounds, ok_block, fail_block, span);
        fb.setBlock(fail_block);

        // Go pattern: "file:line: panic: index out of range [N] with length M\n"
        const fd = try fb.emitConstInt(2, TypeRegistry.I64, span); // stderr

        // Emit file:line prefix from source position
        const pos = self.err.src.position(span.start);
        const loc_str = try std.fmt.allocPrint(self.allocator, "{s}:{d}: ", .{ pos.filename, pos.line });
        const loc_idx = try fb.addStringLiteral(loc_str);
        const loc_val = try fb.emitConstSlice(loc_idx, span);
        var loc_args = [_]ir.NodeIndex{ fd, loc_val };
        _ = try fb.emitCall("write", &loc_args, true, TypeRegistry.I64, span);

        // "panic: index out of range ["
        const prefix = try fb.addStringLiteral(try self.allocator.dupe(u8, "panic: index out of range ["));
        const prefix_val = try fb.emitConstSlice(prefix, span);
        var prefix_args = [_]ir.NodeIndex{ fd, prefix_val };
        _ = try fb.emitCall("write", &prefix_args, true, TypeRegistry.I64, span);

        // Print the index value
        var idx_args = [_]ir.NodeIndex{index_node};
        _ = try fb.emitCall("eprint_int", &idx_args, false, TypeRegistry.VOID, span);

        // "] with length "
        const mid = try fb.addStringLiteral(try self.allocator.dupe(u8, "] with length "));
        const mid_val = try fb.emitConstSlice(mid, span);
        var mid_args = [_]ir.NodeIndex{ fd, mid_val };
        _ = try fb.emitCall("write", &mid_args, true, TypeRegistry.I64, span);

        // Print the length value
        var len_args = [_]ir.NodeIndex{length_node};
        _ = try fb.emitCall("eprint_int", &len_args, false, TypeRegistry.VOID, span);

        // Newline
        const nl = try fb.addStringLiteral(try self.allocator.dupe(u8, "\n"));
        const nl_val = try fb.emitConstSlice(nl, span);
        var nl_args = [_]ir.NodeIndex{ fd, nl_val };
        _ = try fb.emitCall("write", &nl_args, true, TypeRegistry.I64, span);

        // Exit with code 2 (Go crash exit code)
        const exit_code = try fb.emitConstInt(2, TypeRegistry.I64, span);
        var exit_args = [_]ir.NodeIndex{exit_code};
        _ = try fb.emitCall("exit", &exit_args, false, TypeRegistry.VOID, span);

        _ = try fb.emitTrap(span); // unreachable after exit
        fb.setBlock(ok_block);
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
            .tuple => |elems| {
                var elem_types = std.ArrayListUnmanaged(TypeIndex){};
                defer elem_types.deinit(self.allocator);
                for (elems) |e| {
                    elem_types.append(self.allocator, self.resolveTypeNode(e)) catch return TypeRegistry.VOID;
                }
                return self.type_reg.makeTuple(elem_types.items) catch TypeRegistry.VOID;
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

        // Look up error variant using global error table (unique index per variant name).
        // Zig pattern: each error value has a globally unique integer.
        const error_idx = self.getGlobalErrorIndex(el.error_name);

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
        // BUG FIX: emit cleanups on error path before returning
        // Zig reference: errdefer fires on try propagation (AstGen.zig:3132-3200)
        try self.emitCleanupsErrorPath(0);
        _ = try fb.emitRet(err_ret, te.span);

        // OK block: read the success payload at [ptr + 8]
        fb.setBlock(ok_block);
        const payload_addr_ok = try fb.emitAddrOffset(eu_ptr, 8, TypeRegistry.I64, te.span);
        return try fb.emitPtrLoadValue(payload_addr_ok, elem_type, te.span);
    }

    /// Lower await expr — poll a future to completion and extract the result.
    /// On Wasm: calls the poll function in a loop until READY, then reads result.
    /// On native: calls fiber_switch to suspend current fiber and resume target.
    fn lowerAwaitExpr(self: *Lowerer, ae: ast.AwaitExpr) Error!ir.NodeIndex {
        const fb = self.current_func orelse return ir.null_node;

        // Get the future type from the operand
        const operand_type = self.inferExprType(ae.operand);
        const operand_info = self.type_reg.get(operand_type);
        if (operand_info != .future) {
            return try self.lowerExprNode(ae.operand);
        }
        const result_type = operand_info.future.result_type;

        // Lower the future operand (this gives us the state/fiber pointer)
        const future_ptr = try self.lowerExprNode(ae.operand);

        // Check if result is compound (error union, >8 bytes) — needs multi-word copy
        const result_size = self.type_reg.sizeOf(result_type);
        const is_compound_result = result_size > 8;

        if (!self.target.isWasm()) {
            // Native (Zig approach): eager evaluation — result is already in future[8].
            // No poll loop needed. Just extract the result directly.
            if (is_compound_result) {
                // Compound result: copy all words from future[8..] into a local,
                // return pointer to the local (matching lowerTryExpr expectation)
                const num_words = (result_size + 7) / 8;
                const result_local = try fb.addLocalWithSize("__await_result", result_type, false, result_size);
                for (0..num_words) |w| {
                    const src_offset: i64 = @intCast(8 + w * 8);
                    const src_addr = try fb.emitAddrOffset(future_ptr, src_offset, TypeRegistry.I64, ae.span);
                    const word = try fb.emitPtrLoadValue(src_addr, TypeRegistry.I64, ae.span);
                    _ = try fb.emitStoreLocalField(result_local, @intCast(w), @intCast(w * 8), word, ae.span);
                }
                return try fb.emitAddrLocal(result_local, TypeRegistry.I64, ae.span);
            }
            const result_addr = try fb.emitAddrOffset(future_ptr, 8, TypeRegistry.I64, ae.span);
            return try fb.emitPtrLoadValue(result_addr, result_type, ae.span);
        }

        // Wasm (Rust approach): poll-based state machine.
        // Store future pointer to a local so it can be reloaded across blocks
        // (Wasm br_table dispatch doesn't carry values across blocks)
        const future_local = try fb.addLocalWithSize("__await_future", TypeRegistry.I64, false, 8);
        _ = try fb.emitStoreLocal(future_local, future_ptr, ae.span);

        const poll_name = self.resolveAsyncPollName(ae.operand) orelse "__async_poll";

        const poll_loop = try fb.newBlock("await.poll");
        const await_done = try fb.newBlock("await.done");
        _ = try fb.emitJump(poll_loop, ae.span);

        // Poll loop: reload future_ptr from local, call poll, check result
        fb.setBlock(poll_loop);
        const fp_poll = try fb.emitLoadLocal(future_local, TypeRegistry.I64, ae.span);
        var poll_args = [_]ir.NodeIndex{fp_poll};
        const poll_result = try fb.emitCall(poll_name, &poll_args, false, TypeRegistry.I64, ae.span);
        const one = try fb.emitConstInt(1, TypeRegistry.I64, ae.span);
        const is_ready = try fb.emitBinary(.eq, poll_result, one, TypeRegistry.BOOL, ae.span);
        _ = try fb.emitBranch(is_ready, await_done, poll_loop, ae.span);

        // Done: reload future_ptr, extract result from state[8..]
        fb.setBlock(await_done);
        const fp_done = try fb.emitLoadLocal(future_local, TypeRegistry.I64, ae.span);
        if (is_compound_result) {
            // Compound result: copy all words from state[8..] into a local,
            // return pointer to the local (matching lowerTryExpr expectation)
            const num_words = (result_size + 7) / 8;
            const result_local = try fb.addLocalWithSize("__await_result", result_type, false, result_size);
            for (0..num_words) |w| {
                const src_offset: i64 = @intCast(8 + w * 8);
                const src_addr = try fb.emitAddrOffset(fp_done, src_offset, TypeRegistry.I64, ae.span);
                const word = try fb.emitPtrLoadValue(src_addr, TypeRegistry.I64, ae.span);
                _ = try fb.emitStoreLocalField(result_local, @intCast(w), @intCast(w * 8), word, ae.span);
            }
            return try fb.emitAddrLocal(result_local, TypeRegistry.I64, ae.span);
        }
        const result_addr = try fb.emitAddrOffset(fp_done, 8, TypeRegistry.I64, ae.span);
        return try fb.emitPtrLoadValue(result_addr, result_type, ae.span);
    }

    /// Resolve the poll function name from an await operand.
    /// Handles two cases:
    /// 1. Direct call: `await foo(args)` → `foo_poll`
    /// 2. Variable: `await f` → look up f's poll name from async_poll_names map
    fn resolveAsyncPollName(self: *Lowerer, operand_idx: NodeIndex) ?[]const u8 {
        const node = self.tree.getNode(operand_idx) orelse return null;
        const expr = node.asExpr() orelse return null;
        // Case 1: direct call — `await foo(args)`
        if (expr == .call) {
            const callee_node = self.tree.getNode(expr.call.callee) orelse return null;
            const callee_expr = callee_node.asExpr() orelse return null;
            if (callee_expr == .ident) {
                return std.fmt.allocPrint(self.allocator, "{s}_poll", .{callee_expr.ident.name}) catch null;
            }
        }
        // Case 2: ident — `await f` where f was assigned from an async call
        if (expr == .ident) {
            return self.async_poll_names.get(expr.ident.name);
        }
        return null;
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
        const is_compound = (elem_type == TypeRegistry.STRING);

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
        if (is_compound) {
            // Compound type (string): read ptr at [eu_ptr+8], len at [eu_ptr+16]
            // Must match lowerStringInit pattern — decompose into field stores
            const ptr_addr = try fb.emitAddrOffset(eu_ptr, 8, TypeRegistry.I64, ce.span);
            const ptr_val = try fb.emitPtrLoadValue(ptr_addr, TypeRegistry.I64, ce.span);
            const len_addr = try fb.emitAddrOffset(eu_ptr, 16, TypeRegistry.I64, ce.span);
            const len_val = try fb.emitPtrLoadValue(len_addr, TypeRegistry.I64, ce.span);
            _ = try fb.emitStoreLocalField(result_local, 0, 0, ptr_val, ce.span);
            _ = try fb.emitStoreLocalField(result_local, 1, 8, len_val, ce.span);
        } else {
            const payload_addr = try fb.emitAddrOffset(eu_ptr, 8, TypeRegistry.I64, ce.span);
            const success_val = try fb.emitPtrLoadValue(payload_addr, elem_type, ce.span);
            _ = try fb.emitStoreLocal(result_local, success_val, ce.span);
        }
        _ = try fb.emitJump(merge_block, ce.span);

        // Error block: evaluate fallback
        fb.setBlock(err_block);
        var fallback_is_noreturn = false;
        if (ce.capture.len > 0) {
            const scope_depth = fb.markScopeEntry();
            const err_payload_addr = try fb.emitAddrOffset(eu_ptr, 8, TypeRegistry.I64, ce.span);
            const err_val = try fb.emitPtrLoadValue(err_payload_addr, TypeRegistry.I64, ce.span);
            const capture_local = try fb.addLocalWithSize(ce.capture, TypeRegistry.I64, false, 8);
            _ = try fb.emitStoreLocal(capture_local, err_val, ce.span);
            const fallback_val = try self.lowerExprNode(ce.fallback);
            if (fallback_val == ir.null_node) {
                fallback_is_noreturn = true;
            } else if (is_compound) {
                try self.storeCatchCompound(result_local, fallback_val, ce.span);
            } else {
                _ = try fb.emitStoreLocal(result_local, fallback_val, ce.span);
            }
            fb.restoreScope(scope_depth);
        } else {
            const fallback_val = try self.lowerExprNode(ce.fallback);
            if (fallback_val == ir.null_node) {
                fallback_is_noreturn = true;
            } else if (is_compound) {
                try self.storeCatchCompound(result_local, fallback_val, ce.span);
            } else {
                _ = try fb.emitStoreLocal(result_local, fallback_val, ce.span);
            }
        }
        if (!fallback_is_noreturn) {
            _ = try fb.emitJump(merge_block, ce.span);
        }

        // Merge block: load result
        fb.setBlock(merge_block);
        return try fb.emitLoadLocal(result_local, elem_type, ce.span);
    }

    // Helper: store a compound value (string ptr+len) into a local's fields.
    // Matches lowerStringInit decomposition pattern.
    fn storeCatchCompound(self: *Lowerer, local_idx: ir.LocalIdx, val: ir.NodeIndex, span: Span) !void {
        const fb = self.current_func orelse return;
        const ptr_type = self.type_reg.makePointer(TypeRegistry.U8) catch TypeRegistry.VOID;
        const ptr_val = try fb.emitSlicePtr(val, ptr_type, span);
        const len_val = try fb.emitSliceLen(val, span);
        _ = try fb.emitStoreLocalField(local_idx, 0, 0, ptr_val, span);
        _ = try fb.emitStoreLocalField(local_idx, 1, 8, len_val, span);
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

/// Process escape sequences in a raw string (no surrounding quotes).
/// Used for string interpolation text segments.
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
    var generic_ctx = checker.SharedGenericContext.init(allocator);
    defer generic_ctx.deinit(allocator);
    const target = target_mod.Target.native();
    var chk = checker.Checker.init(allocator, &tree, &type_reg, &err, &scope, &generic_ctx, target);
    defer chk.deinit();
    var lowerer = Lowerer.init(allocator, &tree, &type_reg, &err, &chk, target);
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
    var generic_ctx = checker.SharedGenericContext.init(allocator);
    const target = target_mod.Target.native();
    var chk = checker.Checker.init(allocator, &tree, &type_reg, &err, &global_scope, &generic_ctx, target);

    chk.checkFile() catch |e| {
        std.debug.print("Check error: {}\n", .{e});
        return error.CheckFailed;
    };
    if (err.hasErrors()) return error.CheckErrors;

    var lowerer = Lowerer.init(allocator, &tree, &type_reg, &err, &chk, target);
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
        \\    if (a > b) {
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
        \\    while (i < n) {
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
        \\    if (x < 0) {
        \\        return -1
        \\    } else {
        \\        if (x > 0) {
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
        \\    if (n <= 1) {
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
        \\const Color = enum { red, green, blue }
        \\fn get_red() Color { return Color.red }
    ;
    var result = try testPipeline(allocator, code);
    defer result.deinit();

    try std.testing.expect(!result.has_errors);
    try std.testing.expect(result.type_reg.lookupByName("Color") != null);
}
