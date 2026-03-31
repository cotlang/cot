//! Compilation Driver - Orchestrates the full pipeline from source to object code.

const std = @import("std");
const scanner_mod = @import("frontend/scanner.zig");
const ast_mod = @import("frontend/ast.zig");
const parser_mod = @import("frontend/parser.zig");
const errors_mod = @import("frontend/errors.zig");
const types_mod = @import("frontend/types.zig");
const checker_mod = @import("frontend/checker.zig");
const lower_mod = @import("frontend/lower.zig");
const ir_mod = @import("frontend/ir.zig");
const vwt_gen_mod = @import("frontend/vwt_gen.zig");
const ssa_builder_mod = @import("frontend/ssa_builder.zig");
const source_mod = @import("frontend/source.zig");
const token_mod = @import("frontend/token.zig");
// Native codegen imports (AOT compiler path)
// NOTE: Native codegen is being rewritten with Cranelift architecture.
// See CRANELIFT_PORT_MASTER_PLAN.md for status and progress.
const schedule = @import("ssa/passes/schedule.zig");
const layout = @import("ssa/passes/layout.zig");
const rewritegeneric = @import("ssa/passes/rewritegeneric.zig");
const decompose_builtin = @import("ssa/passes/decompose.zig");
const rewritedec = @import("ssa/passes/rewritedec.zig");
const lower_wasm = @import("ssa/passes/lower_wasm.zig");
const lower_native = @import("ssa/passes/lower_native.zig");
const deadcode = @import("ssa/passes/deadcode.zig");
const copyelim = @import("ssa/passes/copyelim.zig");
const async_split = @import("ssa/passes/async_split.zig");
const phielim = @import("ssa/passes/phielim.zig");
const cse_pass = @import("ssa/passes/cse.zig");
const target_mod = @import("frontend/target.zig");
const debug = @import("debug.zig");

// libts (TypeScript/JavaScript frontend)
const libts = @import("libts");

// Wasm codegen
const wasm_old = @import("codegen/wasm.zig"); // CodeBuilder for runtime function emission
const wasm = @import("codegen/wasm/wasm.zig"); // Go-style SSA codegen + Linker
const arc = @import("codegen/arc.zig"); // ARC runtime (Swift)
const mem_runtime = @import("codegen/mem_runtime.zig"); // Memory utils (memcpy, string_eq, etc.)
const slice_runtime = @import("codegen/slice_runtime.zig"); // Slice runtime (Go)
const print_runtime = @import("codegen/print_runtime.zig"); // Print runtime (Go)
const wasi_runtime = @import("codegen/wasi_runtime.zig"); // WASI runtime (fd_write)
const test_runtime = @import("codegen/test_runtime.zig"); // Test runtime (Zig)
const bench_runtime = @import("codegen/bench_runtime.zig"); // Bench runtime (Go testing.B)
const executor_runtime = @import("codegen/executor_runtime.zig"); // Executor runtime (Swift CooperativeExecutor)


const project_mod = @import("project.zig");

const Allocator = std.mem.Allocator;
const Target = target_mod.Target;

const ParsedFile = struct {
    path: []const u8,
    source_text: []const u8,
    source: source_mod.Source,
    tree: ast_mod.Ast,
};

const CheckedFileEntry = struct {
    scope: *checker_mod.Scope,
    tree: *const ast_mod.Ast,
};

pub const Driver = struct {
    allocator: Allocator,
    target: Target = Target.native(),
    test_mode: bool = false,
    test_filter: ?[]const u8 = null,
    fail_fast: bool = false,
    bench_mode: bool = false,
    bench_filter: ?[]const u8 = null,
    bench_n: ?i64 = null,
    release_mode: bool = false,
    debug_mode: bool = false, // --debug / -g: emit full DWARF for lldb + DWARF-based crash traces
    lib_mode: bool = false,
    project_safe: ?bool = null, // cached cot.json "safe" field, loaded lazily
    // Debug info: source file/text and IR funcs for DWARF generation
    debug_source_file: []const u8 = "",
    debug_source_text: []const u8 = "",
    debug_ir_funcs: []const ir_mod.Func = &.{},
    debug_type_reg: ?*const types_mod.TypeRegistry = null,
    // Multi-file source map: per-file paths/texts and func→file mapping.
    // Reference: Go runtime/symtab.go:funcfile() — per-CU file tables via cutab.
    // Reference: Zig Dwarf.zig:SrcLocCache — per-CU file arrays.
    parsed_file_paths: []const []const u8 = &.{},
    parsed_file_texts: []const []const u8 = &.{},
    func_file_indices: []const u16 = &.{},
    // User-declared extern fn names (from imported modules like std/sqlite).
    // Collected from checker scopes after type checking.
    // Registered in func_index_map so native backend can resolve calls.
    user_extern_fns: std.ArrayListUnmanaged([]const u8) = .{},
    // Extern fn signatures — parallel arrays with user_extern_fns.
    // Wasm-level param count (strings decomposed to ptr+len = 2 params).
    user_extern_param_counts: std.ArrayListUnmanaged(u32) = .{},
    user_extern_has_result: std.ArrayListUnmanaged(bool) = .{},
    /// COT_SSA: function name to generate interactive HTML visualizer for.
    /// Set via COT_SSA env var or --ssa=funcname CLI flag.
    /// Reference: Go GOSSAFUNC env var (ssagen/ssa.go lines 44-77)
    ssa_html_func: ?[]const u8 = null,
    pub fn init(allocator: Allocator) Driver {
        var d = Driver{ .allocator = allocator };
        // Read COT_SSA env var for HTML visualizer
        d.ssa_html_func = std.posix.getenv("COT_SSA");
        return d;
    }

    pub fn setTarget(self: *Driver, t: Target) void {
        self.target = t;
    }

    pub fn setTestMode(self: *Driver, enabled: bool) void {
        self.test_mode = enabled;
    }

    pub fn setTestFilter(self: *Driver, filter: []const u8) void {
        self.test_filter = filter;
    }

    pub fn setFailFast(self: *Driver, enabled: bool) void {
        self.fail_fast = enabled;
    }

    pub fn setBenchMode(self: *Driver, enabled: bool) void {
        self.bench_mode = enabled;
    }

    pub fn setBenchFilter(self: *Driver, filter: []const u8) void {
        self.bench_filter = filter;
    }

    pub fn setBenchN(self: *Driver, n: i64) void {
        self.bench_n = n;
    }

    /// Lazily load and cache the project-level @safe flag from cot.json.
    /// Walks up from the input file's directory (like npm's package.json resolution)
    /// so nested files (e.g. self/frontend/scanner.cot) find self/cot.json.
    fn isProjectSafe(self: *Driver, file_path: ?[]const u8) bool {
        if (self.project_safe) |s| return s;
        // Try CWD first
        const maybe_loaded = project_mod.loadConfig(self.allocator, null) catch null;
        if (maybe_loaded) |loaded_val| {
            var loaded = loaded_val;
            defer loaded.deinit();
            const safe = loaded.value().safe orelse false;
            self.project_safe = safe;
            return safe;
        }
        // Walk up from the input file's directory looking for cot.json
        if (file_path) |fp| {
            var dir: ?[]const u8 = std.fs.path.dirname(fp);
            var depth: usize = 0;
            while (dir != null and depth < 10) : (depth += 1) {
                const d = dir.?;
                const maybe_file_loaded = project_mod.loadConfig(self.allocator, d) catch null;
                if (maybe_file_loaded) |loaded_val| {
                    var loaded = loaded_val;
                    defer loaded.deinit();
                    const safe = loaded.value().safe orelse false;
                    self.project_safe = safe;
                    return safe;
                }
                // Go up one level
                const parent = std.fs.path.dirname(d);
                if (parent != null and std.mem.eql(u8, parent.?, d)) break; // reached root
                dir = parent;
            }
        }
        self.project_safe = false;
        return false;
    }

    /// Type-check a source file without compiling (supports imports).
    /// Runs Phase 1 (parse) and Phase 2 (type check), then stops.
    /// On error: errors already printed by ErrorReporter.
    pub fn checkFile(self: *Driver, path: []const u8) !void {
        var seen_files = std.StringHashMap(void).init(self.allocator);
        defer seen_files.deinit();

        // Phase 1: Parse all files recursively
        var parsed_files = std.ArrayListUnmanaged(ParsedFile){};
        defer {
            for (parsed_files.items) |*pf| {
                pf.tree.deinit();
                pf.source.deinit();
                self.allocator.free(pf.source_text);
                self.allocator.free(pf.path);
            }
            parsed_files.deinit(self.allocator);
        }
        var in_progress = std.StringHashMap(void).init(self.allocator);
        defer in_progress.deinit();
        try self.parseFileRecursive(path, &parsed_files, &seen_files, &in_progress);

        // Apply project-level @safe mode from cot.json (search CWD + file's dir)
        if (self.isProjectSafe(path)) {
            for (parsed_files.items) |*pf| {
                if (pf.tree.file) |*file| {
                    // Don't override stdlib files — they control their own safe mode
                    if (!std.mem.containsAtLeast(u8, file.filename, 1, "stdlib/")) {
                        file.safe_mode = true;
                    }
                }
            }
        }

        // Phase 2: Type check all files with shared symbol table
        var type_reg = try types_mod.TypeRegistry.init(self.allocator);
        defer type_reg.deinit();
        var global_scope = checker_mod.Scope.init(self.allocator, null);
        defer global_scope.deinit();
        var generic_ctx = checker_mod.SharedGenericContext.init(self.allocator);
        defer generic_ctx.deinit(self.allocator);

        // Incremental per-file scopes: create scope → copy imports → check → store
        var checked_scopes = std.StringHashMap(CheckedFileEntry).init(self.allocator);
        defer checked_scopes.deinit();
        var file_scopes = std.ArrayListUnmanaged(*checker_mod.Scope){};
        defer {
            for (file_scopes.items) |fs| {
                fs.deinit();
                self.allocator.destroy(fs);
            }
            file_scopes.deinit(self.allocator);
        }

        for (parsed_files.items) |*pf| {
            const file_scope = try self.createFileScope(pf, &global_scope, &checked_scopes);
            try file_scopes.append(self.allocator, file_scope);

            var err_reporter = errors_mod.ErrorReporter.init(&pf.source, null);
            var chk = checker_mod.Checker.init(self.allocator, &pf.tree, &type_reg, &err_reporter, file_scope, &global_scope, &generic_ctx, self.target);
            defer chk.deinit();
            chk.checkFile() catch |e| {
                return e;
            };
            if (err_reporter.hasErrors()) {
                return error.TypeCheckError;
            }

            // Store checked scope for later files to import from
            try checked_scopes.put(pf.path, .{ .scope = file_scope, .tree = &pf.tree });
        }
        // Success — no errors found
    }

    /// Lint a source file: type-check + run lint rules. Returns warning count.
    pub fn lintFile(self: *Driver, path: []const u8) !u32 {
        var seen_files = std.StringHashMap(void).init(self.allocator);
        defer seen_files.deinit();

        // Phase 1: Parse all files recursively
        var parsed_files = std.ArrayListUnmanaged(ParsedFile){};
        defer {
            for (parsed_files.items) |*pf| {
                pf.tree.deinit();
                pf.source.deinit();
                self.allocator.free(pf.source_text);
                self.allocator.free(pf.path);
            }
            parsed_files.deinit(self.allocator);
        }
        var in_progress = std.StringHashMap(void).init(self.allocator);
        defer in_progress.deinit();
        try self.parseFileRecursive(path, &parsed_files, &seen_files, &in_progress);

        // Apply project-level @safe mode from cot.json (search CWD + file's dir)
        if (self.isProjectSafe(path)) {
            for (parsed_files.items) |*pf| {
                if (pf.tree.file) |*file| {
                    // Don't override stdlib files — they control their own safe mode
                    if (!std.mem.containsAtLeast(u8, file.filename, 1, "stdlib/")) {
                        file.safe_mode = true;
                    }
                }
            }
        }

        // Phase 2: Type check all files with shared symbol table + lint
        var type_reg = try types_mod.TypeRegistry.init(self.allocator);
        defer type_reg.deinit();
        var global_scope = checker_mod.Scope.init(self.allocator, null);
        defer global_scope.deinit();
        var generic_ctx = checker_mod.SharedGenericContext.init(self.allocator);
        defer generic_ctx.deinit(self.allocator);

        // Incremental per-file scopes: create scope → copy imports → check → store
        var checked_scopes = std.StringHashMap(CheckedFileEntry).init(self.allocator);
        defer checked_scopes.deinit();
        var file_scopes = std.ArrayListUnmanaged(*checker_mod.Scope){};
        defer {
            for (file_scopes.items) |fs| {
                fs.deinit();
                self.allocator.destroy(fs);
            }
            file_scopes.deinit(self.allocator);
        }

        var total_warnings: u32 = 0;
        for (parsed_files.items) |*pf| {
            const file_scope = try self.createFileScope(pf, &global_scope, &checked_scopes);
            try file_scopes.append(self.allocator, file_scope);

            var err_reporter = errors_mod.ErrorReporter.init(&pf.source, null);
            var chk = checker_mod.Checker.init(self.allocator, &pf.tree, &type_reg, &err_reporter, file_scope, &global_scope, &generic_ctx, self.target);
            chk.lint_mode = true;
            chk.checkFile() catch |e| {
                chk.deinit();
                return e;
            };
            if (err_reporter.hasErrors()) {
                chk.deinit();
                return error.TypeCheckError;
            }
            chk.runLintChecks();
            total_warnings += err_reporter.warning_count;
            chk.deinit();

            // Store checked scope for later files to import from
            try checked_scopes.put(pf.path, .{ .scope = file_scope, .tree = &pf.tree });
        }
        return total_warnings;
    }

    /// Compile source code to machine code (single file, no imports).
    pub fn compileSource(self: *Driver, source_text: []const u8) ![]u8 {
        var src = source_mod.Source.init(self.allocator, "<input>", source_text);
        defer src.deinit();
        var err_reporter = errors_mod.ErrorReporter.init(&src, null);

        // Parse
        var tree = ast_mod.Ast.init(self.allocator);
        defer tree.deinit();
        var scan = scanner_mod.Scanner.initWithErrors(&src, &err_reporter);
        var parser = parser_mod.Parser.init(self.allocator, &scan, &tree, &err_reporter);
        try parser.parseFile();
        if (err_reporter.hasErrors()) return error.ParseError;

        // Type check
        var type_reg = try types_mod.TypeRegistry.init(self.allocator);
        defer type_reg.deinit();
        var global_scope = checker_mod.Scope.init(self.allocator, null);
        defer global_scope.deinit();
        var generic_ctx = checker_mod.SharedGenericContext.init(self.allocator);
        defer generic_ctx.deinit(self.allocator);
        var chk = checker_mod.Checker.init(self.allocator, &tree, &type_reg, &err_reporter, &global_scope, &global_scope, &generic_ctx, self.target);
        defer chk.deinit();
        try chk.checkFile();
        if (err_reporter.hasErrors()) return error.TypeCheckError;

        // Collect user-declared extern fn names for native func_index_map
        self.collectExternFns(&global_scope, &type_reg);

        // Lower to IR
        var lowerer = lower_mod.Lowerer.init(self.allocator, &tree, &type_reg, &err_reporter, &chk, self.target);
        defer lowerer.deinit();
        lowerer.release_mode = self.release_mode;
        if (self.test_mode) lowerer.setTestMode(true);
        if (self.fail_fast) lowerer.setFailFast(true);
        // Pre-allocate metadata globals for all generic type args BEFORE lowering.
        // This ensures emitTypeMetadataRef can find the global during call site lowering.
        // The globals are populated later by emitTrivialTypeMetadata / emitVWTWitnesses.
        debug.log(.codegen, "generic_inst_by_name has {d} entries before prealloc (compileSource path)", .{generic_ctx.generic_inst_by_name.count()});
        try self.preallocateTypeMetadataGlobals(&lowerer.builder, &type_reg, &generic_ctx);

        try lowerer.lowerToBuilder();
        // ARC Phase 4: Generate synthetic deinit functions for structs with ARC fields
        try lowerer.emitPendingAutoDeinits();
        if (err_reporter.hasErrors()) return error.LowerError;

        // Go init function pattern: emit __cot_init_globals after all decls are lowered
        try lowerer.generateGlobalInits();

        // Generate test runner if in test mode
        if (self.test_mode and lowerer.test_names.items.len > 0) {
            try lowerer.generateTestRunner();
        } else if (self.test_mode and lowerer.test_names.items.len == 0) {
            // No test blocks in this file — nothing to compile
            std.debug.print("\nok | 0 passed\n", .{});
            return &.{};
        }

        try self.emitVWTWitnesses(&lowerer.builder, &type_reg);
        try self.emitTrivialTypeMetadata(&lowerer.builder, &type_reg, &generic_ctx);
        try self.emitProtocolWitnessTables(&lowerer.builder, &generic_ctx);
        try self.emitMetadataInitMaster(&lowerer.builder);

        var ir_result = try lowerer.builder.getIR();
        defer ir_result.deinit();

        return self.generateCode(ir_result.funcs, ir_result.globals, &type_reg, "<input>", source_text);
    }

    /// Emit VWT witness functions for all non-trivial concrete types in the registry.
    /// Witnesses are emitted as IR functions into the builder and flow through the
    /// standard SSA → codegen pipeline alongside user functions.
    fn emitVWTWitnesses(self: *Driver, builder: *ir_mod.Builder, type_reg: *types_mod.TypeRegistry) !void {
        if (self.target.isWasm()) return;
        var gen = vwt_gen_mod.VWTGenerator.init(builder.allocator, type_reg);
        defer gen.deinit();

        var vwt_emitted: usize = 0;
        // Swift pattern: emit VWT for EVERY non-trivial concrete type.
        // Every type that participates in generic code gets its own VWT table.
        // Reference: apple/swift lib/IRGen/GenValueWitness.cpp emitValueWitnessTable()
        for (type_reg.types.items, 0..) |t, i| {
            const type_idx: types_mod.TypeIndex = @intCast(i);
            if (type_reg.isTrivial(type_idx)) continue;

            const type_name = switch (t) {
                .struct_type => |s| s.name,
                .union_type => |u| u.name,
                .enum_type => |e| e.name,
                .pointer => "pointer",
                .list => "list",
                .map => "map",
                .optional => "optional",
                .error_union => "error_union",
                .slice => "slice",
                .array => "array",
                .tuple => "tuple",
                .distinct => |d| d.name,
                else => continue,
            };

            _ = gen.emitWitnesses(builder, type_idx, type_name) catch |err| switch (err) {
                error.MissingVWTEntry => continue,
                else => return err,
            };
            vwt_emitted += 1;
        }
        if (vwt_emitted > 0) std.debug.print("VWT: {d} types emitted, {d} unique witnesses, total funcs: {d}\n", .{ vwt_emitted, gen.emitted.count(), builder.funcs.items.len });
    }

    /// Emit lightweight TypeMetadata globals for trivial types (i64, i32, bool, f64, etc.)
    /// used as generic type arguments. Non-trivial types already get metadata via emitVWTWitnesses.
    /// Swift ref: every type has metadata — BuiltinIntegers.swift, Metadata.h.
    /// Layout: [vwt_ptr=0, size, stride, kind=0x100 (trivial)]
    fn emitTrivialTypeMetadata(self: *Driver, builder: *ir_mod.Builder, type_reg: *types_mod.TypeRegistry, generic_ctx: *checker_mod.SharedGenericContext) !void {
        _ = self;
        const Span = @import("frontend/source.zig").Span;
        var count: usize = 0;

        // Collect all type args from all generic instantiations
        var it = generic_ctx.generic_inst_by_name.valueIterator();
        while (it.next()) |inst_info| {
            for (inst_info.type_args) |type_arg| {
                const type_name = type_reg.typeName(type_arg);
                const meta_name = try std.fmt.allocPrint(builder.allocator, "__type_metadata_{s}", .{type_name});

                // Skip if a VWT init function already handles this type's metadata
                const vwt_init_name = try std.fmt.allocPrint(builder.allocator, "__vwt_init_{s}", .{type_name});
                if (builder.hasFunc(vwt_init_name)) continue;

                // Skip if trivial init already emitted for this type (dedup)
                const init_name_check = try std.fmt.allocPrint(builder.allocator, "__trivial_meta_init_{s}", .{type_name});
                if (builder.hasFunc(init_name_check)) continue;

                // Trivial type — emit lightweight metadata (no VWT witnesses needed)
                const size = type_reg.sizeOf(type_arg);
                const stride = size;
                // Global already pre-allocated by preallocateTypeMetadataGlobals

                // Emit init function
                const init_name = try std.fmt.allocPrint(builder.allocator, "__trivial_meta_init_{s}", .{type_name});
                builder.startFunc(init_name, types_mod.TypeRegistry.VOID, types_mod.TypeRegistry.VOID, Span.zero);
                if (builder.func()) |fb| {
                    const meta_info = builder.lookupGlobal(meta_name) orelse continue;
                    const meta_addr = try fb.emitAddrGlobal(meta_info.idx, meta_name, types_mod.TypeRegistry.I64, Span.zero);

                    // [0] vwt_ptr = 0 (no VWT for trivial types)
                    const zero = try fb.emitConstInt(0, types_mod.TypeRegistry.I64, Span.zero);
                    _ = try fb.emitPtrStoreValue(meta_addr, zero, Span.zero);

                    // [1] size
                    const eight = try fb.emitConstInt(8, types_mod.TypeRegistry.I64, Span.zero);
                    const size_addr = try fb.emitBinary(.add, meta_addr, eight, types_mod.TypeRegistry.I64, Span.zero);
                    const size_val = try fb.emitConstInt(@intCast(size), types_mod.TypeRegistry.I64, Span.zero);
                    _ = try fb.emitPtrStoreValue(size_addr, size_val, Span.zero);

                    // [2] stride
                    const sixteen = try fb.emitConstInt(16, types_mod.TypeRegistry.I64, Span.zero);
                    const stride_addr = try fb.emitBinary(.add, meta_addr, sixteen, types_mod.TypeRegistry.I64, Span.zero);
                    const stride_val = try fb.emitConstInt(@intCast(stride), types_mod.TypeRegistry.I64, Span.zero);
                    _ = try fb.emitPtrStoreValue(stride_addr, stride_val, Span.zero);

                    // [3] type_idx — store actual TypeIndex for runtime dispatch
                    // Swift: metadata stores type identity. Cot: stores TypeIndex.
                    const twentyfour = try fb.emitConstInt(24, types_mod.TypeRegistry.I64, Span.zero);
                    const kind_addr = try fb.emitBinary(.add, meta_addr, twentyfour, types_mod.TypeRegistry.I64, Span.zero);
                    const kind_val = try fb.emitConstInt(@intCast(type_arg), types_mod.TypeRegistry.I64, Span.zero);
                    _ = try fb.emitPtrStoreValue(kind_addr, kind_val, Span.zero);

                    // [4] hash_fn_ptr — Swift PWT pattern: protocol witness in metadata.
                    // Only store if the hash function exists in the program (Map imported).
                    const thirtytwo = try fb.emitConstInt(32, types_mod.TypeRegistry.I64, Span.zero);
                    const hash_addr = try fb.emitBinary(.add, meta_addr, thirtytwo, types_mod.TypeRegistry.I64, Span.zero);
                    const hash_fn: ?[]const u8 = if (type_arg == types_mod.TypeRegistry.STRING)
                        "std.map.string_hash"
                    else if (type_arg == types_mod.TypeRegistry.I64 or type_arg == types_mod.TypeRegistry.I32 or
                        type_arg == types_mod.TypeRegistry.I16 or type_arg == types_mod.TypeRegistry.I8 or
                        type_arg == types_mod.TypeRegistry.U64 or type_arg == types_mod.TypeRegistry.U32 or
                        type_arg == types_mod.TypeRegistry.U16 or type_arg == types_mod.TypeRegistry.U8)
                        "std.map.i64_hash"
                    else
                        null;
                    if (hash_fn) |hfn| {
                        if (builder.hasFunc(hfn)) {
                            const fn_addr = try fb.emitFuncAddr(hfn, types_mod.TypeRegistry.I64, Span.zero);
                            _ = try fb.emitPtrStoreValue(hash_addr, fn_addr, Span.zero);
                        } else {
                            _ = try fb.emitPtrStoreValue(hash_addr, zero, Span.zero);
                        }
                    } else {
                        _ = try fb.emitPtrStoreValue(hash_addr, zero, Span.zero);
                    }

                    _ = try fb.emitRet(null, Span.zero);
                }
                try builder.endFunc();
                count += 1;
            }
        }

        // Register all trivial metadata init functions in __cot_init_globals
        if (count > 0) {
            debug.log(.codegen, "Trivial metadata: {d} types emitted", .{count});
        }
    }

    /// Pre-allocate __type_metadata_{Type} globals for ALL types used as generic
    /// type arguments. Called BEFORE lowering so emitTypeMetadataRef can find them.
    /// The globals are empty (zeroed) — populated later by emitVWTWitnesses /
    /// emitTrivialTypeMetadata init functions.
    fn preallocateTypeMetadataGlobals(self: *Driver, builder: *ir_mod.Builder, type_reg: *types_mod.TypeRegistry, generic_ctx: *checker_mod.SharedGenericContext) !void {
        _ = self;
        const Span = @import("frontend/source.zig").Span;
        var count: usize = 0;
        var it = generic_ctx.generic_inst_by_name.valueIterator();
        while (it.next()) |inst_info| {
            for (inst_info.type_args) |type_arg| {
                const type_name = type_reg.typeName(type_arg);
                const meta_name = try std.fmt.allocPrint(builder.allocator, "__type_metadata_{s}", .{type_name});
                if (builder.lookupGlobal(meta_name) != null) continue;
                // 40 bytes = 5 i64 slots: [vwt_ptr, size, stride, kind, hash_fn_ptr]
                try builder.addGlobal(ir_mod.Global.initWithSize(meta_name, types_mod.TypeRegistry.I64, false, Span.zero, 40));
                debug.log(.codegen, "preallocate metadata: '{s}' size={d}", .{ meta_name, type_reg.sizeOf(type_arg) });
                count += 1;
            }
        }
        if (count > 0) debug.log(.codegen, "pre-allocated {d} type metadata globals", .{count});
    }

    /// Generate __cot_init_metadata master function that calls ALL VWT init,
    /// trivial metadata init, and PWT init functions.
    /// Called from main() / test runner before any user code runs.
    /// Swift ref: metadata is static constants, but Cot uses init functions
    /// because our Global struct doesn't yet support initial data values.
    fn emitMetadataInitMaster(self: *Driver, builder: *ir_mod.Builder) !void {
        _ = self;
        const Span = @import("frontend/source.zig").Span;
        builder.startFunc("__cot_init_metadata", types_mod.TypeRegistry.VOID, types_mod.TypeRegistry.VOID, Span.zero);
        if (builder.func()) |fb| {
            // Call every __vwt_init_*, __trivial_meta_init_*, and __pwt_init_* function
            for (builder.funcs.items) |func| {
                if (std.mem.startsWith(u8, func.name, "__vwt_init_") or
                    std.mem.startsWith(u8, func.name, "__trivial_meta_init_") or
                    std.mem.startsWith(u8, func.name, "__pwt_init_"))
                {
                    var no_args = [_]ir_mod.NodeIndex{};
                    _ = try fb.emitCall(func.name, &no_args, false, types_mod.TypeRegistry.VOID, Span.zero);
                }
            }
            _ = try fb.emitRet(null, Span.zero);
        }
        try builder.endFunc();
    }

    /// Emit Protocol Witness Tables for each `impl Trait for Type` conformance.
    /// Each PWT is a global array of function pointers, one per trait method.
    /// Swift ref: GenProto.cpp emitWitnessTable()
    fn emitProtocolWitnessTables(self: *Driver, builder: *ir_mod.Builder, generic_ctx: *checker_mod.SharedGenericContext) !void {
        _ = self;
        const Span = @import("frontend/source.zig").Span;
        var pwt_count: usize = 0;

        // Iterate all trait implementations: key="Trait:Type", value=trait_name
        var it = generic_ctx.trait_impls.iterator();
        while (it.next()) |entry| {
            const impl_key = entry.key_ptr.*;
            const trait_name = entry.value_ptr.*;

            // Parse target type from impl_key "Trait:Type"
            const colon_pos = std.mem.indexOf(u8, impl_key, ":") orelse continue;
            const target_type = impl_key[colon_pos + 1 ..];
            debug.log(.codegen, "PWT: impl_key='{s}' trait='{s}' target='{s}'", .{ impl_key, trait_name, target_type });

            // Look up trait definition for method names
            const trait_def = generic_ctx.trait_defs.get(trait_name) orelse continue;
            const method_count = trait_def.method_names.len;
            if (method_count == 0) continue;

            // Global name: __pwt_{Trait}_{Type}
            const pwt_name = try std.fmt.allocPrint(builder.allocator, "__pwt_{s}_{s}", .{ trait_name, target_type });

            // Skip if already emitted (multi-file builds)
            if (builder.lookupGlobal(pwt_name) != null) continue;

            // Allocate global: method_count * 8 bytes (one function pointer per method)
            const pwt_size: u32 = @intCast(method_count * 8);
            try builder.addGlobal(ir_mod.Global.initWithSize(pwt_name, types_mod.TypeRegistry.I64, false, Span.zero, pwt_size));

            // Emit init function: __pwt_init_{Trait}_{Type}
            const init_name = try std.fmt.allocPrint(builder.allocator, "__pwt_init_{s}_{s}", .{ trait_name, target_type });
            builder.startFunc(init_name, types_mod.TypeRegistry.VOID, types_mod.TypeRegistry.VOID, Span.zero);
            if (builder.func()) |fb| {
                const pwt_info = builder.lookupGlobal(pwt_name) orelse continue;
                const pwt_addr = try fb.emitAddrGlobal(pwt_info.idx, pwt_name, types_mod.TypeRegistry.I64, Span.zero);

                // Store function pointers for each trait method
                for (trait_def.method_names, 0..) |method_name, i| {
                    // Concrete method name: "{Type}_{method}" (e.g., "Dog_speak")
                    // May be module-qualified: "{module}.{Type}_{method}"
                    const bare_fn = try std.fmt.allocPrint(builder.allocator, "{s}_{s}", .{ target_type, method_name });
                    // Search builder for the actual function name (may be module-qualified)
                    const concrete_fn = if (builder.hasFunc(bare_fn))
                        bare_fn
                    else blk: {
                        // Search all functions for a suffix match
                        var found_name: []const u8 = bare_fn;
                        for (builder.funcs.items) |func| {
                            if (std.mem.endsWith(u8, func.name, bare_fn)) {
                                // Verify it ends with ".{bare_fn}" (module-qualified)
                                if (func.name.len > bare_fn.len and func.name[func.name.len - bare_fn.len - 1] == '.') {
                                    found_name = func.name;
                                    break;
                                }
                            }
                        }
                        break :blk found_name;
                    };
                    const fn_addr = try fb.emitFuncAddr(concrete_fn, types_mod.TypeRegistry.I64, Span.zero);
                    const offset = try fb.emitConstInt(@intCast(i * 8), types_mod.TypeRegistry.I64, Span.zero);
                    const slot_addr = try fb.emitBinary(.add, pwt_addr, offset, types_mod.TypeRegistry.I64, Span.zero);
                    _ = try fb.emitPtrStoreValue(slot_addr, fn_addr, Span.zero);
                }

                _ = try fb.emitRet(null, Span.zero);
            }
            try builder.endFunc();
            pwt_count += 1;
        }
        if (pwt_count > 0) std.debug.print("PWT: {d} conformances emitted\n", .{pwt_count});
    }

    /// Compile a source file (supports imports).
    pub fn compileFile(self: *Driver, path: []const u8) ![]u8 {
        var seen_files = std.StringHashMap(void).init(self.allocator);
        defer seen_files.deinit();

        // Phase 1: Parse all files recursively
        var parsed_files = std.ArrayListUnmanaged(ParsedFile){};
        defer {
            for (parsed_files.items) |*pf| {
                pf.tree.deinit();
                pf.source.deinit();
                self.allocator.free(pf.source_text);
                self.allocator.free(pf.path);
            }
            parsed_files.deinit(self.allocator);
        }
        var in_progress = std.StringHashMap(void).init(self.allocator);
        defer in_progress.deinit();
        try self.parseFileRecursive(path, &parsed_files, &seen_files, &in_progress);

        // Apply project-level @safe mode from cot.json (search CWD + file's dir)
        if (self.isProjectSafe(path)) {
            for (parsed_files.items) |*pf| {
                if (pf.tree.file) |*file| {
                    // Don't override stdlib files — they control their own safe mode
                    if (!std.mem.containsAtLeast(u8, file.filename, 1, "stdlib/")) {
                        file.safe_mode = true;
                    }
                }
            }
        }

        // Phase 2: Type check all files with shared symbol table
        var type_reg = try types_mod.TypeRegistry.init(self.allocator);
        defer type_reg.deinit();
        var global_scope = checker_mod.Scope.init(self.allocator, null);
        defer global_scope.deinit();
        var generic_ctx = checker_mod.SharedGenericContext.init(self.allocator);
        defer generic_ctx.deinit(self.allocator);

        var checkers = std.ArrayListUnmanaged(checker_mod.Checker){};
        defer {
            for (checkers.items) |*chk| chk.deinit();
            checkers.deinit(self.allocator);
        }

        // Incremental per-file scopes: create scope → copy imports → check → store
        var checked_scopes = std.StringHashMap(CheckedFileEntry).init(self.allocator);
        defer checked_scopes.deinit();
        var file_scopes = std.ArrayListUnmanaged(*checker_mod.Scope){};
        defer {
            for (file_scopes.items) |fs| {
                fs.deinit();
                self.allocator.destroy(fs);
            }
            file_scopes.deinit(self.allocator);
        }

        for (parsed_files.items) |*pf| {
            const file_scope = try self.createFileScope(pf, &global_scope, &checked_scopes);
            try file_scopes.append(self.allocator, file_scope);

            var err_reporter = errors_mod.ErrorReporter.init(&pf.source, null);
            var chk = checker_mod.Checker.init(self.allocator, &pf.tree, &type_reg, &err_reporter, file_scope, &global_scope, &generic_ctx, self.target);
            chk.checkFile() catch |e| {
                chk.deinit();
                return e;
            };
            if (err_reporter.hasErrors()) {
                chk.deinit();
                return error.TypeCheckError;
            }
            try checkers.append(self.allocator, chk);

            // Store checked scope for later files to import from
            try checked_scopes.put(pf.path, .{ .scope = file_scope, .tree = &pf.tree });
        }

        // Collect user-declared extern fn names for native func_index_map
        for (file_scopes.items) |fs| {
            self.collectExternFns(fs, &type_reg);
        }
        self.collectExternFns(&global_scope, &type_reg);

        // Go LinkFuncName pattern: build module name map for qualified IR function names.
        // This prevents name collisions when two modules export functions with the same name
        // (e.g., std/json.parse vs std/semver.parse). Each function gets a module-qualified
        // IR name: "std.json.parse" vs "std.semver.parse".
        var canonical_path_to_module = std.StringHashMap([]const u8).init(self.allocator);
        defer canonical_path_to_module.deinit();

        // Main file (last in parsed_files — dependencies first) gets basename as module name
        {
            const main_pf = &parsed_files.items[parsed_files.items.len - 1];
            const main_base = std.fs.path.basename(main_pf.path);
            const main_module = if (std.mem.endsWith(u8, main_base, ".cot"))
                try self.allocator.dupe(u8, main_base[0 .. main_base.len - 4])
            else
                try self.allocator.dupe(u8, main_base);
            try canonical_path_to_module.put(main_pf.path, main_module);
        }

        // Derive module names from import paths for all imported files
        for (parsed_files.items) |*pf| {
            const imports = try pf.tree.getImports(self.allocator);
            defer self.allocator.free(imports);
            const file_dir = std.fs.path.dirname(pf.path) orelse ".";
            for (imports) |import_path| {
                const full_path = if (std.mem.startsWith(u8, import_path, "std/"))
                    try self.resolveStdImport(import_path, file_dir)
                else blk: {
                    const has_ext = std.mem.endsWith(u8, import_path, ".cot") or
                        std.mem.endsWith(u8, import_path, ".ts") or
                        std.mem.endsWith(u8, import_path, ".js");
                    if (has_ext) {
                        break :blk try std.fs.path.join(self.allocator, &.{ file_dir, import_path });
                    }
                    // Try .ts first, then .js, then .cot
                    const ts_name = try std.fmt.allocPrint(self.allocator, "{s}.ts", .{import_path});
                    const ts_path = try std.fs.path.join(self.allocator, &.{ file_dir, ts_name });
                    self.allocator.free(ts_name);
                    if (std.fs.cwd().access(ts_path, .{})) |_| {
                        break :blk ts_path;
                    } else |_| {
                        self.allocator.free(ts_path);
                    }
                    const js_name = try std.fmt.allocPrint(self.allocator, "{s}.js", .{import_path});
                    const js_path = try std.fs.path.join(self.allocator, &.{ file_dir, js_name });
                    self.allocator.free(js_name);
                    if (std.fs.cwd().access(js_path, .{})) |_| {
                        break :blk js_path;
                    } else |_| {
                        self.allocator.free(js_path);
                    }
                    const cot_name = try std.fmt.allocPrint(self.allocator, "{s}.cot", .{import_path});
                    defer self.allocator.free(cot_name);
                    break :blk try std.fs.path.join(self.allocator, &.{ file_dir, cot_name });
                };
                defer self.allocator.free(full_path);
                const canonical = try self.normalizePath(full_path);
                if (!canonical_path_to_module.contains(canonical)) {
                    const mod_name = try self.deriveModuleName(import_path);
                    try canonical_path_to_module.put(canonical, mod_name);
                } else {
                    self.allocator.free(canonical);
                }
            }
        }

        // Register module names for auto-imported async infrastructure (all targets).
        for (parsed_files.items) |*pf| {
            if (pf.tree.hasAsyncFunctions()) {
                const async_deps = [_][]const u8{ "std/task_queue", "std/executor" };
                const pf_dir = std.fs.path.dirname(pf.path) orelse ".";
                for (&async_deps) |dep| {
                    const dep_path = try self.resolveStdImport(dep, pf_dir);
                    defer self.allocator.free(dep_path);
                    const canonical = try self.normalizePath(dep_path);
                    if (!canonical_path_to_module.contains(canonical)) {
                        const mod_name = try self.deriveModuleName(dep);
                        try canonical_path_to_module.put(canonical, mod_name);
                    } else {
                        self.allocator.free(canonical);
                    }
                }
            }
        }

        // Build tree→module map for cross-module call resolution in lowerer
        var tree_module_map = std.AutoHashMap(*const ast_mod.Ast, []const u8).init(self.allocator);
        defer tree_module_map.deinit();
        for (parsed_files.items) |*pf| {
            if (canonical_path_to_module.get(pf.path)) |mod_name| {
                try tree_module_map.put(&pf.tree, mod_name);
            }
        }

        // Phase 3: Lower all files to IR with shared builder
        var shared_builder = ir_mod.Builder.init(self.allocator, &type_reg);
        defer shared_builder.deinit();

        // Share lowered_generics across files to prevent duplicate generic instantiations.
        // Same pattern as shared_builder — each Lowerer inherits and returns it.
        var shared_lowered_generics = std.StringHashMap(void).init(self.allocator);
        defer shared_lowered_generics.deinit();

        var all_test_names = std.ArrayListUnmanaged([]const u8){};
        defer all_test_names.deinit(self.allocator);
        var all_test_display_names = std.ArrayListUnmanaged([]const u8){};
        defer all_test_display_names.deinit(self.allocator);
        var all_bench_names = std.ArrayListUnmanaged([]const u8){};
        defer all_bench_names.deinit(self.allocator);
        var all_bench_display_names = std.ArrayListUnmanaged([]const u8){};
        defer all_bench_display_names.deinit(self.allocator);
        // Go init function pattern: track per-file init function names for multi-file builds.
        // Each file with globals gets __cot_init_file_N; master __cot_init_globals calls them all.
        var init_func_names = std.ArrayListUnmanaged([]const u8){};
        defer init_func_names.deinit(self.allocator);

        // Multi-file source map: track per-file paths/texts and func→file mapping.
        // Reference: Go runtime/symtab.go:funcfile() — per-CU file tables via cutab.
        var file_paths_list = std.ArrayListUnmanaged([]const u8){};
        defer file_paths_list.deinit(self.allocator);
        var file_texts_list = std.ArrayListUnmanaged([]const u8){};
        defer file_texts_list.deinit(self.allocator);
        var func_file_idx_list = std.ArrayListUnmanaged(u16){};
        defer func_file_idx_list.deinit(self.allocator);
        for (parsed_files.items) |*pf| {
            try file_paths_list.append(self.allocator, pf.path);
            try file_texts_list.append(self.allocator, pf.source_text);
        }

        // Pre-allocate metadata globals for all generic type args BEFORE lowering.
        // This ensures emitTypeMetadataRef can find the global during call site lowering.
        try self.preallocateTypeMetadataGlobals(&shared_builder, &type_reg, &generic_ctx);

        for (parsed_files.items, 0..) |*pf, i| {
            const func_count_before = shared_builder.funcs.items.len;

            var lower_err = errors_mod.ErrorReporter.init(&pf.source, null);
            var lowerer = lower_mod.Lowerer.initWithBuilder(self.allocator, &pf.tree, &type_reg, &lower_err, &checkers.items[i], shared_builder, self.target);
            lowerer.lowered_generics = shared_lowered_generics;
            lowerer.module_name = canonical_path_to_module.get(pf.path) orelse "";
            lowerer.tree_module_map = &tree_module_map;
            lowerer.release_mode = self.release_mode;
            if (self.test_mode) lowerer.setTestMode(true);
            if (self.fail_fast) lowerer.setFailFast(true);
            if (self.bench_mode) lowerer.setBenchMode(true);

            lowerer.lowerToBuilder() catch |e| {
                shared_lowered_generics = lowerer.lowered_generics;
                lowerer.deinitWithoutBuilder();
                return e;
            };
            // ARC Phase 4: Generate synthetic deinit functions for structs with ARC fields
            lowerer.emitPendingAutoDeinits() catch |e| {
                shared_lowered_generics = lowerer.lowered_generics;
                lowerer.deinitWithoutBuilder();
                return e;
            };
            if (lower_err.hasErrors()) {
                shared_lowered_generics = lowerer.lowered_generics;
                lowerer.deinitWithoutBuilder();
                return error.LowerError;
            }

            // Go init function pattern: generate per-file init function for this file's globals.
            if (lowerer.pending_global_inits.items.len > 0) {
                const init_name = try std.fmt.allocPrint(self.allocator, "__cot_init_file_{d}", .{i});
                lowerer.generateGlobalInitsNamed(init_name) catch |e| {
                    shared_lowered_generics = lowerer.lowered_generics;
                    lowerer.deinitWithoutBuilder();
                    return e;
                };
                try init_func_names.append(self.allocator, init_name);
            }

            if (self.test_mode) {
                for (lowerer.getTestNames()) |n| try all_test_names.append(self.allocator, n);
                for (lowerer.getTestDisplayNames()) |n| try all_test_display_names.append(self.allocator, n);
            }
            if (self.bench_mode) {
                for (lowerer.getBenchNames()) |n| try all_bench_names.append(self.allocator, n);
                for (lowerer.getBenchDisplayNames()) |n| try all_bench_display_names.append(self.allocator, n);
            }
            shared_builder = lowerer.builder;
            shared_lowered_generics = lowerer.lowered_generics;

            // Track func→file mapping: all functions added by this file's lowering
            const func_count_after = shared_builder.funcs.items.len;
            for (func_count_before..func_count_after) |_| {
                try func_file_idx_list.append(self.allocator, @intCast(i));
            }

            lowerer.deinitWithoutBuilder();
        }

        // Go init function pattern: generate master __cot_init_globals that calls all per-file inits.
        // Uses shared_builder directly (no Lowerer needed — just emitting call + ret IR nodes).
        {
            const span = source_mod.Span.init(source_mod.Pos.zero, source_mod.Pos.zero);
            shared_builder.startFunc("__cot_init_globals", types_mod.TypeRegistry.VOID, types_mod.TypeRegistry.VOID, span);
            if (shared_builder.func()) |fb| {
                // Install signal handlers first (before any user code)
                if (!self.target.isWasm()) {
                    var no_sig_args = [_]ir_mod.NodeIndex{};
                    _ = try fb.emitCall("__cot_install_signals", &no_sig_args, false, types_mod.TypeRegistry.VOID, span);
                }
                for (init_func_names.items) |init_name| {
                    var no_args = [_]ir_mod.NodeIndex{};
                    _ = try fb.emitCall(init_name, &no_args, false, types_mod.TypeRegistry.VOID, span);
                }
                _ = try fb.emitRet(null, span);
            }
            try shared_builder.endFunc();
        }

        // Filter tests if --filter is specified
        if (self.test_filter) |filter| {
            var filtered_names = std.ArrayListUnmanaged([]const u8){};
            defer filtered_names.deinit(self.allocator);
            var filtered_display = std.ArrayListUnmanaged([]const u8){};
            defer filtered_display.deinit(self.allocator);

            for (all_test_names.items, all_test_display_names.items) |name, display| {
                if (std.mem.indexOf(u8, display, filter) != null) {
                    try filtered_names.append(self.allocator, name);
                    try filtered_display.append(self.allocator, display);
                }
            }

            all_test_names.clearRetainingCapacity();
            all_test_display_names.clearRetainingCapacity();
            for (filtered_names.items) |n| try all_test_names.append(self.allocator, n);
            for (filtered_display.items) |n| try all_test_display_names.append(self.allocator, n);

            if (all_test_names.items.len == 0) {
                std.debug.print("0 tests matched filter \"{s}\"\n", .{filter});
                return error.NoTestsMatched;
            }
        }

        // Filter benchmarks if --filter is specified
        if (self.bench_filter) |filter| {
            var filtered_names = std.ArrayListUnmanaged([]const u8){};
            defer filtered_names.deinit(self.allocator);
            var filtered_display = std.ArrayListUnmanaged([]const u8){};
            defer filtered_display.deinit(self.allocator);

            for (all_bench_names.items, all_bench_display_names.items) |name, display| {
                if (std.mem.indexOf(u8, display, filter) != null) {
                    try filtered_names.append(self.allocator, name);
                    try filtered_display.append(self.allocator, display);
                }
            }

            all_bench_names.clearRetainingCapacity();
            all_bench_display_names.clearRetainingCapacity();
            for (filtered_names.items) |n| try all_bench_names.append(self.allocator, n);
            for (filtered_display.items) |n| try all_bench_display_names.append(self.allocator, n);

            if (all_bench_names.items.len == 0) {
                std.debug.print("0 benchmarks matched filter \"{s}\"\n", .{filter});
                return error.NoBenchesMatched;
            }
        }

        // Generate test runner if in test mode
        if (self.test_mode and all_test_names.items.len == 0) {
            std.debug.print("\nok | 0 passed\n", .{});
            return &.{};
        }
        if (self.test_mode and all_test_names.items.len > 0) {
            var dummy_err = errors_mod.ErrorReporter.init(&parsed_files.items[0].source, null);
            var runner = lower_mod.Lowerer.initWithBuilder(self.allocator, &parsed_files.items[0].tree, &type_reg, &dummy_err, &checkers.items[0], shared_builder, self.target);
            if (self.fail_fast) runner.setFailFast(true);
            for (all_test_names.items) |n| try runner.addTestName(n);
            for (all_test_display_names.items) |n| try runner.addTestDisplayName(n);
            try runner.generateTestRunner();
            shared_builder = runner.builder;
            runner.deinitWithoutBuilder();
        }

        // Generate bench runner if in bench mode
        if (self.bench_mode and all_bench_names.items.len > 0) {
            var dummy_err = errors_mod.ErrorReporter.init(&parsed_files.items[0].source, null);
            var runner = lower_mod.Lowerer.initWithBuilder(self.allocator, &parsed_files.items[0].tree, &type_reg, &dummy_err, &checkers.items[0], shared_builder, self.target);
            for (all_bench_names.items) |n| try runner.addBenchName(n);
            for (all_bench_display_names.items) |n| try runner.addBenchDisplayName(n);
            try runner.generateBenchRunner();
            shared_builder = runner.builder;
            runner.deinitWithoutBuilder();
        }

        try self.emitVWTWitnesses(&shared_builder, &type_reg);
        try self.emitTrivialTypeMetadata(&shared_builder, &type_reg, &generic_ctx);
        try self.emitProtocolWitnessTables(&shared_builder, &generic_ctx);
        try self.emitMetadataInitMaster(&shared_builder);

        var final_ir = try shared_builder.getIR();
        defer final_ir.deinit();

        // Multi-file source map: store per-file data for pctab encoder
        self.parsed_file_paths = file_paths_list.items;
        self.parsed_file_texts = file_texts_list.items;
        self.func_file_indices = func_file_idx_list.items;

        const main_file = if (parsed_files.items.len > 0) parsed_files.items[parsed_files.items.len - 1] else ParsedFile{
            .path = path,
            .source_text = "",
            .source = undefined,
            .tree = undefined,
        };
        return self.generateCode(final_ir.funcs, final_ir.globals, &type_reg, main_file.path, main_file.source_text);
    }

    /// Collect user-declared extern fn names and signatures from a checker scope.
    /// These are functions declared with `extern fn` in user code (e.g. sqlite3_open).
    /// They need func_index_map entries so the native backend can emit calls to them.
    /// For --target=js, param counts are used to generate Wasm import entries.
    /// Reference: cg_clif abi/mod.rs:97-115 — Linkage::Import pattern for external symbols
    fn collectExternFns(self: *Driver, scope: *const checker_mod.Scope, type_reg: *types_mod.TypeRegistry) void {
        var iter = scope.symbols.iterator();
        while (iter.next()) |entry| {
            const sym = entry.value_ptr.*;
            if (sym.is_extern and sym.kind == .function) {
                // Dupe name — sym.name points into parser arena which may be freed before JS glue generation
                self.user_extern_fns.append(self.allocator, self.allocator.dupe(u8, sym.name) catch sym.name) catch {};

                // Resolve param count and return type from the type registry.
                // Cot's Wasm ABI: all params are i64, strings decompose to (ptr, len).
                var param_count: u32 = 0;
                var has_result: bool = false;
                const ti = sym.type_idx;
                if (ti < type_reg.types.items.len) {
                    const t = type_reg.types.items[ti];
                    if (t == .func) {
                        // Count Wasm-level params: strings/slices = 2 params (ptr+len), everything else = 1
                        for (t.func.params) |p| {
                            const pt = p.type_idx;
                            if (pt < type_reg.types.items.len) {
                                const pt_info = type_reg.types.items[pt];
                                if (pt_info == .slice or pt == types_mod.TypeRegistry.STRING) {
                                    param_count += 2; // ptr + len
                                } else {
                                    param_count += 1;
                                }
                            } else {
                                param_count += 1;
                            }
                        }
                        has_result = t.func.return_type != types_mod.TypeRegistry.VOID;
                    }
                }
                self.user_extern_param_counts.append(self.allocator, param_count) catch {};
                self.user_extern_has_result.append(self.allocator, has_result) catch {};
            }
        }
    }

    fn normalizePath(self: *Driver, path: []const u8) ![]const u8 {
        return std.fs.cwd().realpathAlloc(self.allocator, path) catch try self.allocator.dupe(u8, path);
    }

    /// Go PathToPrefix pattern: derive module name from import path.
    /// "std/json" → "std.json", "frontend/parser" → "frontend.parser"
    /// Note: Go's PathToPrefix also escapes dots in the final segment (`.` → `%2e`)
    /// and special characters. Currently not needed since Cot import paths don't contain
    /// dots (file extensions are stripped). If import paths with dots are ever supported,
    /// add Go-style escaping to avoid ambiguity with the `module.funcName` separator.
    fn deriveModuleName(self: *Driver, import_path: []const u8) ![]const u8 {
        // Strip file extension if present
        const path = if (std.mem.endsWith(u8, import_path, ".cot"))
            import_path[0 .. import_path.len - 4]
        else if (std.mem.endsWith(u8, import_path, ".ts"))
            import_path[0 .. import_path.len - 3]
        else if (std.mem.endsWith(u8, import_path, ".js"))
            import_path[0 .. import_path.len - 3]
        else
            import_path;
        // Replace '/' with '.'
        const buf = try self.allocator.dupe(u8, path);
        for (buf) |*c| {
            if (c.* == '/') c.* = '.';
        }
        return buf;
    }

    /// Go resolver.go pattern: create a per-file scope and copy imported symbols.
    fn createFileScope(self: *Driver, pf: *ParsedFile, global_scope: *checker_mod.Scope, checked_scopes: *std.StringHashMap(CheckedFileEntry)) !*checker_mod.Scope {
        const file_scope = try self.allocator.create(checker_mod.Scope);
        file_scope.* = checker_mod.Scope.init(self.allocator, global_scope);

        // Copy direct imports' OWN symbols into this file's scope
        const imports = try pf.tree.getImports(self.allocator);
        defer self.allocator.free(imports);
        const file_dir = std.fs.path.dirname(pf.path) orelse ".";

        for (imports) |import_path| {
            // Resolve import path to canonical path (same logic as parseFileRecursive)
            const full_path = if (std.mem.startsWith(u8, import_path, "std/"))
                try self.resolveStdImport(import_path, file_dir)
            else blk: {
                const has_ext = std.mem.endsWith(u8, import_path, ".cot") or
                    std.mem.endsWith(u8, import_path, ".ts") or
                    std.mem.endsWith(u8, import_path, ".js");
                if (has_ext) {
                    break :blk try std.fs.path.join(self.allocator, &.{ file_dir, import_path });
                }
                // Try .ts first, then .js, then .cot (same order as parseFileRecursive)
                const ts_name = try std.fmt.allocPrint(self.allocator, "{s}.ts", .{import_path});
                const ts_path = try std.fs.path.join(self.allocator, &.{ file_dir, ts_name });
                self.allocator.free(ts_name);
                if (std.fs.cwd().access(ts_path, .{})) |_| {
                    break :blk ts_path;
                } else |_| {
                    self.allocator.free(ts_path);
                }
                const js_name = try std.fmt.allocPrint(self.allocator, "{s}.js", .{import_path});
                const js_path = try std.fs.path.join(self.allocator, &.{ file_dir, js_name });
                self.allocator.free(js_name);
                if (std.fs.cwd().access(js_path, .{})) |_| {
                    break :blk js_path;
                } else |_| {
                    self.allocator.free(js_path);
                }
                const cot_name = try std.fmt.allocPrint(self.allocator, "{s}.cot", .{import_path});
                defer self.allocator.free(cot_name);
                break :blk try std.fs.path.join(self.allocator, &.{ file_dir, cot_name });
            };
            defer self.allocator.free(full_path);
            const canonical = try self.normalizePath(full_path);
            defer self.allocator.free(canonical);

            // Find imported file's scope (already checked) and copy its OWN symbols
            if (checked_scopes.get(canonical)) |imported| {
                var it = imported.scope.symbols.iterator();
                while (it.next()) |entry| {
                    const sym = entry.value_ptr.*;
                    // Only copy symbols defined in the imported file (not its transitive imports)
                    if (sym.source_tree) |st| {
                        if (st == imported.tree) {
                            try file_scope.define(sym);
                        }
                    } else {
                        // No source_tree (e.g., stdlib extern fns) — copy all
                        try file_scope.define(sym);
                    }
                }
            }
        }

        return file_scope;
    }

    fn parseFileRecursive(self: *Driver, path: []const u8, parsed_files: *std.ArrayListUnmanaged(ParsedFile), seen_files: *std.StringHashMap(void), in_progress: *std.StringHashMap(void)) !void {
        const canonical_path = try self.normalizePath(path);
        defer self.allocator.free(canonical_path);

        // Circular import detection: file is on the recursion stack but not yet finished
        if (in_progress.contains(canonical_path)) {
            std.debug.print("error: circular import detected: {s}\n", .{canonical_path});
            return error.CircularImport;
        }

        if (seen_files.contains(canonical_path)) return;

        const path_copy = try self.allocator.dupe(u8, canonical_path);
        errdefer self.allocator.free(path_copy);
        try seen_files.put(path_copy, {});

        // Mark file as in-progress (on recursion stack)
        try in_progress.put(path_copy, {});

        const source_text = std.fs.cwd().readFileAlloc(self.allocator, canonical_path, 1024 * 1024) catch |e| {
            std.debug.print("Failed to read file: {s}: {any}\n", .{ canonical_path, e });
            return e;
        };
        errdefer self.allocator.free(source_text);

        const is_ts = isTypeScriptFile(canonical_path);

        var src = source_mod.Source.init(self.allocator, path_copy, source_text);
        errdefer src.deinit();

        var tree = ast_mod.Ast.init(self.allocator);
        errdefer tree.deinit();

        if (is_ts) {
            // TypeScript/JavaScript path: libts scanner → parser → transform → Cot AST
            var ts_src = libts.Source.init(self.allocator, path_copy, source_text);
            var ts_p = libts.Parser.init(&ts_src, self.allocator);
            ts_p.ensureInit();
            ts_p.parseSourceFile() catch {
                std.debug.print("TypeScript parse error: {s}\n", .{canonical_path});
                return error.ParseError;
            };

            // Transform TS AST → Cot AST
            try transformTsFile(&ts_p.ast, &tree, self.allocator, path_copy);
        } else {
            // Standard Cot path
            var err_reporter = errors_mod.ErrorReporter.init(&src, null);
            var scan = scanner_mod.Scanner.initWithErrors(&src, &err_reporter);
            var parser = parser_mod.Parser.init(self.allocator, &scan, &tree, &err_reporter);
            // Project-level @safe: set parser safe_mode BEFORE parsing so syntax features work
            if (self.isProjectSafe(canonical_path)) parser.safe_mode = true;
            try parser.parseFile();
            if (err_reporter.hasErrors()) return error.ParseError;
        }

        // Parse imports first (dependencies before dependents)
        const imports = try tree.getImports(self.allocator);
        defer self.allocator.free(imports);
        const file_dir = std.fs.path.dirname(canonical_path) orelse ".";
        for (imports) |import_path| {
            const full_path = if (std.mem.startsWith(u8, import_path, "std/"))
                try self.resolveStdImport(import_path, file_dir)
            else blk: {
                // Auto-append extension if not present
                const has_ext = std.mem.endsWith(u8, import_path, ".cot") or
                    std.mem.endsWith(u8, import_path, ".ts") or
                    std.mem.endsWith(u8, import_path, ".js");
                if (has_ext) {
                    break :blk try std.fs.path.join(self.allocator, &.{ file_dir, import_path });
                }
                // Try .ts first (for TS projects), then .cot
                const ts_name = try std.fmt.allocPrint(self.allocator, "{s}.ts", .{import_path});
                const ts_path = try std.fs.path.join(self.allocator, &.{ file_dir, ts_name });
                self.allocator.free(ts_name);
                if (std.fs.cwd().access(ts_path, .{})) |_| {
                    break :blk ts_path;
                } else |_| {
                    self.allocator.free(ts_path);
                }
                // Try .js
                const js_name = try std.fmt.allocPrint(self.allocator, "{s}.js", .{import_path});
                const js_path = try std.fs.path.join(self.allocator, &.{ file_dir, js_name });
                self.allocator.free(js_name);
                if (std.fs.cwd().access(js_path, .{})) |_| {
                    break :blk js_path;
                } else |_| {
                    self.allocator.free(js_path);
                }
                // Fall back to .cot
                const cot_name = try std.fmt.allocPrint(self.allocator, "{s}.cot", .{import_path});
                defer self.allocator.free(cot_name);
                break :blk try std.fs.path.join(self.allocator, &.{ file_dir, cot_name });
            };
            defer self.allocator.free(full_path);
            try self.parseFileRecursive(full_path, parsed_files, seen_files, in_progress);
        }

        // Auto-import cooperative executor for async files (all targets).
        // Await sites call run_until_task_done() for cooperative scheduling
        // (Rust poll model: coroutine.rs block_on pattern).
        if (tree.hasAsyncFunctions()) {
            const async_deps = [_][]const u8{ "std/task_queue", "std/executor" };
            for (&async_deps) |dep| {
                const dep_path = try self.resolveStdImport(dep, file_dir);
                defer self.allocator.free(dep_path);
                try self.parseFileRecursive(dep_path, parsed_files, seen_files, in_progress);
            }
        }

        // Remove from in-progress stack (file fully processed)
        _ = in_progress.remove(canonical_path);

        try parsed_files.append(self.allocator, .{ .path = path_copy, .source_text = source_text, .source = src, .tree = tree });
    }

    /// Resolve stdlib imports: "std/list" → "<stdlib_dir>/list.cot"
    /// Discovery order (like Zig's findZigLibDirFromSelfExe):
    /// 1. COT_STDLIB env var (explicit override)
    /// 2. Walk up from source file looking for stdlib/ directory
    /// 3. Relative to compiler binary (<exe_dir>/../lib/std/)
    /// 4. CWD fallback (./stdlib/)
    fn resolveStdImport(self: *Driver, import_path: []const u8, source_dir: []const u8) ![]const u8 {
        const module = import_path["std/".len..];
        const filename = try std.fmt.allocPrint(self.allocator, "{s}.cot", .{module});
        defer self.allocator.free(filename);

        // Tier 1: COT_STDLIB env var
        if (std.posix.getenv("COT_STDLIB")) |stdlib_dir| {
            return std.fs.path.join(self.allocator, &.{ stdlib_dir, filename });
        }

        // Tier 2: Walk up from source directory looking for stdlib/
        var dir: []const u8 = source_dir;
        while (true) {
            const candidate = try std.fs.path.join(self.allocator, &.{ dir, "stdlib", filename });
            if (std.fs.cwd().access(candidate, .{})) |_| {
                return candidate;
            } else |_| {}
            self.allocator.free(candidate);
            dir = std.fs.path.dirname(dir) orelse break;
        }

        // Tier 3: Relative to compiler binary (Zig findZigLibDirFromSelfExe pattern)
        // Installed layout: ~/.cot/bin/cot + ~/.cot/lib/std/*.cot
        var exe_dir_buf: [std.fs.max_path_bytes]u8 = undefined;
        if (std.fs.selfExeDirPath(&exe_dir_buf)) |exe_dir| {
            const candidate = try std.fs.path.join(self.allocator, &.{ exe_dir, "..", "lib", "std", filename });
            if (std.fs.cwd().access(candidate, .{})) |_| {
                return candidate;
            } else |_| {}
            self.allocator.free(candidate);
        } else |_| {}

        // Tier 4: CWD fallback
        return std.fs.path.join(self.allocator, &.{ "stdlib", filename });
    }

    /// Unified code generation for all architectures.
    fn generateCode(self: *Driver, funcs: []const ir_mod.Func, globals: []const ir_mod.Global, type_reg: *types_mod.TypeRegistry, source_file: []const u8, source_text: []const u8) ![]u8 {

        // Wasm target: use Wasm codegen pipeline
        if (self.target.isWasm()) {
            return self.generateWasmCode(funcs, globals, type_reg);
        }

        // Store source info for DWARF debug generation in generateMachO/generateElf
        self.debug_source_file = source_file;
        self.debug_source_text = source_text;
        self.debug_ir_funcs = funcs;
        self.debug_type_reg = type_reg;

        // Native path: SSA → CIR → libclif (rust or zig) → .o
        return self.generateNativeCodeViaCIR(funcs, globals, type_reg);
    }

    // OLD generateNativeCode + generateMachODirect + generateDataSectionObj DELETED
    // All native codegen now goes through CIR → libclif (rust or zig)

    /// Native codegen: SSA → CIR → libclif → .o
    /// libclif is either rust/libclif (--backend=cranelift) or zig/libclif (--backend=zig).
    /// Both implement the same C ABI (clif_compile) and consume CIR bytes.
    fn generateNativeCodeViaCIR(self: *Driver, funcs: []const ir_mod.Func, globals: []const ir_mod.Global, type_reg: *types_mod.TypeRegistry) ![]u8 {
        const ssa_to_cir = @import("codegen/ssa_to_cir.zig");
        const CirWriter = ssa_to_cir.CirWriter;
        const libclif = @import("codegen/libclif.zig");
        const arc_runtime_cir = @import("codegen/arc_runtime.zig");
        const io_runtime_cir = @import("codegen/io_runtime.zig");
        const print_runtime_cir = @import("codegen/print_runtime_native.zig");
        const test_runtime_cir = @import("codegen/test_runtime_native.zig");
        const signal_runtime_cir = @import("codegen/signal_runtime.zig");

        debug.log(.codegen, "driver: CIR native path for {d} functions", .{funcs.len});

        // --- String collection + func_index_map (identical to generateNativeCode) ---
        var string_offsets = std.StringHashMap(i32).init(self.allocator);
        defer string_offsets.deinit();
        var string_data = std.ArrayListUnmanaged(u8){};
        defer string_data.deinit(self.allocator);

        for (funcs) |*ir_func| {
            for (ir_func.string_literals) |str| {
                if (!string_offsets.contains(str)) {
                    const offset: i32 = @intCast(string_data.items.len);
                    try string_offsets.put(str, offset);
                    try string_data.appendSlice(self.allocator, str);
                    const padding = (8 - (str.len % 8)) % 8;
                    for (0..padding) |_| try string_data.append(self.allocator, 0);
                }
            }
        }

        var func_index_map = std.StringHashMapUnmanaged(u32){};
        defer func_index_map.deinit(self.allocator);
        for (funcs, 0..) |*ir_func, idx| {
            try func_index_map.put(self.allocator, ir_func.name, @intCast(idx));
        }

        // Register runtime function names — ssa_to_cir needs these to resolve call targets.
        const runtime_func_names = [_][]const u8{
            "alloc",         "dealloc",
            "alloc_raw",     "realloc_raw",   "dealloc_raw",
            "retain",        "release",
            "cot_realloc",   "string_concat",  "string_eq",
            "unowned_retain", "unowned_release", "unowned_load_strong",
            "weak_form_reference", "weak_retain", "weak_release", "weak_load_strong",
            "fd_write",      "fd_read",        "fd_close",      "exit",
            "fd_seek",       "memset_zero",    "fd_open",       "time",
            "random",        "growslice",      "nextslicecap",
            "args_count",    "arg_len",        "arg_ptr",
            "environ_count", "environ_len",    "environ_ptr",
            "cot_mkdir",     "dir_open",       "dir_next",      "dir_close",
            "stat_type",     "cot_unlink",
            "net_socket",    "net_bind",       "net_listen",
            "net_accept",    "net_connect",    "net_set_reuse_addr",
            "set_nonblocking", "poll_read",
            "kqueue_create", "kevent_add",     "kevent_del",    "kevent_wait",
            "epoll_create",  "epoll_add",      "epoll_del",     "epoll_wait",
            "cot_waitpid",   "cot_pipe",
            "cot_openpty",   "cot_ioctl_winsize", "cot_ioctl_set_ctty",
            "print_int",     "eprint_int",       "int_to_string",
            "print_float",   "eprint_float",     "float_to_string",
            "__cot_print_hex", "__cot_signal_handler", "__cot_install_signals", "__cot_print_backtrace", "__cot_print_source_loc",
            "__test_begin",  "__test_print_name", "__test_pass",
            "__test_fail",   "__test_summary",    "__test_store_fail_values",
            "snprintf",
            "write",         "malloc",         "free",          "realloc",   "memset",
            "memcmp",        "memcpy",         "read",          "close",
            "__open",        "lseek",          "_exit",         "gettimeofday",
            "getentropy",    "isatty",         "strlen",        "__error",
            "socket",        "bind",           "listen",        "accept",
            "connect",       "setsockopt",     "kqueue",        "kevent",
            "fcntl",         "fork",           "c_waitpid",     "c_pipe",
            "dup2",          "execve",        "kill",
            "c_openpty",     "setsid",        "ioctl",
            "c_mkdir",       "opendir",       "readdir",       "closedir",
            "c_stat",        "c_unlink",      "signal",
            "sigaction",     "sigaltstack",
            "backtrace",     "backtrace_symbols_fd",
            "dladdr",        "poll",           "usleep",
        };
        const runtime_start_idx: u32 = @intCast(funcs.len);
        for (runtime_func_names, 0..) |name, i| {
            if (!func_index_map.contains(name)) {
                try func_index_map.put(self.allocator, name, runtime_start_idx + @as(u32, @intCast(i)));
            }
        }
        // Aliases
        if (func_index_map.get("cot_waitpid")) |idx| try func_index_map.put(self.allocator, "waitpid", idx);
        if (func_index_map.get("cot_pipe")) |idx| try func_index_map.put(self.allocator, "pipe", idx);
        if (func_index_map.get("cot_openpty")) |idx| try func_index_map.put(self.allocator, "openpty", idx);
        if (func_index_map.get("cot_ioctl_winsize")) |idx| try func_index_map.put(self.allocator, "ioctl_winsize", idx);
        if (func_index_map.get("cot_ioctl_set_ctty")) |idx| try func_index_map.put(self.allocator, "ioctl_set_ctty", idx);
        if (func_index_map.get("cot_mkdir")) |idx| try func_index_map.put(self.allocator, "mkdir", idx);
        if (func_index_map.get("cot_unlink")) |idx| try func_index_map.put(self.allocator, "unlink", idx);
        if (func_index_map.get("cot_realloc")) |idx| try func_index_map.put(self.allocator, "realloc", idx);

        var user_extern_count: u32 = 0;
        for (self.user_extern_fns.items) |name| {
            if (!func_index_map.contains(name)) {
                try func_index_map.put(self.allocator, name, runtime_start_idx + @as(u32, @intCast(runtime_func_names.len)) + user_extern_count);
                user_extern_count += 1;
            }
        }

        const string_data_symbol_idx: ?u32 = if (string_data.items.len > 0)
            runtime_start_idx + @as(u32, @intCast(runtime_func_names.len)) + user_extern_count
        else
            null;
        const ctxt_base_idx = runtime_start_idx + @as(u32, @intCast(runtime_func_names.len)) + user_extern_count + @as(u32, if (string_data.items.len > 0) 1 else 0);
        const ctxt_symbol_idx: u32 = ctxt_base_idx;

        var global_symbol_map = std.StringHashMapUnmanaged(u32){};
        defer global_symbol_map.deinit(self.allocator);
        const globals_base_idx: u32 = ctxt_symbol_idx + 9; // skip argc/argv/envp/pctab/functab/etc
        for (globals, 0..) |g, i| {
            try global_symbol_map.put(self.allocator, g.name, globals_base_idx + @as(u32, @intCast(i)));
        }

        // Register data symbols in func_index_map for call/symbol resolution
        if (string_data_symbol_idx) |idx| {
            try func_index_map.put(self.allocator, "cot_string_data", idx);
        }
        try func_index_map.put(self.allocator, "cot_ctxt", ctxt_symbol_idx);
        {
            const ctxt_base = ctxt_symbol_idx;
            try func_index_map.put(self.allocator, "cot_argc", ctxt_base + 1);
            try func_index_map.put(self.allocator, "cot_argv", ctxt_base + 2);
            try func_index_map.put(self.allocator, "cot_envp", ctxt_base + 3);
            try func_index_map.put(self.allocator, "cot_pctab", ctxt_base + 4);
            try func_index_map.put(self.allocator, "cot_functab", ctxt_base + 5);
            try func_index_map.put(self.allocator, "cot_functab_count", ctxt_base + 6);
            try func_index_map.put(self.allocator, "cot_filetab", ctxt_base + 7);
            try func_index_map.put(self.allocator, "cot_funcnames", ctxt_base + 8);
        }
        for (globals, 0..) |g, i| {
            if (!func_index_map.contains(g.name)) {
                try func_index_map.put(self.allocator, g.name, globals_base_idx + @as(u32, @intCast(i)));
            }
        }

        // Build reverse map: index → name (needed by ssa_to_cir for call target resolution)
        // Prefer cot_ prefixed names over aliases to avoid CIR name collisions
        // (e.g., "mkdir" alias should resolve to "cot_mkdir", not "mkdir")
        var reverse_map = std.AutoHashMapUnmanaged(u32, []const u8){};
        defer reverse_map.deinit(self.allocator);
        {
            // First pass: add all names
            var it = func_index_map.iterator();
            while (it.next()) |entry| {
                reverse_map.put(self.allocator, entry.value_ptr.*, entry.key_ptr.*) catch {};
            }
            // Second pass: overwrite with cot_ prefixed names (canonical)
            it = func_index_map.iterator();
            while (it.next()) |entry| {
                if (std.mem.startsWith(u8, entry.key_ptr.*, "cot_")) {
                    reverse_map.put(self.allocator, entry.value_ptr.*, entry.key_ptr.*) catch {};
                }
            }
        }
        {
            var it = global_symbol_map.iterator();
            while (it.next()) |entry| {
                reverse_map.put(self.allocator, entry.value_ptr.*, entry.key_ptr.*) catch {};
            }
        }

        // --- Create ONE CirWriter for everything (user + runtime functions) ---
        var writer = CirWriter.init(self.allocator);
        defer writer.deinit();

        // --- Phase 1: For each user function, build SSA → passes → CIR directly ---
        for (funcs) |*ir_func| {
            debug.log(.codegen, "driver: CIR compiling '{s}'", .{ir_func.name});

            // Per-function arena
            var func_arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
            defer func_arena.deinit();
            const func_alloc = func_arena.allocator();

            // Build SSA from IR
            var ssa_builder = try ssa_builder_mod.SSABuilder.init(func_alloc, ir_func, globals, type_reg, self.target);
            const ssa_func = try ssa_builder.build();
            ssa_builder.deinit();
            ssa_func.type_idx = ir_func.type_idx;

            // Run SSA passes
            try rewritegeneric.rewrite(func_alloc, ssa_func, &string_offsets);
            try decompose_builtin.decompose(func_alloc, ssa_func, type_reg);
            try rewritedec.rewrite(func_alloc, ssa_func);
            try deadcode.removeUnreachableBlocksOnly(ssa_func);
            try schedule.schedule(ssa_func);
            try layout.layout(ssa_func);
            try lower_native.lower(ssa_func);

            // Translate SSA → CIR directly (no intermediate CLIF IR)
            ssa_to_cir.translateFunc(
                self.allocator,
                ssa_func,
                &writer,
                type_reg,
                ir_func.params,
                ir_func.return_type,
                &func_index_map,
                funcs,
                string_data_symbol_idx,
                ctxt_symbol_idx,
                &global_symbol_map,
                &reverse_map,
            ) catch |e| {
                debug.log(.codegen, "driver: SSA→CIR error for '{s}': {any}", .{ ir_func.name, e });
                return error.SsaToCirError;
            };
        }

        // --- Phase 2: Append runtime functions as CIR ---
        const rt_argc_symbol_idx: u32 = ctxt_symbol_idx + 1;
        const rt_argv_symbol_idx: u32 = ctxt_symbol_idx + 2;
        const rt_envp_symbol_idx: u32 = ctxt_symbol_idx + 3;

        arc_runtime_cir.generate(&writer);
        io_runtime_cir.generate(&writer, rt_argc_symbol_idx, rt_argv_symbol_idx, rt_envp_symbol_idx, self.lib_mode, switch (self.target.os) {
            .macos => .macos,
            .linux => .linux,
            else => .macos,
        });
        print_runtime_cir.generate(&writer);
        signal_runtime_cir.generate(&writer);
        if (self.test_mode) test_runtime_cir.generate(&writer);

        // --- Phase 3: Emit data definitions into CIR ---
        // String data blob
        if (string_data.items.len > 0) {
            writer.defineData("cot_string_data", string_data.items, false, true);
        }
        // Context, argc, argv, envp globals (8 bytes each, zero-init, writable)
        writer.defineZeroData("cot_ctxt", 8, true, true);
        writer.defineZeroData("cot_argc", 8, true, true);
        writer.defineZeroData("cot_argv", 8, true, true);
        writer.defineZeroData("cot_envp", 8, true, true);
        // Source map placeholders
        writer.defineZeroData("cot_pctab", 8, false, true);
        writer.defineZeroData("cot_functab", 8, false, true);
        writer.defineZeroData("cot_functab_count", 4, false, true);
        writer.defineZeroData("cot_filetab", 8, false, true);
        writer.defineZeroData("cot_funcnames", 8, false, true);
        // User globals
        for (globals) |g| {
            const size = @max(g.size, 8);
            const zeroes = try self.allocator.alloc(u8, size);
            defer self.allocator.free(zeroes);
            @memset(zeroes, 0);
            writer.defineData(g.name, zeroes, true, true);
        }

        // --- Phase 3b: Entry wrapper as CIR function ---
        // __cot_entry(argc, argv, envp) → i64: stores globals, calls _cot_main, returns 0
        if (!self.lib_mode) {
            var main_returns_void: bool = false;
            for (funcs) |*ir_func| {
                if (std.mem.eql(u8, ir_func.name, "main")) {
                    main_returns_void = (ir_func.return_type == 12);
                    break;
                }
            }
            const OP_ARG: u16 = 0x0092;
            const OP_CONST_INT: u16 = 0x0001;
            const OP_STORE: u16 = 0x0071;
            const OP_GLOBAL_VALUE: u16 = 0x0081;
            const OP_GLOBAL_VALUE_SYMBOL: u16 = 0x00B1;
            const OP_STATIC_CALL: u16 = 0x0093;
            const OP_RET: u16 = 0x0097;
            const CIR_I64: u32 = ssa_to_cir.CIR_I64;

            const entry_name = writer.internString("__cot_entry");
            const argc_name = writer.internString("cot_argc");
            const argv_name = writer.internString("cot_argv");
            const envp_name = writer.internString("cot_envp");
            const main_name = writer.internString("_cot_main");

            writer.beginFuncWithSig(entry_name, &.{ CIR_I64, CIR_I64, CIR_I64 }, &.{CIR_I64}, 1, 0x02); // export
            writer.beginBlock(0, 0, &.{}, &.{});

            // args
            writer.emit(OP_ARG, &.{ 100, CIR_I64, 0 }); // argc
            writer.emit(OP_ARG, &.{ 101, CIR_I64, 1 }); // argv
            writer.emit(OP_ARG, &.{ 102, CIR_I64, 2 }); // envp

            // global symbols
            writer.emit(OP_GLOBAL_VALUE_SYMBOL, &.{ 0, argc_name, 0, 0, 1 });
            writer.emit(OP_GLOBAL_VALUE, &.{ 103, CIR_I64, 0 });
            writer.emit(OP_STORE, &.{ CIR_I64, 103, 100 }); // *cot_argc = argc

            writer.emit(OP_GLOBAL_VALUE_SYMBOL, &.{ 1, argv_name, 0, 0, 1 });
            writer.emit(OP_GLOBAL_VALUE, &.{ 104, CIR_I64, 1 });
            writer.emit(OP_STORE, &.{ CIR_I64, 104, 101 }); // *cot_argv = argv

            writer.emit(OP_GLOBAL_VALUE_SYMBOL, &.{ 2, envp_name, 0, 0, 1 });
            writer.emit(OP_GLOBAL_VALUE, &.{ 105, CIR_I64, 2 });
            writer.emit(OP_STORE, &.{ CIR_I64, 105, 102 }); // *cot_envp = envp

            // call _cot_main()
            if (main_returns_void) {
                writer.emit(OP_STATIC_CALL, &.{ 0, main_name, 0 }); // no result, no args
                writer.emit(OP_CONST_INT, &.{ 106, CIR_I64, 0, 0 }); // return 0
                writer.emit(OP_RET, &.{106});
            } else {
                writer.emit(OP_STATIC_CALL, &.{ 1, 107, CIR_I64, main_name, 0 }); // 1 result
                writer.emit(OP_RET, &.{107});
            }

            writer.endBlock();
            writer.endFunc();
        }

        // --- Phase 4: Serialize CIR and compile via libclif → single .o ---
        const cir_bytes = writer.finish();
        debug.log(.codegen, "driver: CIR serialized ({d} bytes, {d} user + runtime functions)", .{ cir_bytes.len, funcs.len });

        const target_str: []const u8 = switch (self.target.os) {
            .macos => "arm64-macos",
            .linux => "x64-linux",
            else => "native",
        };
        const obj_bytes = try libclif.compileToObject(cir_bytes, target_str);
        debug.log(.codegen, "driver: libclif produced .o ({d} bytes)", .{obj_bytes.len});

        const result = try self.allocator.alloc(u8, obj_bytes.len);
        @memcpy(result, obj_bytes);
        libclif.freeObjectBytes(obj_bytes);

        return result;
    }

    /// Generate data-section .o for CIR path.
    /// Contains: entry wrapper, data sections (string data, ctxt, globals), external name declarations.
    /// All user and runtime functions are in the CIR .o (compiled by libclif/Cranelift).
    fn generateWasmCode(self: *Driver, funcs: []const ir_mod.Func, globals: []const ir_mod.Global, type_reg: *types_mod.TypeRegistry) ![]u8 {
        debug.log(.codegen, "driver: generating Wasm for {d} functions", .{funcs.len});

        var linker = wasm.Linker.init(self.allocator);
        defer linker.deinit();

        const is_wasm_gc = self.target.isWasmGC();

        // Configure memory (256 pages = 16MB minimum)
        // Pages 0-127: Stack (grows down from 8MB, matches OS thread default)
        // Page 128+: Heap (starts at 8MB / 0x800000)
        linker.setMemory(256, null);

        // ====================================================================
        // Add CTXT global (closure context pointer, Go: REG_CTXT = Global 1)
        // Must be before arc.addToLinker so heap_ptr shifts to global 2
        // Layout: SP(0), CTXT(1), heap_ptr(2)
        // ====================================================================
        _ = try linker.addGlobal(.{ .val_type = .i64, .mutable = true, .init_i64 = 0 });

        // ====================================================================
        // WasmGC: Register GC struct types in linker BEFORE runtime functions.
        // GC types get indices 0..N-1, shifting all function type indices by N.
        // This must happen before ARC/runtime function assembly so that
        // call_indirect type indices in those functions are correct.
        // ====================================================================
        if (is_wasm_gc) {
            const wasm_link = @import("codegen/wasm/link.zig");
            // First pass: register all struct/union base type names
            for (type_reg.types.items) |t| {
                if (t == .struct_type) {
                    const st = t.struct_type;
                    // Pre-register with empty fields to get type index
                    _ = try linker.addGcStructType(st.name, &.{});
                } else if (t == .union_type) {
                    const ut = t.union_type;
                    // Register empty base type for the union
                    _ = try linker.addGcStructType(ut.name, &.{});
                    // Register variant subtypes: __union_Name_variant
                    for (ut.variants) |v| {
                        const variant_name = try std.fmt.allocPrint(self.allocator, "__union_{s}_{s}", .{ ut.name, v.name });
                        _ = try linker.addGcStructType(variant_name, &.{});
                    }
                }
            }
            // Second pass: fill in field types (may reference other structs)
            // Compound fields (string=3 chunks, slice=3 chunks) are expanded to
            // multiple i64 GC fields so struct.new/get/set operate per-chunk.
            for (type_reg.types.items) |t| {
                if (t == .struct_type) {
                    const st = t.struct_type;
                    // Count total chunks across all fields
                    var total_chunks: usize = 0;
                    for (st.fields) |field| {
                        const field_info = type_reg.get(field.type_idx);
                        if (field_info == .struct_type) {
                            total_chunks += 1; // GC ref = 1 chunk
                        } else {
                            const field_size = type_reg.sizeOf(field.type_idx);
                            total_chunks += @max(1, (field_size + 7) / 8);
                        }
                    }
                    var gc_fields = try self.allocator.alloc(wasm_link.GcFieldType, total_chunks);
                    defer self.allocator.free(gc_fields);
                    var chunk_idx: usize = 0;
                    for (st.fields) |field| {
                        const is_float = field.type_idx == types_mod.TypeRegistry.F64 or
                            field.type_idx == types_mod.TypeRegistry.F32;
                        const field_info = type_reg.get(field.type_idx);
                        // Check if field is a struct type (or pointer to struct) → GC ref
                        const field_struct_name: ?[]const u8 = switch (field_info) {
                            .struct_type => |fst| fst.name,
                            .pointer => |ptr| switch (type_reg.get(ptr.elem)) {
                                .struct_type => |fst| fst.name,
                                else => null,
                            },
                            else => null,
                        };
                        const gc_ref: ?u32 = if (field_struct_name) |name|
                            linker.gc_struct_name_map.get(name)
                        else
                            null;
                        if (gc_ref != null) {
                            // Struct field: single GC ref
                            gc_fields[chunk_idx] = .{
                                .val_type = .i64,
                                .mutable = true,
                                .gc_ref = gc_ref,
                            };
                            chunk_idx += 1;
                        } else {
                            // Primitive or compound field: expand to i64 chunks
                            const field_size = type_reg.sizeOf(field.type_idx);
                            const num_chunks = @max(1, (field_size + 7) / 8);
                            for (0..num_chunks) |ci| {
                                gc_fields[chunk_idx] = .{
                                    .val_type = if (ci == 0 and is_float) .f64 else .i64,
                                    .mutable = true,
                                    .gc_ref = null,
                                };
                                chunk_idx += 1;
                            }
                        }
                    }
                    // Update the existing struct type's fields
                    const type_idx = linker.gc_struct_name_map.get(st.name).?;
                    const gs = &linker.gc_struct_types.items[type_idx];
                    gs.field_offset = @intCast(linker.gc_field_storage.items.len);
                    gs.field_count = @intCast(gc_fields.len);
                    try linker.gc_field_storage.appendSlice(self.allocator, gc_fields);
                }
            }

            // Third pass: fill in union variant subtype fields + set super_type
            for (type_reg.types.items) |t| {
                if (t == .union_type) {
                    const ut = t.union_type;
                    const base_idx = linker.gc_struct_name_map.get(ut.name) orelse continue;

                    for (ut.variants) |v| {
                        const variant_name = try std.fmt.allocPrint(self.allocator, "__union_{s}_{s}", .{ ut.name, v.name });
                        const variant_idx = linker.gc_struct_name_map.get(variant_name) orelse continue;

                        // Set super_type to base union type
                        linker.gc_struct_types.items[variant_idx].super_type = base_idx;

                        // Build fields for variant payload
                        if (v.payload_type == types_mod.TypeRegistry.VOID) {
                            // Unit variant: no fields (empty struct subtype)
                            continue;
                        }

                        const payload_info = type_reg.get(v.payload_type);
                        if (payload_info == .struct_type) {
                            // Struct payload: expand fields same as struct type registration
                            const pst = payload_info.struct_type;
                            var total_chunks: usize = 0;
                            for (pst.fields) |field| {
                                const fi = type_reg.get(field.type_idx);
                                if (fi == .struct_type) {
                                    total_chunks += 1;
                                } else {
                                    const fs = type_reg.sizeOf(field.type_idx);
                                    total_chunks += @max(1, (fs + 7) / 8);
                                }
                            }
                            var gc_fields = try self.allocator.alloc(wasm_link.GcFieldType, total_chunks);
                            defer self.allocator.free(gc_fields);
                            var ci: usize = 0;
                            for (pst.fields) |field| {
                                const is_float = field.type_idx == types_mod.TypeRegistry.F64 or
                                    field.type_idx == types_mod.TypeRegistry.F32;
                                const fi = type_reg.get(field.type_idx);
                                const field_struct_name: ?[]const u8 = switch (fi) {
                                    .struct_type => |fst| fst.name,
                                    .pointer => |ptr| switch (type_reg.get(ptr.elem)) {
                                        .struct_type => |fst| fst.name,
                                        else => null,
                                    },
                                    else => null,
                                };
                                const gc_ref: ?u32 = if (field_struct_name) |name|
                                    linker.gc_struct_name_map.get(name)
                                else
                                    null;
                                if (gc_ref != null) {
                                    gc_fields[ci] = .{ .val_type = .i64, .mutable = true, .gc_ref = gc_ref };
                                    ci += 1;
                                } else {
                                    const fs = type_reg.sizeOf(field.type_idx);
                                    const nc = @max(1, (fs + 7) / 8);
                                    for (0..nc) |cj| {
                                        gc_fields[ci] = .{
                                            .val_type = if (cj == 0 and is_float) .f64 else .i64,
                                            .mutable = true,
                                            .gc_ref = null,
                                        };
                                        ci += 1;
                                    }
                                }
                            }
                            const gv = &linker.gc_struct_types.items[variant_idx];
                            gv.field_offset = @intCast(linker.gc_field_storage.items.len);
                            gv.field_count = @intCast(gc_fields.len);
                            try linker.gc_field_storage.appendSlice(self.allocator, gc_fields);
                        } else {
                            // Primitive payload: single i64 field
                            const is_float = v.payload_type == types_mod.TypeRegistry.F64 or
                                v.payload_type == types_mod.TypeRegistry.F32;
                            const gc_field = wasm_link.GcFieldType{
                                .val_type = if (is_float) .f64 else .i64,
                                .mutable = true,
                                .gc_ref = null,
                            };
                            const gv = &linker.gc_struct_types.items[variant_idx];
                            gv.field_offset = @intCast(linker.gc_field_storage.items.len);
                            gv.field_count = 1;
                            try linker.gc_field_storage.append(self.allocator, gc_field);
                        }
                    }
                }
            }
        }

        // ====================================================================
        // Heap pointer global for linear memory allocation (slice growth, etc.)
        // WasmGC: no ARC, but slice_runtime still needs a heap pointer for
        // growslice (linear memory backing store for slices).
        // Layout: SP(0), CTXT(1), heap_ptr(2)
        // ====================================================================
        const heap_ptr_dynamic_idx = try linker.addGlobal(.{
            .val_type = .i32,
            .mutable = true,
            .init_i32 = @intCast(0x800000), // HEAP_START = 8MB
        });
        const heap_ptr_global = heap_ptr_dynamic_idx + 1; // Offset by SP

        // ====================================================================
        // Add WASI runtime functions FIRST (adds WASI host imports for Wasm targets)
        // Must be before other runtimes so import_count is known for index offsetting.
        // Reference: WASI preview1 fd_write
        // ====================================================================
        const wasi_funcs = try wasi_runtime.addToLinker(self.allocator, &linker, self.target);

        // ====================================================================
        // For --target=js (browser): register user extern fns as real Wasm imports
        // instead of stubs. These become import entries in the Wasm binary that
        // the JS host must provide. Module name "env" (standard convention).
        // Runtime functions (alloc, fd_write, etc.) remain as module stubs.
        // Reference: WASI import pattern in wasi_runtime.zig — same addType+addImport.
        // Reference: Go syscall/js — wasm_exec.js provides host functions.
        // ====================================================================
        if (!self.target.isWasi()) {
            for (self.user_extern_fns.items, 0..) |name, idx| {
                // Skip runtime functions — they're already handled as module stubs
                // by wasi_runtime, mem_runtime, print_runtime, etc.
                if (wasi_runtime.isRuntimeFunction(name)) continue;

                const param_count = self.user_extern_param_counts.items[idx];
                const has_result = self.user_extern_has_result.items[idx];

                // Create Wasm type: all params are i64 (Cot's Wasm calling convention)
                const wasm_constants = @import("codegen/wasm/constants.zig");
                var param_types_buf: [32]wasm_constants.ValType = undefined;
                const pc = @min(param_count, 32);
                for (0..pc) |i| {
                    param_types_buf[i] = .i64;
                }
                const result_types: []const wasm_constants.ValType = if (has_result) &.{.i64} else &.{};
                const type_idx = try linker.addType(param_types_buf[0..pc], result_types);

                _ = try linker.addImport(.{
                    .module = "env",
                    .name = name,
                    .type_idx = type_idx,
                });
            }
        }

        // On Wasm targets, WASI imports shift all module function indices.
        // All module function indices must be offset by import_count to get
        // the correct Wasm function index for call instructions.
        const import_count = linker.numImports();

        // ====================================================================
        // Add memory utility functions (memcpy, memset_zero, string_eq, string_concat)
        // These are non-ARC, needed by stdlib on both ARC and WasmGC paths.
        // ====================================================================
        const mem_funcs = try mem_runtime.addToLinker(self.allocator, &linker, heap_ptr_global);

        // ====================================================================
        // Add slice runtime functions (Go style)
        // Reference: Go runtime/slice.go
        // ====================================================================
        const slice_funcs = try slice_runtime.addToLinker(self.allocator, &linker, heap_ptr_global);

        // ====================================================================
        // Add print runtime functions (Go style)
        // On Wasm: write forwards to WASI fd_write_simple for real I/O.
        // On native: write is a stub, overridden with ARM64/x64 syscall.
        // Reference: Go runtime/print.go
        // ====================================================================
        const wasi_write_override: ?u32 = if (self.target.isWasm()) wasi_funcs.fd_write_simple_idx else null;
        const print_funcs = try print_runtime.addToLinker(self.allocator, &linker, import_count, wasi_write_override);

        // ====================================================================
        // Add test runtime functions (Zig test runner pattern)
        // Reference: Zig test runner output format
        // ====================================================================
        const test_funcs = try test_runtime.addToLinker(self.allocator, &linker, print_funcs.write_idx, print_funcs.eprint_int_idx, wasi_funcs.time_idx);

        // ====================================================================
        // Add bench runtime functions (Go testing.B calibration pattern)
        // ====================================================================
        const bench_funcs = try bench_runtime.addToLinker(self.allocator, &linker, print_funcs.write_idx, print_funcs.eprint_int_idx, wasi_funcs.time_idx, self.bench_n);

        // ====================================================================
        // Add executor runtime (call_indirect bridge for task polling)
        // Swift reference: ExecutorJob.runSynchronously(on:)
        // ====================================================================
        const executor_funcs = try executor_runtime.addToLinker(self.allocator, &linker);

        // Get actual count from linker - never hardcode (Go: len(hostImports))
        const runtime_func_count = linker.funcCount();

        // Set minimum table size of 1 for call_indirect (destructor calls)
        // Table entry 0 is reserved (null/no destructor)
        linker.setTableSize(1);

        // Build function name -> index mapping
        // Runtime functions come first, then user functions
        var func_indices = wasm.FuncIndexMap{};
        defer func_indices.deinit(self.allocator);

        // Add memory utility function names to index map
        // Module function indices are offset by import_count to get Wasm function indices
        try func_indices.put(self.allocator, mem_runtime.ALLOC_NAME, mem_funcs.alloc_idx + import_count);
        try func_indices.put(self.allocator, mem_runtime.DEALLOC_NAME, mem_funcs.dealloc_idx + import_count);
        try func_indices.put(self.allocator, mem_runtime.REALLOC_NAME, mem_funcs.realloc_idx + import_count);
        try func_indices.put(self.allocator, "realloc", mem_funcs.realloc_idx + import_count); // alias for user code
        try func_indices.put(self.allocator, mem_runtime.ALLOC_RAW_NAME, mem_funcs.alloc_raw_idx + import_count);
        try func_indices.put(self.allocator, mem_runtime.REALLOC_RAW_NAME, mem_funcs.realloc_raw_idx + import_count);
        try func_indices.put(self.allocator, mem_runtime.DEALLOC_RAW_NAME, mem_funcs.dealloc_raw_idx + import_count);
        try func_indices.put(self.allocator, mem_runtime.MEMCPY_NAME, mem_funcs.memcpy_idx + import_count);
        try func_indices.put(self.allocator, mem_runtime.MEMSET_ZERO_NAME, mem_funcs.memset_zero_idx + import_count);
        try func_indices.put(self.allocator, mem_runtime.STRING_EQ_NAME, mem_funcs.string_eq_idx + import_count);
        try func_indices.put(self.allocator, mem_runtime.STRING_CONCAT_NAME, mem_funcs.string_concat_idx + import_count);
        try func_indices.put(self.allocator, "memcmp", mem_funcs.memcmp_idx + import_count);
        try func_indices.put(self.allocator, "retain", mem_funcs.retain_noop_idx + import_count);
        try func_indices.put(self.allocator, "release", mem_funcs.release_noop_idx + import_count);

        // Add slice function names to index map (Go)
        try func_indices.put(self.allocator, slice_runtime.GROWSLICE_NAME, slice_funcs.growslice_idx + import_count);
        try func_indices.put(self.allocator, slice_runtime.NEXTSLICECAP_NAME, slice_funcs.nextslicecap_idx + import_count);

        // Add print function names to index map (Go)
        try func_indices.put(self.allocator, print_runtime.WRITE_NAME, print_funcs.write_idx);
        try func_indices.put(self.allocator, print_runtime.PRINT_INT_NAME, print_funcs.print_int_idx);
        try func_indices.put(self.allocator, print_runtime.EPRINT_INT_NAME, print_funcs.eprint_int_idx);
        try func_indices.put(self.allocator, print_runtime.INT_TO_STRING_NAME, print_funcs.int_to_string_idx);
        try func_indices.put(self.allocator, print_runtime.PRINT_FLOAT_NAME, print_funcs.print_float_idx);
        try func_indices.put(self.allocator, print_runtime.EPRINT_FLOAT_NAME, print_funcs.eprint_float_idx);
        try func_indices.put(self.allocator, print_runtime.FLOAT_TO_STRING_NAME, print_funcs.float_to_string_idx);

        // Add WASI function names to index map
        try func_indices.put(self.allocator, wasi_runtime.FD_WRITE_NAME, wasi_funcs.fd_write_idx);
        try func_indices.put(self.allocator, wasi_runtime.FD_WRITE_SIMPLE_NAME, wasi_funcs.fd_write_simple_idx);
        try func_indices.put(self.allocator, wasi_runtime.FD_READ_SIMPLE_NAME, wasi_funcs.fd_read_simple_idx);
        try func_indices.put(self.allocator, wasi_runtime.FD_CLOSE_NAME, wasi_funcs.fd_close_idx);
        try func_indices.put(self.allocator, wasi_runtime.FD_SEEK_NAME, wasi_funcs.fd_seek_idx);
        try func_indices.put(self.allocator, wasi_runtime.FD_OPEN_NAME, wasi_funcs.fd_open_idx);
        try func_indices.put(self.allocator, wasi_runtime.TIME_NAME, wasi_funcs.time_idx);
        try func_indices.put(self.allocator, wasi_runtime.RANDOM_NAME, wasi_funcs.random_idx);
        try func_indices.put(self.allocator, wasi_runtime.EXIT_NAME, wasi_funcs.exit_idx);
        try func_indices.put(self.allocator, wasi_runtime.ARGS_COUNT_NAME, wasi_funcs.args_count_idx);
        try func_indices.put(self.allocator, wasi_runtime.ARG_LEN_NAME, wasi_funcs.arg_len_idx);
        try func_indices.put(self.allocator, wasi_runtime.ARG_PTR_NAME, wasi_funcs.arg_ptr_idx);
        try func_indices.put(self.allocator, wasi_runtime.ENVIRON_COUNT_NAME, wasi_funcs.environ_count_idx);
        try func_indices.put(self.allocator, wasi_runtime.ENVIRON_LEN_NAME, wasi_funcs.environ_len_idx);
        try func_indices.put(self.allocator, wasi_runtime.ENVIRON_PTR_NAME, wasi_funcs.environ_ptr_idx);
        // Networking
        try func_indices.put(self.allocator, wasi_runtime.NET_SOCKET_NAME, wasi_funcs.net_socket_idx);
        try func_indices.put(self.allocator, wasi_runtime.NET_BIND_NAME, wasi_funcs.net_bind_idx);
        try func_indices.put(self.allocator, wasi_runtime.NET_LISTEN_NAME, wasi_funcs.net_listen_idx);
        try func_indices.put(self.allocator, wasi_runtime.NET_ACCEPT_NAME, wasi_funcs.net_accept_idx);
        try func_indices.put(self.allocator, wasi_runtime.NET_CONNECT_NAME, wasi_funcs.net_connect_idx);
        try func_indices.put(self.allocator, wasi_runtime.NET_SET_REUSE_ADDR_NAME, wasi_funcs.net_set_reuse_addr_idx);
        // Event loop (kqueue/epoll)
        try func_indices.put(self.allocator, wasi_runtime.KQUEUE_CREATE_NAME, wasi_funcs.kqueue_create_idx);
        try func_indices.put(self.allocator, wasi_runtime.KEVENT_ADD_NAME, wasi_funcs.kevent_add_idx);
        try func_indices.put(self.allocator, wasi_runtime.KEVENT_DEL_NAME, wasi_funcs.kevent_del_idx);
        try func_indices.put(self.allocator, wasi_runtime.KEVENT_WAIT_NAME, wasi_funcs.kevent_wait_idx);
        try func_indices.put(self.allocator, wasi_runtime.EPOLL_CREATE_NAME, wasi_funcs.epoll_create_idx);
        try func_indices.put(self.allocator, wasi_runtime.EPOLL_ADD_NAME, wasi_funcs.epoll_add_idx);
        try func_indices.put(self.allocator, wasi_runtime.EPOLL_DEL_NAME, wasi_funcs.epoll_del_idx);
        try func_indices.put(self.allocator, wasi_runtime.EPOLL_WAIT_NAME, wasi_funcs.epoll_wait_idx);
        try func_indices.put(self.allocator, wasi_runtime.SET_NONBLOCK_NAME, wasi_funcs.set_nonblocking_idx);
        // Process spawning
        try func_indices.put(self.allocator, wasi_runtime.FORK_NAME, wasi_funcs.fork_idx);
        try func_indices.put(self.allocator, wasi_runtime.EXECVE_NAME, wasi_funcs.execve_idx);
        try func_indices.put(self.allocator, wasi_runtime.WAITPID_NAME, wasi_funcs.waitpid_idx);
        try func_indices.put(self.allocator, wasi_runtime.PIPE_NAME, wasi_funcs.pipe_idx);
        try func_indices.put(self.allocator, wasi_runtime.DUP2_NAME, wasi_funcs.dup2_idx);
        try func_indices.put(self.allocator, wasi_runtime.ISATTY_NAME, wasi_funcs.isatty_idx);
        // Directory
        try func_indices.put(self.allocator, wasi_runtime.MKDIR_NAME, wasi_funcs.mkdir_idx);
        try func_indices.put(self.allocator, wasi_runtime.DIR_OPEN_NAME, wasi_funcs.dir_open_idx);
        try func_indices.put(self.allocator, wasi_runtime.DIR_NEXT_NAME, wasi_funcs.dir_next_idx);
        try func_indices.put(self.allocator, wasi_runtime.DIR_CLOSE_NAME, wasi_funcs.dir_close_idx);
        try func_indices.put(self.allocator, wasi_runtime.STAT_TYPE_NAME, wasi_funcs.stat_type_idx);
        try func_indices.put(self.allocator, wasi_runtime.UNLINK_NAME, wasi_funcs.unlink_idx);

        // Add test function names to index map (Zig)
        try func_indices.put(self.allocator, test_runtime.TEST_BEGIN_NAME, test_funcs.test_begin_idx + import_count);
        try func_indices.put(self.allocator, test_runtime.TEST_PRINT_NAME_NAME, test_funcs.test_print_name_idx + import_count);
        try func_indices.put(self.allocator, test_runtime.TEST_PASS_NAME, test_funcs.test_pass_idx + import_count);
        try func_indices.put(self.allocator, test_runtime.TEST_FAIL_NAME, test_funcs.test_fail_idx + import_count);
        try func_indices.put(self.allocator, test_runtime.TEST_SUMMARY_NAME, test_funcs.test_summary_idx + import_count);
        try func_indices.put(self.allocator, test_runtime.TEST_STORE_FAIL_VALUES_NAME, test_funcs.test_store_fail_values_idx + import_count);

        // Add bench function names to index map (Go testing.B)
        try func_indices.put(self.allocator, bench_runtime.BENCH_PRINT_NAME_NAME, bench_funcs.bench_print_name_idx + import_count);
        try func_indices.put(self.allocator, bench_runtime.BENCH_CALIBRATE_START_NAME, bench_funcs.bench_calibrate_start_idx + import_count);
        try func_indices.put(self.allocator, bench_runtime.BENCH_CALIBRATE_END_NAME, bench_funcs.bench_calibrate_end_idx + import_count);
        try func_indices.put(self.allocator, bench_runtime.BENCH_MEASURE_START_NAME, bench_funcs.bench_measure_start_idx + import_count);
        try func_indices.put(self.allocator, bench_runtime.BENCH_MEASURE_END_NAME, bench_funcs.bench_measure_end_idx + import_count);
        try func_indices.put(self.allocator, bench_runtime.BENCH_GET_N_NAME, bench_funcs.bench_get_n_idx + import_count);
        try func_indices.put(self.allocator, bench_runtime.BENCH_SUMMARY_NAME, bench_funcs.bench_summary_idx + import_count);

        // Add executor runtime function indices
        try func_indices.put(self.allocator, executor_runtime.POLL_TASK_NAME, executor_funcs.poll_task_idx + import_count);
        try func_indices.put(self.allocator, executor_runtime.RUN_UNTIL_COMPLETE_NAME, executor_funcs.run_until_complete_idx + import_count);
        // task_cancel/task_is_cancelled are stdlib Cot functions (not runtime)
        // — they compile from stdlib/sys.cot and work on both native and Wasm.

        // Add user function names (offset by runtime func count + import count)
        for (funcs, 0..) |*ir_func, i| {
            try func_indices.put(self.allocator, ir_func.name, @intCast(i + runtime_func_count + import_count));
        }

        // ====================================================================
        // Build destructor table: map type_name -> table index
        // Reference: Swift stores destructor pointer in type metadata
        // WasmGC: skip — GC handles object lifetime, no manual destructors
        // ====================================================================
        var destructor_table = std.StringHashMap(u32).init(self.allocator);
        defer destructor_table.deinit();

        // Metadata address map: type_name -> metadata memory address
        var metadata_addrs = std.StringHashMap(i32).init(self.allocator);
        defer metadata_addrs.deinit();

        // WasmGC: no destructor table needed — GC handles object lifetime
        // ARC destructors are native-only (handled in compileNative path)

        // ====================================================================
        // Build function table: ALL user functions get table entries
        // Go reference: link/internal/wasm/asm.go:476-494
        // Go uses: table[funcValueOffset + i] = function[i]
        // We use a direct map: func_name → table_idx (assigned by linker)
        // ====================================================================
        var func_table_indices = std.StringHashMap(u32).init(self.allocator);
        defer func_table_indices.deinit();

        for (funcs, 0..) |*ir_func, i| {
            const func_idx: u32 = @intCast(i + runtime_func_count);
            const table_idx = try linker.addTableFunc(func_idx);
            try func_table_indices.put(ir_func.name, table_idx);
        }

        // ====================================================================
        // Build function type index map: func_name → Wasm type section index
        // Go reference: wasmobj.go — type index from instruction's p.To.Offset
        // Pre-register all function types for call_indirect resolution
        // ====================================================================
        var func_type_indices = std.StringHashMap(u32).init(self.allocator);
        defer func_type_indices.deinit();

        if (!is_wasm_gc) {
            // Reserve offset 0 as null sentinel. cot_release checks
            // "if (metadata_ptr != 0)" to skip destructor lookup — so metadata must
            // never live at offset 0. This matches C's convention (address 0 = NULL).
            _ = try linker.addData(&[_]u8{ 0, 0, 0, 0, 0, 0, 0, 0 });

            // Generate metadata for each type with destructor
            // Metadata layout: type_id(4), size(4), destructor_ptr(4) = 12 bytes
            var metadata_buf: [12]u8 = undefined;

            var type_id: u32 = 1;
            var dtor_iter = destructor_table.iterator();
            while (dtor_iter.next()) |entry| {
                const dtor_idx = entry.value_ptr.*;

                // Build metadata bytes
                std.mem.writeInt(u32, metadata_buf[0..4], type_id, .little); // type_id
                std.mem.writeInt(u32, metadata_buf[4..8], 8, .little); // size (placeholder)
                std.mem.writeInt(u32, metadata_buf[8..12], dtor_idx, .little); // destructor table index

                const offset = try linker.addData(&metadata_buf);
                try metadata_addrs.put(entry.key_ptr.*, offset);

                debug.log(.codegen, "driver: metadata for {s} at offset {d}, dtor_idx={d}", .{ entry.key_ptr.*, offset, dtor_idx });
                type_id += 1;
            }
        }

        // ====================================================================
        // Pass 1: Collect all string literals and add to linker
        // ====================================================================
        var string_offsets = std.StringHashMap(i32).init(self.allocator);
        defer string_offsets.deinit();

        for (funcs) |*ir_func| {
            for (ir_func.string_literals) |str| {
                if (!string_offsets.contains(str)) {
                    const offset = try linker.addData(str);
                    try string_offsets.put(str, offset);
                    debug.log(.codegen, "driver: string literal at offset {d}: \"{s}\"", .{ offset, str });
                }
            }
        }

        // ====================================================================
        // Pass 2: Generate code for each function
        // ====================================================================
        for (funcs) |*ir_func| {
            // Per-function arena: all SSA data allocated here, freed at once after codegen
            var func_arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
            defer func_arena.deinit();
            const func_alloc = func_arena.allocator();

            // Build SSA
            var ssa_builder = try ssa_builder_mod.SSABuilder.init(func_alloc, ir_func, globals, type_reg, self.target);
            errdefer ssa_builder.deinit();

            const ssa_func = try ssa_builder.build();
            // No need for individual deinit — arena frees everything at once
            ssa_builder.deinit();

            // SSA text dump (Wasm path) — same as native path
            if (debug.isEnabled(.ssa)) {
                const ssa_dbg = @import("ssa/debug.zig");
                debug.log(.ssa, "\n=== SSA for '{s}' ({d} blocks) ===", .{ ir_func.name, ssa_func.blocks.items.len });
                for (ssa_func.blocks.items) |blk| {
                    debug.log(.ssa, "  Block b{d} ({s}, {d} values, succs={d}, preds={d}):", .{
                        blk.id, @tagName(blk.kind), blk.values.items.len, blk.succs.len, blk.preds.len,
                    });
                    for (blk.values.items) |val| {
                        var abuf: [256]u8 = undefined;
                        var alen: usize = 0;
                        for (val.args) |a| {
                            if (alen > 0) { abuf[alen] = ' '; alen += 1; }
                            const s = std.fmt.bufPrint(abuf[alen..], "v{d}", .{a.id}) catch break;
                            alen += s.len;
                        }
                        const type_name = type_reg.typeName(val.type_idx);
                        debug.log(.ssa, "    v{d}: {s} <{s}> {s} aux={d} uses={d}", .{
                            val.id, @tagName(val.op), type_name, abuf[0..alen], val.aux_int, val.uses,
                        });
                    }
                    if (blk.controls[0]) |c| {
                        if (blk.controls[1]) |c2| {
                            debug.log(.ssa, "    control: v{d} v{d}", .{ c.id, c2.id });
                        } else {
                            debug.log(.ssa, "    control: v{d}", .{c.id});
                        }
                    }
                }
                _ = ssa_dbg;
            }

            // COT_SSA: interactive HTML visualizer (Go GOSSAFUNC port)
            const ssa_html = @import("ssa/html.zig");
            var html_writer: ?ssa_html.HTMLWriter = null;
            if (self.ssa_html_func) |target_name| {
                if (std.mem.eql(u8, ir_func.name, target_name) or
                    std.mem.eql(u8, target_name, "*"))
                {
                    const html_path = std.fmt.allocPrint(self.allocator, "{s}.ssa.html", .{ir_func.name}) catch ir_func.name;
                    html_writer = ssa_html.HTMLWriter.init(self.allocator, ir_func.name, html_path, type_reg);

                    // Write source code column (if source text available)
                    if (self.parsed_file_texts.len > 0) {
                        const src_text = self.parsed_file_texts[0];
                        const src_path = if (self.parsed_file_paths.len > 0) self.parsed_file_paths[0] else "unknown";
                        html_writer.?.writeSources(src_text, src_path);
                    }

                    // Initial SSA state
                    html_writer.?.writePhase("start", "start", ssa_func);
                }
            }

            // Run Wasm-specific passes with optimization (Go pass order)
            // Reference: Go compile.go lines 58-145 — pass loop + HTMLWriter integration
            // Each pass is timed (Go: time.Now before/after, LogStat "TIME(ns)").
            const ssa_debug = @import("ssa/debug.zig");
            var t: i128 = undefined;
            var elapsed: i128 = undefined;

            // Async state machine splitting — must run FIRST, before other SSA passes.
            // Transforms async functions with multiple await points into state machines.
            // Rust reference: rustc_mir_transform/src/coroutine.rs (StateTransform)
            t = debug.timestamp();
            try async_split.asyncSplit(ssa_func, type_reg);
            elapsed = debug.timestamp() - t;
            debug.logTimed(.ssa, "  pass async_split", elapsed, .{});
            if (html_writer != null) html_writer.?.writePhase("async_split", "async_split", ssa_func);

            t = debug.timestamp();
            try copyelim.copyelim(ssa_func);
            elapsed = debug.timestamp() - t;
            debug.logTimed(.copyelim, "  pass copyelim", elapsed, .{});
            ssa_debug.checkFunc(ssa_func, "copyelim", func_alloc);
            if (html_writer != null) html_writer.?.writePhase("copyelim", "copyelim", ssa_func);

            t = debug.timestamp();
            try rewritegeneric.rewrite(func_alloc, ssa_func, &string_offsets);
            elapsed = debug.timestamp() - t;
            debug.logTimed(.codegen, "  pass rewritegeneric", elapsed, .{});
            ssa_debug.checkFunc(ssa_func, "rewritegeneric", func_alloc);
            if (html_writer != null) html_writer.?.writePhase("rewritegeneric", "rewritegeneric", ssa_func);

            t = debug.timestamp();
            try decompose_builtin.decompose(func_alloc, ssa_func, type_reg);
            elapsed = debug.timestamp() - t;
            debug.logTimed(.codegen, "  pass decompose", elapsed, .{});
            ssa_debug.checkFunc(ssa_func, "decompose", func_alloc);
            if (html_writer != null) html_writer.?.writePhase("decompose", "decompose", ssa_func);

            t = debug.timestamp();
            try rewritedec.rewrite(func_alloc, ssa_func);
            elapsed = debug.timestamp() - t;
            debug.logTimed(.codegen, "  pass rewritedec", elapsed, .{});
            ssa_debug.checkFunc(ssa_func, "rewritedec", func_alloc);
            if (html_writer != null) html_writer.?.writePhase("rewritedec", "rewritedec", ssa_func);

            t = debug.timestamp();
            try copyelim.copyelim(ssa_func);
            elapsed = debug.timestamp() - t;
            debug.logTimed(.copyelim, "  pass copyelim2", elapsed, .{});
            ssa_debug.checkFunc(ssa_func, "copyelim2", func_alloc);
            if (html_writer != null) html_writer.?.writePhase("copyelim2", "copyelim (2nd)", ssa_func);

            t = debug.timestamp();
            try cse_pass.cse(ssa_func);
            elapsed = debug.timestamp() - t;
            debug.logTimed(.codegen, "  pass cse", elapsed, .{});
            ssa_debug.checkFunc(ssa_func, "cse", func_alloc);
            if (html_writer != null) html_writer.?.writePhase("cse", "cse", ssa_func);

            t = debug.timestamp();
            try deadcode.deadcode(ssa_func);
            elapsed = debug.timestamp() - t;
            debug.logTimed(.deadcode, "  pass deadcode", elapsed, .{});
            ssa_debug.checkFunc(ssa_func, "deadcode", func_alloc);
            if (html_writer != null) html_writer.?.writePhase("deadcode", "deadcode", ssa_func);

            t = debug.timestamp();
            try schedule.schedule(ssa_func);
            elapsed = debug.timestamp() - t;
            debug.logTimed(.schedule, "  pass schedule", elapsed, .{});
            ssa_debug.checkFunc(ssa_func, "schedule", func_alloc);
            if (html_writer != null) html_writer.?.writePhase("schedule", "schedule", ssa_func);

            t = debug.timestamp();
            try layout.layout(ssa_func);
            elapsed = debug.timestamp() - t;
            debug.logTimed(.codegen, "  pass layout", elapsed, .{});
            ssa_debug.checkFunc(ssa_func, "layout", func_alloc);
            if (html_writer != null) html_writer.?.writePhase("layout", "layout", ssa_func);

            t = debug.timestamp();
            try lower_wasm.lower(ssa_func);
            elapsed = debug.timestamp() - t;
            debug.logTimed(.codegen, "  pass lower_wasm", elapsed, .{});
            ssa_debug.checkFunc(ssa_func, "lower_wasm", func_alloc);
            if (html_writer != null) {
                html_writer.?.writePhase("lower_wasm", "lower_wasm", ssa_func);
                html_writer.?.flushPhases(ssa_func);
                html_writer.?.close();
                html_writer.?.deinit();
                html_writer = null;
            }

            // Determine function signature
            var param_count: u32 = 0;
            for (ssa_func.blocks.items) |block| {
                for (block.values.items) |v| {
                    if (v.op == .arg) {
                        const arg_idx: u32 = @intCast(v.aux_int);
                        if (arg_idx >= param_count) param_count = arg_idx + 1;
                    }
                }
            }
            var params: [32]wasm.ValType = undefined;
            // WasmGC: parallel array for GC-aware param types (ref types for structs)
            var gc_params: [32]wasm.WasmType = undefined;
            var wasm_param_idx: usize = 0;
            {
                // Build Wasm param types, decomposing compound types (slice, string)
                // into 2 separate i64 entries (ptr, len). This must match the SSA
                // builder's arg decomposition in ssa_builder.zig.
                // WasmGC: no decomposition — structs are single (ref null $T) values.
                for (ir_func.params) |param| {
                    const param_type = type_reg.get(param.type_idx);
                    const is_string_or_slice = param.type_idx == types_mod.TypeRegistry.STRING or param_type == .slice;
                    const type_size = type_reg.sizeOf(param.type_idx);
                    const is_opt_ptr = param_type == .optional and blk: {
                        const ei = type_reg.get(param_type.optional.elem);
                        break :blk ei == .pointer and ei.pointer.managed;
                    };
                    const is_compound_opt = param_type == .optional and type_reg.get(param_type.optional.elem) != .pointer and !is_opt_ptr;
                    // Compound optionals are always linear memory (not GC refs), decompose on all targets.
                    // Structs/unions/tuples skip decomposition on WasmGC (they use GC refs).
                    const is_large_struct = (!is_wasm_gc and (param_type == .struct_type or param_type == .union_type or param_type == .tuple) and type_size > 8) or (is_compound_opt and type_size > 8);

                    // WasmGC: struct and managed pointer-to-struct params are single (ref null $typeidx)
                    // Raw pointers (@intToPtr) are i64 linear memory addresses, not GC refs.
                    // Reference: Kotlin/Dart WasmGC — struct refs in params, including self: *Type
                    const is_gc_struct_param = is_wasm_gc and (param_type == .struct_type or
                        (param_type == .pointer and param_type.pointer.managed and type_reg.get(param_type.pointer.elem) == .struct_type));
                    if (is_gc_struct_param) {
                        const st = if (param_type == .struct_type)
                            param_type.struct_type
                        else
                            type_reg.get(param_type.pointer.elem).struct_type;
                        const gc_type_idx = linker.gc_struct_name_map.get(st.name) orelse 0;
                        gc_params[wasm_param_idx] = wasm.WasmType.gcRefNull(gc_type_idx);
                        params[wasm_param_idx] = .i64; // placeholder for param_count
                        wasm_param_idx += 1;
                    } else if (is_opt_ptr or is_string_or_slice) {
                        // ?*T / String / Slice: 2 i64 params (tag+payload or ptr+len)
                        params[wasm_param_idx] = .i64;
                        gc_params[wasm_param_idx] = wasm.WasmType.fromVal(.i64);
                        wasm_param_idx += 1;
                        params[wasm_param_idx] = .i64;
                        gc_params[wasm_param_idx] = wasm.WasmType.fromVal(.i64);
                        wasm_param_idx += 1;
                    } else if (is_large_struct) {
                        // Large struct: N i64 params (one per 8-byte chunk)
                        const num_slots = (type_size + 7) / 8;
                        for (0..num_slots) |_| {
                            params[wasm_param_idx] = .i64;
                            gc_params[wasm_param_idx] = wasm.WasmType.fromVal(.i64);
                            wasm_param_idx += 1;
                        }
                    } else {
                        const is_float = param.type_idx == types_mod.TypeRegistry.F64 or
                            param.type_idx == types_mod.TypeRegistry.F32;
                        params[wasm_param_idx] = if (is_float) .f64 else .i64;
                        gc_params[wasm_param_idx] = wasm.WasmType.fromVal(if (is_float) .f64 else .i64);
                        wasm_param_idx += 1;
                    }
                }
            }
            const has_return = ir_func.return_type != types_mod.TypeRegistry.VOID;
            const ret_is_float = ir_func.return_type == types_mod.TypeRegistry.F64 or
                ir_func.return_type == types_mod.TypeRegistry.F32;
            // Decompose compound return types (string, slice) into 2 i64 values
            // to match the param decomposition pattern above.
            // WasmGC: no decomposition — compound returns are single ref values.
            const ret_type_info = type_reg.get(ir_func.return_type);
            const ret_is_opt_ptr = ret_type_info == .optional and blk: {
                const ei = type_reg.get(ret_type_info.optional.elem);
                break :blk ei == .pointer and ei.pointer.managed;
            };
            const ret_is_compound = ir_func.return_type == types_mod.TypeRegistry.STRING or ret_type_info == .slice or ret_is_opt_ptr;
            const results: []const wasm.ValType = if (!has_return)
                &[_]wasm.ValType{}
            else if (ret_is_compound)
                &[_]wasm.ValType{ .i64, .i64 }
            else if (ret_is_float)
                &[_]wasm.ValType{.f64}
            else
                &[_]wasm.ValType{.i64};


            // Add function type to linker
            // WasmGC: use addTypeWasm for GC-aware type registration (ref type params)
            const type_idx = if (is_wasm_gc) blk: {
                // Convert results to WasmType, handling GC ref returns
                var gc_results: [4]wasm.WasmType = undefined;
                var gc_results_len: usize = results.len;
                // WasmGC: struct returns become (ref null $T) result types.
                // Raw pointers (@intToPtr) return i64, not GC refs.
                const is_gc_struct_return = ret_type_info == .struct_type or
                    (ret_type_info == .pointer and ret_type_info.pointer.managed and type_reg.get(ret_type_info.pointer.elem) == .struct_type);
                if (has_return and is_gc_struct_return) {
                    const st = if (ret_type_info == .struct_type)
                        ret_type_info.struct_type
                    else
                        type_reg.get(ret_type_info.pointer.elem).struct_type;
                    const gc_type_idx_ret = linker.gc_struct_name_map.get(st.name) orelse 0;
                    gc_results[0] = wasm.WasmType.gcRefNull(gc_type_idx_ret);
                    gc_results_len = 1;
                } else {
                    for (results, 0..) |r, ri| gc_results[ri] = wasm.WasmType.fromVal(r);
                }
                break :blk try linker.addTypeWasm(gc_params[0..wasm_param_idx], gc_results[0..gc_results_len]);
            } else blk: {
                // Fill remaining params (e.g., state pointer for async poll functions)
                // SSA may have more args than ir_func.params due to async split.
                for (wasm_param_idx..param_count) |i| {
                    params[i] = .i64;
                    gc_params[i] = wasm.WasmType.fromVal(.i64);
                }
                if (wasm_param_idx < param_count) wasm_param_idx = param_count;
                break :blk try linker.addType(params[0..wasm_param_idx], results);
            };

            // Register this function's Wasm type index for call_indirect resolution
            try func_type_indices.put(ir_func.name, type_idx);

            // Post-pass: resolve call_indirect type indices on SSA values
            // Go: type index is stored in instruction by assembler (p.To.Offset)
            // We set aux_int on wasm_lowered_closure_call/inter_call values
            for (ssa_func.blocks.items) |block| {
                for (block.values.items) |sv| {
                    if (sv.op == .wasm_lowered_closure_call or sv.op == .wasm_lowered_inter_call) {
                        // Determine actual param count and return type from the call's args
                        const skip: usize = if (sv.op == .wasm_lowered_closure_call) 2 else 1;
                        const n_params: u32 = @intCast(sv.args.len - skip);
                        const has_res = sv.type_idx != types_mod.TypeRegistry.VOID and sv.type_idx != 0;

                        // Build Wasm type signature and register with linker
                        // WasmGC: use WasmType to support GC ref params (same as direct call path)
                        var gc_cp: [16]wasm.WasmType = undefined;
                        var cp: [16]wasm.ValType = undefined;
                        for (0..n_params) |pi| {
                            const arg_val = sv.args[skip + pi];
                            const is_f = arg_val.type_idx == types_mod.TypeRegistry.F64 or
                                arg_val.type_idx == types_mod.TypeRegistry.F32;
                            const arg_type_info = type_reg.get(arg_val.type_idx);
                            const is_gc_ref = is_wasm_gc and (arg_type_info == .struct_type or
                                (arg_type_info == .pointer and arg_type_info.pointer.managed and type_reg.get(arg_type_info.pointer.elem) == .struct_type));
                            if (is_gc_ref) {
                                const st = if (arg_type_info == .struct_type)
                                    arg_type_info.struct_type
                                else
                                    type_reg.get(arg_type_info.pointer.elem).struct_type;
                                const gc_idx = linker.gc_struct_name_map.get(st.name) orelse 0;
                                gc_cp[pi] = wasm.WasmType.gcRefNull(gc_idx);
                            } else {
                                gc_cp[pi] = wasm.WasmType.fromVal(if (is_f) .f64 else .i64);
                            }
                            cp[pi] = if (is_f) .f64 else .i64;
                        }
                        const ret_f = sv.type_idx == types_mod.TypeRegistry.F64 or
                            sv.type_idx == types_mod.TypeRegistry.F32;

                        const call_type_idx = if (is_wasm_gc) blk: {
                            // WasmGC: handle GC ref return types
                            var gc_res: [2]wasm.WasmType = undefined;
                            var gc_res_len: usize = 0;
                            if (has_res) {
                                const ci_ret_type_info = type_reg.get(sv.type_idx);
                                const is_gc_ret = ci_ret_type_info == .struct_type or
                                    (ci_ret_type_info == .pointer and ci_ret_type_info.pointer.managed and type_reg.get(ci_ret_type_info.pointer.elem) == .struct_type);
                                if (is_gc_ret) {
                                    const st = if (ci_ret_type_info == .struct_type)
                                        ci_ret_type_info.struct_type
                                    else
                                        type_reg.get(ci_ret_type_info.pointer.elem).struct_type;
                                    const gc_idx = linker.gc_struct_name_map.get(st.name) orelse 0;
                                    gc_res[0] = wasm.WasmType.gcRefNull(gc_idx);
                                } else {
                                    gc_res[0] = wasm.WasmType.fromVal(if (ret_f) .f64 else .i64);
                                }
                                gc_res_len = 1;
                            }
                            break :blk try linker.addTypeWasm(gc_cp[0..n_params], gc_res[0..gc_res_len]);
                        } else blk: {
                            const res: []const wasm.ValType = if (!has_res)
                                &[_]wasm.ValType{}
                            else if (ret_f)
                                &[_]wasm.ValType{.f64}
                            else
                                &[_]wasm.ValType{.i64};
                            break :blk try linker.addType(cp[0..n_params], res);
                        };
                        sv.aux_int = @intCast(linker.funcTypeIndex(call_type_idx));
                    }
                }
            }

            // Generate function body code using Go-style two-pass architecture
            const gc_name_map = if (is_wasm_gc) &linker.gc_struct_name_map else null;
            const gc_array_map = if (is_wasm_gc) &linker.gc_array_name_map else null;
            const gc_type_reg = if (is_wasm_gc) type_reg else null;
            const body = try wasm.generateFunc(self.allocator, ssa_func, &func_indices, &string_offsets, &metadata_addrs, &func_table_indices, gc_name_map, gc_array_map, gc_type_reg);
            errdefer self.allocator.free(body);

            // Export all functions for AOT compatibility
            // (AOT needs function names to resolve calls)
            const exported = true;

            // Add function to linker
            _ = try linker.addFunc(.{
                .name = ir_func.name,
                .type_idx = type_idx,
                .code = body, // Linker takes ownership
                .exported = exported,
            });
        }

        // Add _start wrapper for WASI compatibility (wasmtime expects _start, not main)
        // _start calls main and drops the return value (if any).
        if (self.target.isWasm()) {
            if (func_indices.get("main")) |main_wasm_idx| {
                const start_type = try linker.addType(&[_]wasm.ValType{}, &[_]wasm.ValType{});
                var start_code = wasm_old.CodeBuilder.init(self.allocator);
                defer start_code.deinit();
                try start_code.emitCall(main_wasm_idx);
                // If main returns a value, drop it (_start must return void)
                const main_returns_value = blk: {
                    for (funcs) |*ir_func| {
                        if (std.mem.eql(u8, ir_func.name, "main")) {
                            break :blk ir_func.return_type != types_mod.TypeRegistry.VOID;
                        }
                    }
                    break :blk false;
                };
                if (main_returns_value) {
                    try start_code.emitDrop();
                }
                const start_body = try start_code.finish();
                _ = try linker.addFunc(.{
                    .name = "_start",
                    .type_idx = start_type,
                    .code = start_body,
                    .exported = true,
                });
            }
        }

        // Emit Wasm binary using Go-style linker
        // This includes proper sections: type, function, memory, global (SP), export, code, data
        var output: std.ArrayListUnmanaged(u8) = .{};
        try linker.emit(&output);
        return output.toOwnedSlice(self.allocator);
    }
};

// ============================================================================
// TypeScript/JavaScript support
// ============================================================================

fn isTypeScriptFile(path: []const u8) bool {
    return std.mem.endsWith(u8, path, ".ts") or
        std.mem.endsWith(u8, path, ".js") or
        std.mem.endsWith(u8, path, ".tsx") or
        std.mem.endsWith(u8, path, ".jsx");
}

/// Transform a parsed TS AST into a Cot AST.
/// Produces Cot AST nodes that the checker and lowerer can process normally.
fn transformTsFile(ts: *const libts.ts_ast.Ast, cot: *ast_mod.Ast, allocator: std.mem.Allocator, filename: []const u8) error{OutOfMemory}!void {
    const sf = ts.source_file orelse return;

    var top_decls = std.ArrayListUnmanaged(ast_mod.NodeIndex){};
    var main_stmts = std.ArrayListUnmanaged(ast_mod.NodeIndex){};

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
                        .span = source_mod.Span.zero,
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
                .export_decl => |exp| {
                    if (exp.declaration != libts.ts_ast.null_node) {
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
                if (cot_stmt != ast_mod.null_node) {
                    try main_stmts.append(allocator, cot_stmt);
                }
            },
            else => {},
        }
    }

    // Wrap top-level statements in fn main() void { ... }
    if (main_stmts.items.len > 0) {
        const owned_stmts = try allocator.dupe(ast_mod.NodeIndex, main_stmts.items);
        const body = try cot.addExpr(.{ .block_expr = .{
            .stmts = owned_stmts,
            .expr = ast_mod.null_node,
            .span = source_mod.Span.zero,
        } });
        const void_type = try cot.addExpr(.{ .type_expr = .{
            .kind = .{ .named = "void" },
            .span = source_mod.Span.zero,
        } });
        const main_fn = try cot.addDecl(.{ .fn_decl = .{
            .name = "main",
            .params = &.{},
            .return_type = void_type,
            .body = body,
            .is_extern = false,
            .span = source_mod.Span.zero,
        } });
        try top_decls.append(allocator, main_fn);
    }

    cot.file = .{
        .filename = filename,
        .decls = try allocator.dupe(ast_mod.NodeIndex, top_decls.items),
        .span = source_mod.Span.zero,
        .safe_mode = true, // TS classes use this→self, which requires @safe mode
    };
}

fn transformTsFn(ts: *const libts.ts_ast.Ast, cot: *ast_mod.Ast, allocator: std.mem.Allocator, func: libts.ts_ast.Decl.FunctionDecl) error{OutOfMemory}!ast_mod.NodeIndex {
    const name = func.name orelse "anonymous";
    const params = try transformTsParams(ts, cot, allocator, func.params);
    const return_type = if (func.return_type != libts.ts_ast.null_node)
        try transformTsType(ts, cot, allocator, func.return_type)
    else
        try cot.addExpr(.{ .type_expr = .{ .kind = .{ .named = "void" }, .span = source_mod.Span.zero } });

    const body = if (func.body != libts.ts_ast.null_node)
        try transformTsBlock(ts, cot, allocator, func.body)
    else
        ast_mod.null_node;

    return cot.addDecl(.{ .fn_decl = .{
        .name = name,
        .params = params,
        .return_type = return_type,
        .body = body,
        .is_extern = false,
        .is_export = func.is_export,
        .is_async = func.is_async,
        .span = source_mod.Span.zero,
    } });
}

/// Transform TS class → Cot struct_decl + impl_block.
/// Emits both the struct (fields) and the impl (methods) as separate top-level decls.
fn transformTsClass(
    ts: *const libts.ts_ast.Ast,
    cot: *ast_mod.Ast,
    allocator: std.mem.Allocator,
    cls: libts.ts_ast.Decl.ClassDecl,
    top_decls: *std.ArrayListUnmanaged(ast_mod.NodeIndex),
) error{OutOfMemory}!void {
    const class_name = cls.name orelse "AnonymousClass";

    // Collect fields from property declarations.
    // If class extends a base, find and copy base fields first.
    var fields = std.ArrayListUnmanaged(ast_mod.Field){};
    var methods = std.ArrayListUnmanaged(ast_mod.NodeIndex){};

    if (cls.extends != libts.ts_ast.null_node) {
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
                                                    const bp_type = if (bp.type_ann != libts.ts_ast.null_node)
                                                        try transformTsType(ts, cot, allocator, bp.type_ann)
                                                    else
                                                        ast_mod.null_node;
                                                    try fields.append(allocator, .{
                                                        .name = bp_name,
                                                        .type_expr = bp_type,
                                                        .default_value = ast_mod.null_node,
                                                        .span = source_mod.Span.zero,
                                                    });
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
                    const type_expr = if (prop.type_ann != libts.ts_ast.null_node)
                        try transformTsType(ts, cot, allocator, prop.type_ann)
                    else
                        ast_mod.null_node;
                    const default_val = if (prop.init != libts.ts_ast.null_node)
                        try transformTsExpr(ts, cot, allocator, prop.init)
                    else
                        ast_mod.null_node;
                    try fields.append(allocator, .{
                        .name = prop_name,
                        .type_expr = type_expr,
                        .default_value = default_val,
                        .span = source_mod.Span.zero,
                    });
                },
                .constructor => |ctor| {
                    // constructor → init method. Inject self param (safe mode requires it at parse time).
                    const user_params = try transformTsParams(ts, cot, allocator, ctor.params);
                    const params = try prependSelfParam(cot, allocator, class_name, user_params);
                    const body = if (ctor.body != libts.ts_ast.null_node)
                        try transformTsBlock(ts, cot, allocator, ctor.body)
                    else
                        ast_mod.null_node;
                    const void_type = try cot.addExpr(.{ .type_expr = .{
                        .kind = .{ .named = "void" },
                        .span = source_mod.Span.zero,
                    } });
                    const init_fn = try cot.addDecl(.{ .fn_decl = .{
                        .name = "init",
                        .params = params,
                        .return_type = void_type,
                        .body = body,
                        .is_extern = false,
                        .span = source_mod.Span.zero,
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
                    const return_type = if (method.return_type != libts.ts_ast.null_node)
                        try transformTsType(ts, cot, allocator, method.return_type)
                    else
                        try cot.addExpr(.{ .type_expr = .{
                            .kind = .{ .named = "void" },
                            .span = source_mod.Span.zero,
                        } });
                    const body = if (method.body != libts.ts_ast.null_node)
                        try transformTsBlock(ts, cot, allocator, method.body)
                    else
                        ast_mod.null_node;
                    const fn_decl = try cot.addDecl(.{ .fn_decl = .{
                        .name = method_name,
                        .params = params,
                        .return_type = return_type,
                        .body = body,
                        .is_extern = false,
                        .is_async = method.is_async,
                        .is_static = method.is_static,
                        .span = source_mod.Span.zero,
                    } });
                    try methods.append(allocator, fn_decl);
                },
                else => {},
            },
            else => {},
        }
    }

    // Emit struct declaration
    const owned_fields = try allocator.dupe(ast_mod.Field, fields.items);
    const struct_decl = try cot.addDecl(.{ .struct_decl = .{
        .name = class_name,
        .fields = owned_fields,
        .span = source_mod.Span.zero,
    } });
    try top_decls.append(allocator, struct_decl);

    // Emit impl block (if there are methods)
    if (methods.items.len > 0) {
        const owned_methods = try allocator.dupe(ast_mod.NodeIndex, methods.items);
        const impl_block = try cot.addDecl(.{ .impl_block = .{
            .type_name = class_name,
            .methods = owned_methods,
            .span = source_mod.Span.zero,
        } });
        try top_decls.append(allocator, impl_block);
    }
}

/// Transform TS interface → Cot trait_decl.
fn transformTsInterface(
    ts: *const libts.ts_ast.Ast,
    cot: *ast_mod.Ast,
    allocator: std.mem.Allocator,
    iface: libts.ts_ast.Decl.InterfaceDecl,
    top_decls: *std.ArrayListUnmanaged(ast_mod.NodeIndex),
) error{OutOfMemory}!void {
    var methods = std.ArrayListUnmanaged(ast_mod.NodeIndex){};
    for (iface.members) |member| {
        if (member.kind == .method) {
            const params = try transformTsParams(ts, cot, allocator, member.params);
            const return_type = if (member.return_type != libts.ts_ast.null_node)
                try transformTsType(ts, cot, allocator, member.return_type)
            else
                try cot.addExpr(.{ .type_expr = .{ .kind = .{ .named = "void" }, .span = source_mod.Span.zero } });
            const fn_decl = try cot.addDecl(.{ .fn_decl = .{
                .name = member.name orelse "method",
                .params = params,
                .return_type = return_type,
                .body = ast_mod.null_node, // trait methods have no body
                .is_extern = false,
                .span = source_mod.Span.zero,
            } });
            try methods.append(allocator, fn_decl);
        }
    }

    const trait_decl = try cot.addDecl(.{ .trait_decl = .{
        .name = iface.name,
        .methods = try allocator.dupe(ast_mod.NodeIndex, methods.items),
        .span = source_mod.Span.zero,
    } });
    try top_decls.append(allocator, trait_decl);
}

/// Transform TS enum → Cot enum_decl (using const Name = enum { ... } pattern).
fn transformTsEnum(
    ts: *const libts.ts_ast.Ast,
    cot: *ast_mod.Ast,
    allocator: std.mem.Allocator,
    en: libts.ts_ast.Decl.EnumDecl,
) error{OutOfMemory}!ast_mod.NodeIndex {
    var variants = std.ArrayListUnmanaged(ast_mod.EnumVariant){};
    for (en.members) |member| {
        const value = if (member.init != libts.ts_ast.null_node)
            try transformTsExpr(ts, cot, allocator, member.init)
        else
            ast_mod.null_node;
        try variants.append(allocator, .{
            .name = member.name,
            .value = value,
            .span = source_mod.Span.zero,
        });
    }

    return cot.addDecl(.{ .enum_decl = .{
        .name = en.name,
        .backing_type = ast_mod.null_node,
        .variants = try allocator.dupe(ast_mod.EnumVariant, variants.items),
        .span = source_mod.Span.zero,
    } });
}

fn transformTsStmt(ts: *const libts.ts_ast.Ast, cot: *ast_mod.Ast, allocator: std.mem.Allocator, stmt: libts.ts_ast.Stmt) error{OutOfMemory}!ast_mod.NodeIndex {
    return switch (stmt) {
        .expr_stmt => |es| blk: {
            // Check if this is an assignment expression → emit assign_stmt
            const inner = ts.getNode(es.expr) orelse break :blk ast_mod.null_node;
            switch (inner.*) {
                .expr => |e| switch (e) {
                    .assign => |a| {
                        const target = try transformTsExpr(ts, cot, allocator, a.left);
                        const value = try transformTsExpr(ts, cot, allocator, a.right);
                        const op: token_mod.Token = switch (a.op) {
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
                            .span = source_mod.Span.zero,
                        } });
                    },
                    .update => |u| {
                        // i++ → i = i + 1, i-- → i = i - 1
                        const operand = try transformTsExpr(ts, cot, allocator, u.operand);
                        const one = try cot.addExpr(.{ .literal = .{
                            .kind = .float,
                            .value = "1.0",
                            .span = source_mod.Span.zero,
                        } });
                        const bin_op: token_mod.Token = if (u.op == .increment) .add else .sub;
                        const sum = try cot.addExpr(.{ .binary = .{
                            .op = bin_op,
                            .left = operand,
                            .right = one,
                            .span = source_mod.Span.zero,
                        } });
                        // Need a fresh reference for the target (can't reuse operand NodeIndex for both sides)
                        const target = try transformTsExpr(ts, cot, allocator, u.operand);
                        break :blk cot.addStmt(.{ .assign_stmt = .{
                            .target = target,
                            .value = sum,
                            .op = .assign,
                            .span = source_mod.Span.zero,
                        } });
                    },
                    else => {},
                },
                else => {},
            }
            const expr = try transformTsExpr(ts, cot, allocator, es.expr);
            break :blk cot.addStmt(.{ .expr_stmt = .{
                .expr = expr,
                .span = source_mod.Span.zero,
            } });
        },
        .return_stmt => |rs| blk: {
            const value = if (rs.value != libts.ts_ast.null_node)
                try transformTsExpr(ts, cot, allocator, rs.value)
            else
                ast_mod.null_node;
            break :blk cot.addStmt(.{ .return_stmt = .{
                .value = value,
                .span = source_mod.Span.zero,
            } });
        },
        .if_stmt => |ifs| blk: {
            const cond = try transformTsExpr(ts, cot, allocator, ifs.condition);
            const then_b = try transformTsStmtNode(ts, cot, allocator, ifs.consequent);
            const else_b = if (ifs.alternate != libts.ts_ast.null_node)
                try transformTsStmtNode(ts, cot, allocator, ifs.alternate)
            else
                ast_mod.null_node;
            break :blk cot.addStmt(.{ .if_stmt = .{
                .condition = cond,
                .then_branch = then_b,
                .else_branch = else_b,
                .span = source_mod.Span.zero,
            } });
        },
        .while_stmt => |ws| blk: {
            const cond = try transformTsExpr(ts, cot, allocator, ws.condition);
            const body = try transformTsStmtNode(ts, cot, allocator, ws.body);
            break :blk cot.addStmt(.{ .while_stmt = .{
                .condition = cond,
                .body = body,
                .span = source_mod.Span.zero,
            } });
        },
        .block => |bs| transformTsBlockStmt(ts, cot, allocator, bs),
        .var_stmt => |vs| blk: {
            const inner = ts.getNode(vs.decl) orelse break :blk ast_mod.null_node;
            switch (inner.*) {
                .decl => |d| switch (d) {
                    .variable => |v| {
                        if (v.declarators.len == 0) break :blk ast_mod.null_node;
                        const decl = v.declarators[0];
                        const vname = if (ts.getNode(decl.binding)) |bn| switch (bn.*) {
                            .expr => |e| switch (e) {
                                .ident => |id| id.name,
                                else => "_",
                            },
                            else => "_",
                        } else "_";
                        const type_expr = if (decl.type_ann != libts.ts_ast.null_node)
                            try transformTsType(ts, cot, allocator, decl.type_ann)
                        else
                            ast_mod.null_node;
                        const value = if (decl.init != libts.ts_ast.null_node)
                            try transformTsExpr(ts, cot, allocator, decl.init)
                        else
                            ast_mod.null_node;
                        break :blk cot.addStmt(.{ .var_stmt = .{
                            .name = vname,
                            .type_expr = type_expr,
                            .value = value,
                            .is_const = v.kind == .const_kw,
                            .span = source_mod.Span.zero,
                        } });
                    },
                    else => {},
                },
                else => {},
            }
            break :blk ast_mod.null_node;
        },
        .for_of => |fo| blk: {
            // for (const item of items) → for item in items
            const binding = ts.getNode(fo.left) orelse break :blk ast_mod.null_node;
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
                .span = source_mod.Span.zero,
            } });
        },
        .for_stmt => |fs| blk: {
            // for (init; cond; update) → init; while(cond) { body; update; }
            // Emit init as a separate statement, then while loop
            var stmts = std.ArrayListUnmanaged(ast_mod.NodeIndex){};
            if (fs.init != libts.ts_ast.null_node) {
                const init = try transformTsStmtNode(ts, cot, allocator, fs.init);
                if (init != ast_mod.null_node) try stmts.append(allocator, init);
            }
            const cond = if (fs.condition != libts.ts_ast.null_node)
                try transformTsExpr(ts, cot, allocator, fs.condition)
            else
                try cot.addExpr(.{ .literal = .{ .kind = .true_lit, .value = "true", .span = source_mod.Span.zero } });

            // Build while body: original body + update
            var body_stmts = std.ArrayListUnmanaged(ast_mod.NodeIndex){};
            const inner_body = try transformTsStmtNode(ts, cot, allocator, fs.body);
            if (inner_body != ast_mod.null_node) try body_stmts.append(allocator, inner_body);
            if (fs.update != libts.ts_ast.null_node) {
                const update_node = ts.getNode(fs.update) orelse break :blk ast_mod.null_node;
                switch (update_node.*) {
                    .expr => |e| switch (e) {
                        .update => |u| {
                            const operand = try transformTsExpr(ts, cot, allocator, u.operand);
                            const one = try cot.addExpr(.{ .literal = .{ .kind = .float, .value = "1.0", .span = source_mod.Span.zero } });
                            const bin_op: token_mod.Token = if (u.op == .increment) .add else .sub;
                            const sum = try cot.addExpr(.{ .binary = .{ .op = bin_op, .left = operand, .right = one, .span = source_mod.Span.zero } });
                            const target = try transformTsExpr(ts, cot, allocator, u.operand);
                            const assign = try cot.addStmt(.{ .assign_stmt = .{ .target = target, .value = sum, .op = .assign, .span = source_mod.Span.zero } });
                            try body_stmts.append(allocator, assign);
                        },
                        else => {
                            const update_expr = try transformTsExpr(ts, cot, allocator, fs.update);
                            const update_stmt = try cot.addStmt(.{ .expr_stmt = .{ .expr = update_expr, .span = source_mod.Span.zero } });
                            try body_stmts.append(allocator, update_stmt);
                        },
                    },
                    else => {},
                }
            }
            const while_body = try cot.addExpr(.{ .block_expr = .{
                .stmts = try allocator.dupe(ast_mod.NodeIndex, body_stmts.items),
                .expr = ast_mod.null_node,
                .span = source_mod.Span.zero,
            } });
            const while_stmt = try cot.addStmt(.{ .while_stmt = .{
                .condition = cond,
                .body = while_body,
                .span = source_mod.Span.zero,
            } });
            try stmts.append(allocator, while_stmt);

            // Wrap init + while in a block
            break :blk cot.addExpr(.{ .block_expr = .{
                .stmts = try allocator.dupe(ast_mod.NodeIndex, stmts.items),
                .expr = ast_mod.null_node,
                .span = source_mod.Span.zero,
            } });
        },
        .throw_stmt => |ts_throw| blk: {
            // throw expr → return error.TsError
            _ = ts_throw;
            break :blk cot.addStmt(.{ .return_stmt = .{
                .value = try cot.addExpr(.{ .error_literal = .{
                    .error_name = "TsError",
                    .span = source_mod.Span.zero,
                } }),
                .span = source_mod.Span.zero,
            } });
        },
        .break_stmt => |bs| blk: {
            _ = bs;
            break :blk cot.addStmt(.{ .break_stmt = .{
                .label = null,
                .value = ast_mod.null_node,
                .span = source_mod.Span.zero,
            } });
        },
        .continue_stmt => |cs| blk: {
            _ = cs;
            break :blk cot.addStmt(.{ .continue_stmt = .{
                .label = null,
                .span = source_mod.Span.zero,
            } });
        },
        else => ast_mod.null_node,
    };
}

fn transformTsStmtNode(ts: *const libts.ts_ast.Ast, cot: *ast_mod.Ast, allocator: std.mem.Allocator, idx: libts.ts_ast.NodeIndex) error{OutOfMemory}!ast_mod.NodeIndex {
    const node = ts.getNode(idx) orelse return ast_mod.null_node;
    switch (node.*) {
        .stmt => |s| return transformTsStmt(ts, cot, allocator, s),
        else => return ast_mod.null_node,
    }
}

fn transformTsBlock(ts: *const libts.ts_ast.Ast, cot: *ast_mod.Ast, allocator: std.mem.Allocator, idx: libts.ts_ast.NodeIndex) error{OutOfMemory}!ast_mod.NodeIndex {
    const node = ts.getNode(idx) orelse return ast_mod.null_node;
    switch (node.*) {
        .stmt => |s| switch (s) {
            .block => |bs| return transformTsBlockStmt(ts, cot, allocator, bs),
            else => {},
        },
        else => {},
    }
    return ast_mod.null_node;
}

fn transformTsBlockStmt(ts: *const libts.ts_ast.Ast, cot: *ast_mod.Ast, allocator: std.mem.Allocator, bs: libts.ts_ast.Stmt.BlockStmt) error{OutOfMemory}!ast_mod.NodeIndex {
    var stmts = std.ArrayListUnmanaged(ast_mod.NodeIndex){};
    for (bs.stmts) |stmt_idx| {
        const cot_stmt = try transformTsStmtNode(ts, cot, allocator, stmt_idx);
        if (cot_stmt != ast_mod.null_node) {
            try stmts.append(allocator, cot_stmt);
        }
    }
    const owned = try allocator.dupe(ast_mod.NodeIndex, stmts.items);
    return cot.addExpr(.{ .block_expr = .{
        .stmts = owned,
        .expr = ast_mod.null_node,
        .span = source_mod.Span.zero,
    } });
}

fn transformTsExpr(ts: *const libts.ts_ast.Ast, cot: *ast_mod.Ast, allocator: std.mem.Allocator, idx: libts.ts_ast.NodeIndex) error{OutOfMemory}!ast_mod.NodeIndex {
    const node = ts.getNode(idx) orelse return ast_mod.null_node;
    switch (node.*) {
        .expr => |e| return transformTsExprNode(ts, cot, allocator, e),
        else => return ast_mod.null_node,
    }
}

fn transformTsExprNode(ts: *const libts.ts_ast.Ast, cot: *ast_mod.Ast, allocator: std.mem.Allocator, expr: libts.ts_ast.Expr) error{OutOfMemory}!ast_mod.NodeIndex {
    return switch (expr) {
        .ident => |id| cot.addExpr(.{ .ident = .{
            .name = id.name,
            .span = source_mod.Span.zero,
        } }),
        .literal => |lit| blk: {
            // In TS, all numbers are f64. Emit integer-looking literals as floats
            // so Cot's type checker sees them as f64, not int.
            const kind: ast_mod.LiteralKind = switch (lit.kind) {
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
                .span = source_mod.Span.zero,
            } });
        },
        .call => |c| blk: {
            // console.log → println, console.error → eprintln
            const callee = if (isConsoleMethod(ts, c.callee, "log"))
                try cot.addExpr(.{ .ident = .{ .name = "println", .span = source_mod.Span.zero } })
            else if (isConsoleMethod(ts, c.callee, "error"))
                try cot.addExpr(.{ .ident = .{ .name = "eprintln", .span = source_mod.Span.zero } })
            else
                try transformTsExpr(ts, cot, allocator, c.callee);

            var args = std.ArrayListUnmanaged(ast_mod.NodeIndex){};
            for (c.args) |arg_idx| {
                const a = try transformTsExpr(ts, cot, allocator, arg_idx);
                try args.append(allocator, a);
            }
            const owned_args = try allocator.dupe(ast_mod.NodeIndex, args.items);
            break :blk cot.addExpr(.{ .call = .{
                .callee = callee,
                .args = owned_args,
                .span = source_mod.Span.zero,
            } });
        },
        .member => |m| blk: {
            const base = try transformTsExpr(ts, cot, allocator, m.object);
            break :blk cot.addExpr(.{ .field_access = .{
                .base = base,
                .field = m.property,
                .span = source_mod.Span.zero,
            } });
        },
        .binary => |b| blk: {
            const left = try transformTsExpr(ts, cot, allocator, b.left);
            const right = try transformTsExpr(ts, cot, allocator, b.right);
            const op: token_mod.Token = switch (b.op) {
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
                .span = source_mod.Span.zero,
            } });
        },
        .unary => |u| blk: {
            const operand = try transformTsExpr(ts, cot, allocator, u.operand);
            const op: token_mod.Token = switch (u.op) {
                .neg => .sub,
                .pos => .add,
                .lnot => .lnot,
                .bitnot => .not,
                else => .sub,
            };
            break :blk cot.addExpr(.{ .unary = .{
                .op = op,
                .operand = operand,
                .span = source_mod.Span.zero,
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
            var args = std.ArrayListUnmanaged(ast_mod.NodeIndex){};
            for (n.args) |arg_idx| {
                const a = try transformTsExpr(ts, cot, allocator, arg_idx);
                try args.append(allocator, a);
            }
            const owned_args = try allocator.dupe(ast_mod.NodeIndex, args.items);
            break :blk cot.addExpr(.{ .new_expr = .{
                .type_name = type_name,
                .fields = &.{},
                .constructor_args = owned_args,
                .is_constructor = true,
                .span = source_mod.Span.zero,
            } });
        },
        .this_expr => cot.addExpr(.{ .ident = .{ .name = "self", .span = source_mod.Span.zero } }),
        .paren => |p| blk: {
            if (p.inner == libts.ts_ast.null_node) break :blk ast_mod.null_node;
            break :blk transformTsExpr(ts, cot, allocator, p.inner);
        },
        .template => |t| blk: {
            // Simple template with no substitutions → string literal
            if (t.expressions.len == 0 and t.quasis.len == 1) {
                const raw = t.quasis[0];
                const stripped = if (raw.len >= 2 and raw[0] == '`' and raw[raw.len - 1] == '`')
                    raw[1 .. raw.len - 1]
                else
                    raw;
                const quoted = try std.fmt.allocPrint(allocator, "\"{s}\"", .{stripped});
                break :blk cot.addExpr(.{ .literal = .{
                    .kind = .string,
                    .value = quoted,
                    .span = source_mod.Span.zero,
                } });
            }
            break :blk cot.addExpr(.{ .literal = .{
                .kind = .string,
                .value = "\"[template]\"",
                .span = source_mod.Span.zero,
            } });
        },
        else => ast_mod.null_node,
    };
}

fn transformTsType(ts: *const libts.ts_ast.Ast, cot: *ast_mod.Ast, _: std.mem.Allocator, idx: libts.ts_ast.NodeIndex) error{OutOfMemory}!ast_mod.NodeIndex {
    const node = ts.getNode(idx) orelse return ast_mod.null_node;
    switch (node.*) {
        .type_node => |t| switch (t) {
            .keyword => |kw| {
                const name: []const u8 = switch (kw.kind) {
                    .number => "f64",
                    .string => "string",
                    .boolean => "bool",
                    .void_kw, .null_kw, .undefined => "void",
                    .any, .unknown, .object, .symbol, .bigint => "i64",
                    .never => "noreturn",
                };
                return cot.addExpr(.{ .type_expr = .{ .kind = .{ .named = name }, .span = source_mod.Span.zero } });
            },
            .reference => |ref| return cot.addExpr(.{ .type_expr = .{ .kind = .{ .named = ref.name }, .span = source_mod.Span.zero } }),
            else => return cot.addExpr(.{ .type_expr = .{ .kind = .{ .named = "i64" }, .span = source_mod.Span.zero } }),
        },
        else => return ast_mod.null_node,
    }
}

fn transformTsParams(ts: *const libts.ts_ast.Ast, cot: *ast_mod.Ast, allocator: std.mem.Allocator, params: []const libts.ts_ast.Param) error{OutOfMemory}![]const ast_mod.Field {
    var fields = std.ArrayListUnmanaged(ast_mod.Field){};
    for (params) |p| {
        const name = if (ts.getNode(p.binding)) |bn| switch (bn.*) {
            .expr => |e| switch (e) {
                .ident => |id| id.name,
                else => "_",
            },
            else => "_",
        } else "_";
        const type_expr = if (p.type_ann != libts.ts_ast.null_node)
            try transformTsType(ts, cot, allocator, p.type_ann)
        else
            ast_mod.null_node;
        const default_value = if (p.default_value != libts.ts_ast.null_node)
            try transformTsExpr(ts, cot, allocator, p.default_value)
        else
            ast_mod.null_node;
        try fields.append(allocator, .{
            .name = name,
            .type_expr = type_expr,
            .default_value = default_value,
            .span = source_mod.Span.zero,
        });
    }
    return allocator.dupe(ast_mod.Field, fields.items);
}

/// Prepend `self: TypeName` parameter to a method's param list.
/// Mirrors what the Cot parser does in @safe mode for impl block methods.
fn prependSelfParam(cot: *ast_mod.Ast, allocator: std.mem.Allocator, type_name: []const u8, user_params: []const ast_mod.Field) error{OutOfMemory}![]const ast_mod.Field {
    const self_type = try cot.addExpr(.{ .type_expr = .{
        .kind = .{ .named = type_name },
        .span = source_mod.Span.zero,
    } });
    var new_params = try allocator.alloc(ast_mod.Field, user_params.len + 1);
    new_params[0] = .{
        .name = "self",
        .type_expr = self_type,
        .default_value = ast_mod.null_node,
        .span = source_mod.Span.zero,
    };
    @memcpy(new_params[1..], user_params);
    return new_params;
}

fn isConsoleMethod(ts: *const libts.ts_ast.Ast, idx: libts.ts_ast.NodeIndex, method: []const u8) bool {
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

// ============================================================================
// ============================================================================
// Tests
// ============================================================================

test "Driver: init and set target" {
    const allocator = std.testing.allocator;
    var driver = Driver.init(allocator);
    driver.setTarget(.{ .arch = .amd64, .os = .linux });
    try std.testing.expectEqual(target_mod.Arch.amd64, driver.target.arch);
    try std.testing.expectEqual(target_mod.Os.linux, driver.target.os);
}

test "Driver: test mode toggle" {
    const allocator = std.testing.allocator;
    var driver = Driver.init(allocator);
    try std.testing.expect(!driver.test_mode);
    driver.setTestMode(true);
    try std.testing.expect(driver.test_mode);
}

test "Driver: normalizePath returns copy on error" {
    const allocator = std.testing.allocator;
    var driver = Driver.init(allocator);
    const result = try driver.normalizePath("nonexistent_file_12345.zig");
    defer allocator.free(result);
    try std.testing.expectEqualStrings("nonexistent_file_12345.zig", result);
}
