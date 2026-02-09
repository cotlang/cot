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
const ssa_builder_mod = @import("frontend/ssa_builder.zig");
const source_mod = @import("frontend/source.zig");
// Native codegen imports (AOT compiler path)
// NOTE: Native codegen is being rewritten with Cranelift architecture.
// See CRANELIFT_PORT_MASTER_PLAN.md for status and progress.
const schedule = @import("ssa/passes/schedule.zig");
const layout = @import("ssa/passes/layout.zig");
const rewritegeneric = @import("ssa/passes/rewritegeneric.zig");
const decompose_builtin = @import("ssa/passes/decompose.zig");
const rewritedec = @import("ssa/passes/rewritedec.zig");
const lower_wasm = @import("ssa/passes/lower_wasm.zig");
const target_mod = @import("core/target.zig");
const pipeline_debug = @import("pipeline_debug.zig");

// Wasm codegen
const wasm_old = @import("codegen/wasm.zig"); // Old module builder (for CodeBuilder)
const wasm = @import("codegen/wasm/wasm.zig"); // New Go-style package (for Linker)
const wasm_gen = @import("codegen/wasm_gen.zig");
const arc = @import("codegen/arc.zig"); // ARC runtime (Swift)
const slice_runtime = @import("codegen/slice_runtime.zig"); // Slice runtime (Go)
const print_runtime = @import("codegen/print_runtime.zig"); // Print runtime (Go)
const wasi_runtime = @import("codegen/wasi_runtime.zig"); // WASI runtime (fd_write)
const test_runtime = @import("codegen/test_runtime.zig"); // Test runtime (Zig)

// Native codegen modules (Cranelift-style AOT compiler)
const native_compile = @import("codegen/native/compile.zig");
const wasm_parser = @import("codegen/native/wasm_parser.zig");
const wasm_to_clif = @import("codegen/native/wasm_to_clif/translator.zig");
const wasm_decoder = @import("codegen/native/wasm_to_clif/decoder.zig");
const wasm_func_translator = @import("codegen/native/wasm_to_clif/func_translator.zig");
const clif = @import("ir/clif/mod.zig");
const macho = @import("codegen/native/macho.zig");
const elf = @import("codegen/native/elf.zig");
const object_module = @import("codegen/native/object_module.zig");
const buffer_mod = @import("codegen/native/machinst/buffer.zig");

const Allocator = std.mem.Allocator;
const Target = target_mod.Target;

const ParsedFile = struct {
    path: []const u8,
    source_text: []const u8,
    source: source_mod.Source,
    tree: ast_mod.Ast,
};

pub const Driver = struct {
    allocator: Allocator,
    target: Target = Target.native(),
    test_mode: bool = false,

    pub fn init(allocator: Allocator) Driver {
        return .{ .allocator = allocator };
    }

    pub fn setTarget(self: *Driver, t: Target) void {
        self.target = t;
    }

    pub fn setTestMode(self: *Driver, enabled: bool) void {
        self.test_mode = enabled;
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
        var chk = checker_mod.Checker.init(self.allocator, &tree, &type_reg, &err_reporter, &global_scope, &generic_ctx);
        defer chk.deinit();
        try chk.checkFile();
        if (err_reporter.hasErrors()) return error.TypeCheckError;

        // Lower to IR
        var lowerer = lower_mod.Lowerer.init(self.allocator, &tree, &type_reg, &err_reporter, &chk);
        defer lowerer.deinit();
        if (self.test_mode) lowerer.setTestMode(true);
        try lowerer.lowerToBuilder();
        if (err_reporter.hasErrors()) return error.LowerError;

        // Generate test runner if in test mode
        if (self.test_mode and lowerer.test_names.items.len > 0) {
            try lowerer.generateTestRunner();
        }

        var ir_result = try lowerer.builder.getIR();
        defer ir_result.deinit();

        return self.generateCode(ir_result.funcs, ir_result.globals, &type_reg, "<input>", source_text);
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
        try self.parseFileRecursive(path, &parsed_files, &seen_files);

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

        for (parsed_files.items) |*pf| {
            var err_reporter = errors_mod.ErrorReporter.init(&pf.source, null);
            var chk = checker_mod.Checker.init(self.allocator, &pf.tree, &type_reg, &err_reporter, &global_scope, &generic_ctx);
            chk.checkFile() catch |e| {
                chk.deinit();
                return e;
            };
            if (err_reporter.hasErrors()) {
                chk.deinit();
                return error.TypeCheckError;
            }
            try checkers.append(self.allocator, chk);
        }

        // Phase 3: Lower all files to IR with shared builder
        var shared_builder = ir_mod.Builder.init(self.allocator, &type_reg);
        defer shared_builder.deinit();

        var all_test_names = std.ArrayListUnmanaged([]const u8){};
        defer all_test_names.deinit(self.allocator);
        var all_test_display_names = std.ArrayListUnmanaged([]const u8){};
        defer all_test_display_names.deinit(self.allocator);

        for (parsed_files.items, 0..) |*pf, i| {
            var lower_err = errors_mod.ErrorReporter.init(&pf.source, null);
            var lowerer = lower_mod.Lowerer.initWithBuilder(self.allocator, &pf.tree, &type_reg, &lower_err, &checkers.items[i], shared_builder);
            if (self.test_mode) lowerer.setTestMode(true);

            lowerer.lowerToBuilder() catch |e| {
                lowerer.deinitWithoutBuilder();
                return e;
            };
            if (lower_err.hasErrors()) {
                lowerer.deinitWithoutBuilder();
                return error.LowerError;
            }

            if (self.test_mode) {
                for (lowerer.getTestNames()) |n| try all_test_names.append(self.allocator, n);
                for (lowerer.getTestDisplayNames()) |n| try all_test_display_names.append(self.allocator, n);
            }
            shared_builder = lowerer.builder;
            lowerer.deinitWithoutBuilder();
        }

        // Generate test runner if in test mode
        if (self.test_mode and all_test_names.items.len > 0) {
            var dummy_err = errors_mod.ErrorReporter.init(&parsed_files.items[0].source, null);
            var runner = lower_mod.Lowerer.initWithBuilder(self.allocator, &parsed_files.items[0].tree, &type_reg, &dummy_err, &checkers.items[0], shared_builder);
            for (all_test_names.items) |n| try runner.addTestName(n);
            for (all_test_display_names.items) |n| try runner.addTestDisplayName(n);
            try runner.generateTestRunner();
            shared_builder = runner.builder;
            runner.deinitWithoutBuilder();
        }

        var final_ir = try shared_builder.getIR();
        defer final_ir.deinit();

        const main_file = if (parsed_files.items.len > 0) parsed_files.items[parsed_files.items.len - 1] else ParsedFile{
            .path = path,
            .source_text = "",
            .source = undefined,
            .tree = undefined,
        };
        return self.generateCode(final_ir.funcs, final_ir.globals, &type_reg, main_file.path, main_file.source_text);
    }

    fn normalizePath(self: *Driver, path: []const u8) ![]const u8 {
        return std.fs.cwd().realpathAlloc(self.allocator, path) catch try self.allocator.dupe(u8, path);
    }

    fn parseFileRecursive(self: *Driver, path: []const u8, parsed_files: *std.ArrayListUnmanaged(ParsedFile), seen_files: *std.StringHashMap(void)) !void {
        const canonical_path = try self.normalizePath(path);
        defer self.allocator.free(canonical_path);

        if (seen_files.contains(canonical_path)) return;

        const path_copy = try self.allocator.dupe(u8, canonical_path);
        errdefer self.allocator.free(path_copy);
        try seen_files.put(path_copy, {});

        const source_text = std.fs.cwd().readFileAlloc(self.allocator, canonical_path, 1024 * 1024) catch |e| {
            std.debug.print("Failed to read file: {s}: {any}\n", .{ canonical_path, e });
            return e;
        };
        errdefer self.allocator.free(source_text);

        var src = source_mod.Source.init(self.allocator, canonical_path, source_text);
        errdefer src.deinit();
        var err_reporter = errors_mod.ErrorReporter.init(&src, null);

        var tree = ast_mod.Ast.init(self.allocator);
        errdefer tree.deinit();
        var scan = scanner_mod.Scanner.initWithErrors(&src, &err_reporter);
        var parser = parser_mod.Parser.init(self.allocator, &scan, &tree, &err_reporter);
        try parser.parseFile();
        if (err_reporter.hasErrors()) return error.ParseError;

        // Parse imports first (dependencies before dependents)
        const imports = try tree.getImports(self.allocator);
        defer self.allocator.free(imports);
        const file_dir = std.fs.path.dirname(canonical_path) orelse ".";
        for (imports) |import_path| {
            const full_path = if (std.mem.startsWith(u8, import_path, "std/"))
                try self.resolveStdImport(import_path, file_dir)
            else
                try std.fs.path.join(self.allocator, &.{ file_dir, import_path });
            defer self.allocator.free(full_path);
            try self.parseFileRecursive(full_path, parsed_files, seen_files);
        }

        try parsed_files.append(self.allocator, .{ .path = path_copy, .source_text = source_text, .source = src, .tree = tree });
    }

    /// Resolve stdlib imports: "std/list" → "<stdlib_dir>/list.cot"
    /// Discovery order (like Zig's findZigLibDirFromSelfExe):
    /// 1. COT_STDLIB env var (explicit override)
    /// 2. Walk up from source file looking for stdlib/ directory
    /// 3. CWD fallback (./stdlib/)
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

        // Tier 3: CWD fallback
        return std.fs.path.join(self.allocator, &.{ "stdlib", filename });
    }

    /// Unified code generation for all architectures.
    fn generateCode(self: *Driver, funcs: []const ir_mod.Func, globals: []const ir_mod.Global, type_reg: *types_mod.TypeRegistry, source_file: []const u8, source_text: []const u8) ![]u8 {
        _ = globals;

        // Wasm target: use Wasm codegen pipeline
        if (self.target.isWasm()) {
            return self.generateWasmCode(funcs, type_reg);
        }

        // Native target: AOT compilation path (Wasm → Native)
        // Pipeline: Cot → Wasm → CLIF → VCode → Native
        //
        // This implements the Cranelift-style architecture:
        // 1. First compile to Wasm bytecode (standard path)
        // 2. Parse the Wasm module
        // 3. Translate Wasm → CLIF IR for each function
        // 4. Lower CLIF → VCode (virtual registers)
        // 5. Run register allocation
        // 6. Emit machine code
        // 7. Link into object file

        _ = source_file;
        _ = source_text;

        // Step 1: Generate Wasm bytecode first
        pipeline_debug.log(.codegen, "driver: generating Wasm for native AOT compilation", .{});
        const wasm_bytes = try self.generateWasmCode(funcs, type_reg);
        defer self.allocator.free(wasm_bytes);

        // Step 2: Call native code generation
        return self.generateNativeCode(wasm_bytes);
    }

    /// Generate native machine code from Wasm bytecode.
    /// Uses the Cranelift-style AOT compilation pipeline.
    ///
    /// Pipeline: Wasm bytecode → CLIF IR → VCode → Native machine code
    ///
    /// This is a faithful port of Cranelift's compilation flow from
    /// wasmtime/cranelift/src/compiler.rs and cranelift/codegen/src/machinst/compile.rs
    fn generateNativeCode(self: *Driver, wasm_bytes: []const u8) ![]u8 {
        pipeline_debug.log(.codegen, "driver: AOT compiling {d} bytes of Wasm to native", .{wasm_bytes.len});

        // ====================================================================
        // Step 1: Parse Wasm module
        // Reference: wasmparser crate's Module parsing
        // ====================================================================
        var wasm_parser_inst = wasm_parser.Parser.init(self.allocator, wasm_bytes);
        var wasm_module = wasm_parser_inst.parse() catch |e| {
            pipeline_debug.log(.codegen, "driver: Wasm parse error: {any}", .{e});
            return error.WasmParseError;
        };
        defer wasm_module.deinit();

        pipeline_debug.log(.codegen, "driver: parsed Wasm module with {d} functions, {d} types", .{
            wasm_module.code.len,
            wasm_module.types.len,
        });

        // Report target
        const arch_name: []const u8 = switch (self.target.arch) {
            .arm64 => "ARM64",
            .amd64 => "x86-64",
            .wasm32 => "Wasm32",
        };
        const os_name: []const u8 = switch (self.target.os) {
            .macos => "macOS",
            .linux => "Linux",
            .freestanding => "freestanding",
        };
        pipeline_debug.log(.codegen, "driver: target: {s} / {s}", .{ arch_name, os_name });

        // ====================================================================
        // Step 2: Translate each Wasm function to CLIF IR
        // Reference: wasmtime/cranelift/src/compiler.rs translate_function()
        // ====================================================================
        var compiled_funcs = std.ArrayListUnmanaged(native_compile.CompiledCode){};
        defer {
            for (compiled_funcs.items) |*cf| {
                cf.deinit();
            }
            compiled_funcs.deinit(self.allocator);
        }

        // Convert module globals to translator format for type lookup
        var globals_converted = try self.allocator.alloc(wasm_func_translator.WasmGlobalType, wasm_module.globals.len);
        defer self.allocator.free(globals_converted);
        for (wasm_module.globals, 0..) |g, i| {
            globals_converted[i] = .{
                .val_type = convertToTranslatorValType(g.val_type),
                .mutable = g.mutable,
            };
        }

        // Convert module function types to translator format
        var func_types_converted = try self.allocator.alloc(wasm_func_translator.WasmFuncType, wasm_module.types.len);
        defer {
            for (func_types_converted) |ft| {
                self.allocator.free(ft.params);
                self.allocator.free(ft.results);
            }
            self.allocator.free(func_types_converted);
        }
        for (wasm_module.types, 0..) |ft, i| {
            const params = try self.allocator.alloc(wasm_func_translator.TranslatorWasmValType, ft.params.len);
            for (ft.params, 0..) |p, j| {
                params[j] = convertToTranslatorValType(p);
            }
            const results = try self.allocator.alloc(wasm_func_translator.TranslatorWasmValType, ft.results.len);
            for (ft.results, 0..) |r, j| {
                results[j] = convertToTranslatorValType(r);
            }
            func_types_converted[i] = .{
                .params = params,
                .results = results,
            };
        }

        // Create reusable translator context with module info
        // Pass func_to_type mapping (wasm_module.funcs maps function_index -> type_index)
        // Pass table_elements for AOT call_indirect resolution
        var func_translator = wasm_func_translator.WasmFuncTranslator.init(self.allocator, globals_converted, func_types_converted, wasm_module.funcs, wasm_module.table_elements);
        defer func_translator.deinit();

        // Select ISA based on target
        const isa = switch (self.target.arch) {
            .arm64 => native_compile.TargetIsa{ .aarch64 = native_compile.AArch64Backend.default },
            .amd64 => native_compile.TargetIsa{ .x64 = native_compile.X64Backend.default },
            .wasm32 => return error.InvalidTargetForNative,
        };

        for (wasm_module.code, 0..) |func_code, func_idx| {
            pipeline_debug.log(.codegen, "driver: translating function {d}", .{func_idx});

            // Get function type from module
            const type_idx = if (func_idx < wasm_module.funcs.len)
                wasm_module.funcs[func_idx]
            else
                0;
            const func_type = if (type_idx < wasm_module.types.len)
                wasm_module.types[type_idx]
            else
                wasm_parser.FuncType{ .params = &[_]wasm_old.ValType{}, .results = &[_]wasm_old.ValType{} };

            // ----------------------------------------------------------------
            // Step 2a: Decode Wasm bytecode into operators
            // Reference: wasmparser's BinaryReader iteration
            // ----------------------------------------------------------------
            var decoder = wasm_decoder.Decoder.init(self.allocator, func_code.body);
            const wasm_ops = decoder.decodeAll() catch |e| {
                pipeline_debug.log(.codegen, "driver: decode error for function {d}: {any}", .{ func_idx, e });
                return error.WasmDecodeError;
            };
            defer {
                // Free inner allocations (br_table targets)
                for (wasm_ops) |op| {
                    switch (op) {
                        .br_table => |data| self.allocator.free(data.targets),
                        else => {},
                    }
                }
                self.allocator.free(wasm_ops);
            }

            pipeline_debug.log(.codegen, "driver: decoded {d} operators", .{wasm_ops.len});

            // ----------------------------------------------------------------
            // Step 2b: Create CLIF Function and translate
            // Reference: cranelift-wasm FuncTranslator::translate_body()
            // ----------------------------------------------------------------
            var clif_func = clif.Function.init(self.allocator);
            defer clif_func.deinit();

            // Convert wasm types to func_translator types
            // Allocate and convert param types
            var param_types = try self.allocator.alloc(wasm_func_translator.WasmValType, func_type.params.len);
            defer self.allocator.free(param_types);
            for (func_type.params, 0..) |p, i| {
                param_types[i] = convertWasmValType(p);
            }

            // Allocate and convert result types
            var result_types = try self.allocator.alloc(wasm_func_translator.WasmValType, func_type.results.len);
            defer self.allocator.free(result_types);
            for (func_type.results, 0..) |r, i| {
                result_types[i] = convertWasmValType(r);
            }

            const signature = wasm_func_translator.FuncSignature{
                .params = param_types,
                .results = result_types,
            };

            // Convert locals
            var locals_converted = try self.allocator.alloc(wasm_func_translator.LocalDecl, func_code.locals.len);
            defer self.allocator.free(locals_converted);
            for (func_code.locals, 0..) |local, i| {
                locals_converted[i] = .{
                    .count = local.count,
                    .val_type = convertWasmValType(local.val_type),
                };
            }

            // Convert WasmOp to WasmOperator (basic subset)
            var basic_ops = std.ArrayListUnmanaged(wasm_func_translator.WasmOperator){};
            defer basic_ops.deinit(self.allocator);
            for (wasm_ops) |op| {
                if (op.toBasicOperator()) |basic| {
                    try basic_ops.append(self.allocator, basic);
                }
                // Skip unsupported ops for now (memory, calls, etc.)
            }

            // Translate to CLIF
            func_translator.translateFunction(
                &clif_func,
                signature,
                locals_converted,
                basic_ops.items,
            ) catch |e| {
                pipeline_debug.log(.codegen, "driver: translation error for function {d}: {any}", .{ func_idx, e });
                return error.WasmTranslationError;
            };

            // Debug: check CLIF function size
            const num_blocks = clif_func.dfg.blocks.items.len;
            const num_insts = clif_func.dfg.insts.items.len;
            pipeline_debug.log(.codegen, "driver: translated function {d} to CLIF ({d} blocks, {d} insts)", .{ func_idx, num_blocks, num_insts });

            // ----------------------------------------------------------------
            // D1/D3: Layout verification - ensure blocks are in Layout, not just DFG
            // Reference: Cranelift requires blocks in Layout for compilation
            // Uses the D3 Layout vs DFG comparison utility
            // ----------------------------------------------------------------
            clif_func.logLayoutComparison(pipeline_debug);

            const layout_block_count = blk: {
                var count: usize = 0;
                var layout_iter = clif_func.layout.blocks();
                while (layout_iter.next()) |_| count += 1;
                break :blk count;
            };

            if (layout_block_count == 0 and num_blocks > 0) {
                pipeline_debug.log(.codegen, "CRITICAL: Blocks exist in DFG but Layout is EMPTY!", .{});
                pipeline_debug.log(.codegen, "This indicates ensureInsertedBlock() was never called during translation", .{});
                // Don't return error yet - let's see what compilation produces
            }

            // ----------------------------------------------------------------
            // Step 2c: Compile CLIF to native
            // Reference: cranelift/codegen/src/machinst/compile.rs compile()
            // ----------------------------------------------------------------
            var ctrl_plane = native_compile.ControlPlane.init();
            const compiled = native_compile.compile(
                self.allocator,
                &clif_func,
                isa,
                &ctrl_plane,
            ) catch |e| {
                pipeline_debug.log(.codegen, "driver: compile error for function {d}: {any}", .{ func_idx, e });
                return error.NativeCompileError;
            };

            try compiled_funcs.append(self.allocator, compiled);
            pipeline_debug.log(.codegen, "driver: compiled function {d}: {d} bytes", .{
                func_idx,
                compiled.codeSize(),
            });
        }

        // ====================================================================
        // Step 3: Generate object file
        // Reference: cranelift-object crate
        // ====================================================================
        pipeline_debug.log(.codegen, "driver: generating object file for {d} functions", .{compiled_funcs.items.len});

        const object_bytes = switch (self.target.os) {
            .macos => try self.generateMachO(compiled_funcs.items, wasm_module.exports, wasm_module.data_segments, wasm_module.globals),
            .linux => try self.generateElf(compiled_funcs.items, wasm_module.exports, wasm_module.data_segments, wasm_module.globals),
            .freestanding => return error.UnsupportedObjectFormat,
        };

        return object_bytes;
    }

    /// Generate Mach-O object file from compiled functions.
    /// Uses ObjectModule to bridge CompiledCode to Mach-O format.
    fn generateMachO(self: *Driver, compiled_funcs: []const native_compile.CompiledCode, exports: []const wasm_parser.Export, data_segments: []const wasm_parser.DataSegment, globals: []const wasm_parser.GlobalType) ![]u8 {
        var module = object_module.ObjectModule.initWithTarget(
            self.allocator,
            .macos,
            .aarch64,
        );
        defer module.deinit();

        // Build export name map for function lookup
        var export_names = std.StringHashMap(u32).init(self.allocator);
        defer export_names.deinit();
        for (exports, 0..) |exp, i| {
            if (exp.kind == .func) {
                try export_names.put(exp.name, @intCast(i));
            }
        }

        // Track if we need to generate a main wrapper
        var main_func_index: ?usize = null;

        // Pass 1: Declare all functions and external names (Cranelift pattern:
        // separate declaration from definition so forward references resolve)
        var func_ids = try self.allocator.alloc(object_module.FuncId, compiled_funcs.len);
        defer self.allocator.free(func_ids);

        for (compiled_funcs, 0..) |_, i| {
            // Determine function name from exports, or generate one
            var func_name: []const u8 = "";
            var func_name_allocated = false;
            for (exports) |exp| {
                if (exp.kind == .func and exp.index == i) {
                    func_name = exp.name;
                    break;
                }
            }
            if (func_name.len == 0) {
                func_name = try std.fmt.allocPrint(self.allocator, "_func_{d}", .{i});
                func_name_allocated = true;
            }
            defer if (func_name_allocated) self.allocator.free(func_name);

            const is_main = std.mem.eql(u8, func_name, "main");
            if (is_main) {
                main_func_index = i;
            }

            const mangled_name = if (is_main)
                try self.allocator.dupe(u8, "__wasm_main")
            else
                try std.fmt.allocPrint(self.allocator, "_{s}", .{func_name});
            defer self.allocator.free(mangled_name);

            const linkage: object_module.Linkage = if (is_main)
                .Local
            else if (func_name_allocated)
                .Local
            else
                .Export;

            func_ids[i] = try module.declareFunction(mangled_name, linkage);
            try module.declareExternalName(@intCast(i), mangled_name);
        }

        // Pass 2: Define all functions (relocations can now resolve forward references)
        for (compiled_funcs, 0..) |*cf, i| {
            // Check for native overrides (exported runtime stubs replaced with ARM64 syscalls)
            var override_name: ?[]const u8 = null;
            for (exports) |exp| {
                if (exp.kind == .func and exp.index == i) {
                    if (std.mem.eql(u8, exp.name, "cot_write") or
                        std.mem.eql(u8, exp.name, "cot_fd_write_simple") or
                        std.mem.eql(u8, exp.name, "cot_fd_read_simple") or
                        std.mem.eql(u8, exp.name, "cot_fd_close") or
                        std.mem.eql(u8, exp.name, "cot_fd_seek") or
                        std.mem.eql(u8, exp.name, "cot_fd_open") or
                        std.mem.eql(u8, exp.name, "cot_time") or
                        std.mem.eql(u8, exp.name, "cot_random") or
                        std.mem.eql(u8, exp.name, "cot_exit") or
                        std.mem.eql(u8, exp.name, "wasi_fd_write") or
                        std.mem.eql(u8, exp.name, "cot_args_count") or
                        std.mem.eql(u8, exp.name, "cot_arg_len") or
                        std.mem.eql(u8, exp.name, "cot_arg_ptr"))
                    {
                        override_name = exp.name;
                        break;
                    }
                }
            }
            if (override_name) |name| {
                if (std.mem.eql(u8, name, "cot_write") or std.mem.eql(u8, name, "cot_fd_write_simple")) {
                    // ARM64 macOS syscall for write(fd, ptr, len)
                    // Cranelift CC: x0=vmctx, x1=caller_vmctx, x2=fd, x3=ptr, x4=len
                    // Reference: Go syscall1 on Darwin ARM64 — BCC/NEG pattern for error handling
                    // On success: x0 = bytes written. On error: x0 = -errno (negative).
                    const arm64_write = [_]u8{
                        0xFD, 0x7B, 0xBF, 0xA9, // stp x29, x30, [sp, #-16]!
                        0xFD, 0x03, 0x00, 0x91, // mov x29, sp
                        0x08, 0x00, 0x41, 0x91, // add x8, x0, #0x40, lsl #12  (vmctx + 0x40000)
                        0x01, 0x01, 0x03, 0x8B, // add x1, x8, x3  (real_ptr = linmem + wasm_ptr)
                        0xE0, 0x03, 0x02, 0xAA, // mov x0, x2  (fd)
                        0xE2, 0x03, 0x04, 0xAA, // mov x2, x4  (len)
                        0x90, 0x00, 0x80, 0xD2, // mov x16, #4  (SYS_write)
                        0x01, 0x10, 0x00, 0xD4, // svc #0x80
                        0x43, 0x00, 0x00, 0x54, // b.cc +2     (carry clear = success, skip neg)
                        0xE0, 0x03, 0x00, 0xCB, // neg x0, x0  (error: negate errno)
                        0xFD, 0x7B, 0xC1, 0xA8, // ldp x29, x30, [sp], #16
                        0xC0, 0x03, 0x5F, 0xD6, // ret
                    };
                    try module.defineFunctionBytes(func_ids[i], &arm64_write, &.{});
                } else if (std.mem.eql(u8, name, "cot_fd_read_simple")) {
                    // ARM64 macOS syscall for read(fd, buf, len)
                    // Cranelift CC: x0=vmctx, x1=caller_vmctx, x2=fd, x3=buf, x4=len
                    // Reference: Go syscall1 on Darwin ARM64 — BCC/NEG pattern
                    // On success: x0 = bytes read (0 = EOF). On error: x0 = -errno.
                    const arm64_read = [_]u8{
                        0xFD, 0x7B, 0xBF, 0xA9, // stp x29, x30, [sp, #-16]!
                        0xFD, 0x03, 0x00, 0x91, // mov x29, sp
                        0x08, 0x00, 0x41, 0x91, // add x8, x0, #0x40, lsl #12  (vmctx + 0x40000)
                        0x01, 0x01, 0x03, 0x8B, // add x1, x8, x3  (real_buf = linmem + wasm_ptr)
                        0xE0, 0x03, 0x02, 0xAA, // mov x0, x2  (fd)
                        0xE2, 0x03, 0x04, 0xAA, // mov x2, x4  (len)
                        0x70, 0x00, 0x80, 0xD2, // mov x16, #3  (SYS_read)
                        0x01, 0x10, 0x00, 0xD4, // svc #0x80
                        0x43, 0x00, 0x00, 0x54, // b.cc +2     (carry clear = success, skip neg)
                        0xE0, 0x03, 0x00, 0xCB, // neg x0, x0  (error: negate errno)
                        0xFD, 0x7B, 0xC1, 0xA8, // ldp x29, x30, [sp], #16
                        0xC0, 0x03, 0x5F, 0xD6, // ret
                    };
                    try module.defineFunctionBytes(func_ids[i], &arm64_read, &.{});
                } else if (std.mem.eql(u8, name, "cot_fd_close")) {
                    // ARM64 macOS syscall for close(fd)
                    // Cranelift CC: x0=vmctx, x1=caller_vmctx, x2=fd
                    // Reference: Go syscall1 on Darwin ARM64 — BCC/NEG pattern
                    // On success: x0 = 0. On error: x0 = -errno.
                    const arm64_close = [_]u8{
                        0xFD, 0x7B, 0xBF, 0xA9, // stp x29, x30, [sp, #-16]!
                        0xFD, 0x03, 0x00, 0x91, // mov x29, sp
                        0xE0, 0x03, 0x02, 0xAA, // mov x0, x2  (fd)
                        0xD0, 0x00, 0x80, 0xD2, // mov x16, #6  (SYS_close)
                        0x01, 0x10, 0x00, 0xD4, // svc #0x80
                        0x43, 0x00, 0x00, 0x54, // b.cc +2     (carry clear = success, skip neg)
                        0xE0, 0x03, 0x00, 0xCB, // neg x0, x0  (error: negate errno)
                        0xFD, 0x7B, 0xC1, 0xA8, // ldp x29, x30, [sp], #16
                        0xC0, 0x03, 0x5F, 0xD6, // ret
                    };
                    try module.defineFunctionBytes(func_ids[i], &arm64_close, &.{});
                } else if (std.mem.eql(u8, name, "cot_fd_seek")) {
                    // ARM64 macOS syscall for lseek(fd, offset, whence)
                    // Cranelift CC: x0=vmctx, x1=caller_vmctx, x2=fd, x3=offset, x4=whence
                    // Reference: Go syscall1 on Darwin ARM64 — BCC/NEG pattern
                    // On success: x0 = new offset. On error: x0 = -errno.
                    const arm64_seek = [_]u8{
                        0xFD, 0x7B, 0xBF, 0xA9, // stp x29, x30, [sp, #-16]!
                        0xFD, 0x03, 0x00, 0x91, // mov x29, sp
                        0xE0, 0x03, 0x02, 0xAA, // mov x0, x2  (fd)
                        0xE1, 0x03, 0x03, 0xAA, // mov x1, x3  (offset, i64)
                        0xE2, 0x03, 0x04, 0xAA, // mov x2, x4  (whence)
                        0xF0, 0x18, 0x80, 0xD2, // mov x16, #199  (SYS_lseek)
                        0x01, 0x10, 0x00, 0xD4, // svc #0x80
                        0x43, 0x00, 0x00, 0x54, // b.cc +2     (carry clear = success, skip neg)
                        0xE0, 0x03, 0x00, 0xCB, // neg x0, x0  (error: negate errno)
                        0xFD, 0x7B, 0xC1, 0xA8, // ldp x29, x30, [sp], #16
                        0xC0, 0x03, 0x5F, 0xD6, // ret
                    };
                    try module.defineFunctionBytes(func_ids[i], &arm64_seek, &.{});
                } else if (std.mem.eql(u8, name, "cot_fd_open")) {
                    // ARM64 macOS syscall for openat(AT_FDCWD, path, flags, mode)
                    // Cranelift CC: x0=vmctx, x1=caller_vmctx, x2=path_ptr, x3=path_len, x4=flags
                    // Reference: Go zsyscall_darwin_arm64.go openat() + BCC/NEG pattern
                    // On success: x0 = fd. On error: x0 = -errno.
                    // Must null-terminate path: copy to stack buffer, append \0
                    const arm64_open = [_]u8{
                        0xFD, 0x7B, 0xBF, 0xA9, // stp x29, x30, [sp, #-16]!
                        0xFD, 0x03, 0x00, 0x91, // mov x29, sp
                        0x08, 0x00, 0x41, 0x91, // add x8, x0, #0x40, lsl #12  (linmem base)
                        0x09, 0x01, 0x02, 0x8B, // add x9, x8, x2  (real path = linmem + path_ptr)
                        0xE5, 0x03, 0x04, 0xAA, // mov x5, x4  (save flags before clobbering)
                        0xFF, 0x43, 0x10, 0xD1, // sub sp, sp, #1040  (stack buf: 1024 + 16 align)
                        0xEB, 0x03, 0x00, 0x91, // mov x11, sp  (dst = stack buffer)
                        0xEC, 0x03, 0x03, 0xAA, // mov x12, x3  (counter = path_len)
                        // .copy_loop:
                        0xAC, 0x00, 0x00, 0xB4, // cbz x12, +5  (skip to null-term if empty)
                        0x2D, 0x15, 0x40, 0x38, // ldrb w13, [x9], #1  (load byte, post-inc)
                        0x6D, 0x15, 0x00, 0x38, // strb w13, [x11], #1  (store byte, post-inc)
                        0x8C, 0x05, 0x00, 0xD1, // sub x12, x12, #1
                        0xFC, 0xFF, 0xFF, 0x17, // b -4  (back to cbz)
                        // .copy_done:
                        0x7F, 0x01, 0x00, 0x39, // strb wzr, [x11]  (null terminate)
                        // openat(AT_FDCWD, path, flags, mode)
                        0x60, 0x0C, 0x80, 0x92, // movn x0, #99  (AT_FDCWD = -100)
                        0xE1, 0x03, 0x00, 0x91, // mov x1, sp  (null-terminated path on stack)
                        0xE2, 0x03, 0x05, 0xAA, // mov x2, x5  (flags, saved earlier)
                        0x83, 0x34, 0x80, 0xD2, // movz x3, #420  (mode = 0644)
                        0xF0, 0x39, 0x80, 0xD2, // movz x16, #463  (SYS_openat)
                        0x01, 0x10, 0x00, 0xD4, // svc #0x80
                        0x43, 0x00, 0x00, 0x54, // b.cc +2     (carry clear = success, skip neg)
                        0xE0, 0x03, 0x00, 0xCB, // neg x0, x0  (error: negate errno)
                        // Restore stack
                        0xBF, 0x03, 0x00, 0x91, // mov sp, x29  (restore from frame pointer)
                        0xFD, 0x7B, 0xC1, 0xA8, // ldp x29, x30, [sp], #16
                        0xC0, 0x03, 0x5F, 0xD6, // ret
                    };
                    try module.defineFunctionBytes(func_ids[i], &arm64_open, &.{});
                } else if (std.mem.eql(u8, name, "cot_time")) {
                    // ARM64 macOS: gettimeofday → convert to nanoseconds since epoch
                    // Cranelift CC: x0=vmctx, x1=caller_vmctx (no user args)
                    // Returns: i64 nanoseconds = tv_sec * 1_000_000_000 + tv_usec * 1_000
                    // Reference: Go runtime/sys_darwin_arm64.s walltime_trampoline
                    // Note: Go doesn't check gettimeofday errors either — it never fails in practice.
                    const arm64_time = [_]u8{
                        0xFD, 0x7B, 0xBE, 0xA9, // stp x29, x30, [sp, #-32]!  (save FP/LR + 16 bytes for timeval)
                        0xFD, 0x03, 0x00, 0x91, // mov x29, sp
                        0xE0, 0x43, 0x00, 0x91, // add x0, sp, #16             (x0 = &timeval at sp+16)
                        0x01, 0x00, 0x80, 0xD2, // movz x1, #0                 (tz = NULL)
                        0x90, 0x0E, 0x80, 0xD2, // movz x16, #116              (SYS_gettimeofday)
                        0x01, 0x10, 0x00, 0xD4, // svc #0x80
                        0xE8, 0x0B, 0x40, 0xF9, // ldr x8, [sp, #16]           (tv_sec: i64)
                        0xE9, 0x1B, 0x40, 0xB9, // ldr w9, [sp, #24]           (tv_usec: i32, zero-ext)
                        0x0A, 0x40, 0x99, 0xD2, // movz x10, #0xCA00
                        0x4A, 0x73, 0xA7, 0xF2, // movk x10, #0x3B9A, lsl #16 (x10 = 1_000_000_000)
                        0x00, 0x7D, 0x0A, 0x9B, // mul x0, x8, x10             (tv_sec * 1B)
                        0x0B, 0x7D, 0x80, 0xD2, // movz x11, #1000
                        0x20, 0x01, 0x0B, 0x9B, // madd x0, x9, x11, x0        (+ tv_usec * 1000)
                        0xFD, 0x7B, 0xC2, 0xA8, // ldp x29, x30, [sp], #32
                        0xC0, 0x03, 0x5F, 0xD6, // ret
                    };
                    try module.defineFunctionBytes(func_ids[i], &arm64_time, &.{});
                } else if (std.mem.eql(u8, name, "cot_random")) {
                    // ARM64 macOS: getentropy(buf, len) — fill buffer with random bytes
                    // Cranelift CC: x0=vmctx, x1=caller_vmctx, x2=buf(wasm ptr), x3=len
                    // Reference: Go syscall1 on Darwin ARM64 — BCC/NEG pattern
                    // On success: x0 = 0. On error: x0 = -errno.
                    const arm64_random = [_]u8{
                        0xFD, 0x7B, 0xBF, 0xA9, // stp x29, x30, [sp, #-16]!
                        0xFD, 0x03, 0x00, 0x91, // mov x29, sp
                        0x08, 0x00, 0x41, 0x91, // add x8, x0, #0x40, lsl #12  (linmem base)
                        0x00, 0x01, 0x02, 0x8B, // add x0, x8, x2              (real buf = linmem + wasm_ptr)
                        0xE1, 0x03, 0x03, 0xAA, // mov x1, x3                  (len)
                        0x90, 0x3E, 0x80, 0xD2, // movz x16, #500              (SYS_getentropy)
                        0x01, 0x10, 0x00, 0xD4, // svc #0x80
                        0x43, 0x00, 0x00, 0x54, // b.cc +2     (carry clear = success, skip neg)
                        0xE0, 0x03, 0x00, 0xCB, // neg x0, x0  (error: negate errno)
                        0xFD, 0x7B, 0xC1, 0xA8, // ldp x29, x30, [sp], #16
                        0xC0, 0x03, 0x5F, 0xD6, // ret
                    };
                    try module.defineFunctionBytes(func_ids[i], &arm64_random, &.{});
                } else if (std.mem.eql(u8, name, "cot_exit")) {
                    // ARM64 macOS: exit(code) — never returns
                    // Cranelift CC: x0=vmctx, x1=caller_vmctx, x2=code
                    // Reference: WASI proc_exit, Go runtime/sys_darwin_arm64.s exit_trampoline
                    const arm64_exit = [_]u8{
                        0xE0, 0x03, 0x02, 0xAA, // mov x0, x2              (exit code)
                        0x30, 0x00, 0x80, 0xD2, // movz x16, #1            (SYS_exit)
                        0x01, 0x10, 0x00, 0xD4, // svc #0x80
                    };
                    try module.defineFunctionBytes(func_ids[i], &arm64_exit, &.{});
                } else if (std.mem.eql(u8, name, "wasi_fd_write")) {
                    // ARM64 macOS syscall for WASI fd_write(fd, iovs, iovs_len, nwritten)
                    // Cranelift CC: x0=vmctx, x1=caller_vmctx, x2=fd, x3=iovs, x4=iovs_len, x5=nwritten
                    // Reference: Go syscall1 BCC pattern. On error, skip nwritten store, return errno.
                    const arm64_fd_write = [_]u8{
                        0xFD, 0x7B, 0xBF, 0xA9, // stp x29, x30, [sp, #-16]!
                        0xFD, 0x03, 0x00, 0x91, // mov x29, sp
                        0x08, 0x00, 0x41, 0x91, // add x8, x0, #0x40, lsl #12  (x8 = linmem base)
                        // Read iovec[0] from linmem (fast path: adapter always sends 1 iovec)
                        0x09, 0x01, 0x23, 0x8B, // add x9, x8, w3, uxtw  (x9 = linmem + iovs)
                        0x2A, 0x01, 0x40, 0xB9, // ldr w10, [x9]         (w10 = iovec.buf)
                        0x2B, 0x05, 0x40, 0xB9, // ldr w11, [x9, #4]     (w11 = iovec.buf_len)
                        // SYS_write(fd, real_ptr, len)
                        0xE0, 0x03, 0x02, 0x2A, // mov w0, w2            (fd, truncate to i32)
                        0x01, 0x01, 0x2A, 0x8B, // add x1, x8, w10, uxtw (real_ptr = linmem + buf)
                        0xE2, 0x03, 0x0B, 0x2A, // mov w2, w11           (len = buf_len, zero-ext)
                        0x90, 0x00, 0x80, 0xD2, // mov x16, #4           (SYS_write)
                        0x01, 0x10, 0x00, 0xD4, // svc #0x80
                        0x82, 0x00, 0x00, 0x54, // b.cs +4     (carry set = error, skip store, ret errno)
                        // Success: store bytes_written, return ESUCCESS
                        0x09, 0x01, 0x25, 0x8B, // add x9, x8, w5, uxtw  (linmem + nwritten)
                        0x20, 0x01, 0x00, 0xB9, // str w0, [x9]          (store result as i32)
                        0xE0, 0x03, 0x1F, 0xAA, // mov x0, xzr           (return 0)
                        // Error: x0 = macOS errno (falls through to epilogue)
                        0xFD, 0x7B, 0xC1, 0xA8, // ldp x29, x30, [sp], #16
                        0xC0, 0x03, 0x5F, 0xD6, // ret
                    };
                    try module.defineFunctionBytes(func_ids[i], &arm64_fd_write, &.{});
                } else if (std.mem.eql(u8, name, "cot_args_count")) {
                    // ARM64: read argc from vmctx+0x30000
                    // Cranelift CC: x0=vmctx, x1=caller_vmctx (no user args)
                    // vmctx+0x30000 = argc (stored by _main wrapper)
                    const arm64_args_count = [_]u8{
                        0x08, 0xC0, 0x40, 0x91, // add x8, x0, #0x30, lsl #12  (vmctx + 0x30000)
                        0x00, 0x01, 0x40, 0xF9, // ldr x0, [x8]                (x0 = argc)
                        0xC0, 0x03, 0x5F, 0xD6, // ret
                    };
                    try module.defineFunctionBytes(func_ids[i], &arm64_args_count, &.{});
                } else if (std.mem.eql(u8, name, "cot_arg_len")) {
                    // ARM64: strlen(argv[n]) with bounds check
                    // Cranelift CC: x0=vmctx, x1=caller_vmctx, x2=n
                    // Reference: Go goenvs() validates argc before accessing argv
                    // Returns 0 if n >= argc (out of bounds).
                    const arm64_arg_len = [_]u8{
                        0x08, 0xC0, 0x40, 0x91, // add x8, x0, #0x30, lsl #12  (vmctx + 0x30000)
                        0x09, 0x01, 0x40, 0xF9, // ldr x9, [x8]                (x9 = argc)
                        0x5F, 0x00, 0x09, 0xEB, // cmp x2, x9                  (n vs argc)
                        0x63, 0x00, 0x00, 0x54, // b.lo +3                     (n < argc → .valid)
                        0x00, 0x00, 0x80, 0xD2, // movz x0, #0                 (out of bounds)
                        0xC0, 0x03, 0x5F, 0xD6, // ret
                        // .valid:
                        0x09, 0x05, 0x40, 0xF9, // ldr x9, [x8, #8]            (x9 = argv)
                        0x29, 0x79, 0x62, 0xF8, // ldr x9, [x9, x2, lsl #3]    (x9 = argv[n])
                        0x00, 0x00, 0x80, 0xD2, // movz x0, #0                 (len = 0)
                        // .strlen_loop:
                        0x2A, 0x69, 0x60, 0x38, // ldrb w10, [x9, x0]          (byte at argv[n][len])
                        0x6A, 0x00, 0x00, 0x34, // cbz w10, +3                 (if null → ret)
                        0x00, 0x04, 0x00, 0x91, // add x0, x0, #1              (len++)
                        0xFD, 0xFF, 0xFF, 0x17, // b -3                        (→ .strlen_loop)
                        // .done:
                        0xC0, 0x03, 0x5F, 0xD6, // ret                         (x0 = strlen)
                    };
                    try module.defineFunctionBytes(func_ids[i], &arm64_arg_len, &.{});
                } else if (std.mem.eql(u8, name, "cot_arg_ptr")) {
                    // ARM64: copy argv[n] into linear memory at wasm offset 0xAF000, with bounds check
                    // Cranelift CC: x0=vmctx, x1=caller_vmctx, x2=n
                    // Reference: Go goenvs() validates argc before accessing argv
                    // Returns 0 if n >= argc (out of bounds).
                    // Dest: vmctx + 0x40000 (linmem) + 0xAF000 = vmctx + 0xEF000
                    const arm64_arg_ptr = [_]u8{
                        0xFD, 0x7B, 0xBF, 0xA9, // stp x29, x30, [sp, #-16]!
                        0xFD, 0x03, 0x00, 0x91, // mov x29, sp
                        0x08, 0xC0, 0x40, 0x91, // add x8, x0, #0x30, lsl #12  (vmctx + 0x30000)
                        0x09, 0x01, 0x40, 0xF9, // ldr x9, [x8]                (x9 = argc)
                        0x5F, 0x00, 0x09, 0xEB, // cmp x2, x9                  (n vs argc)
                        0x83, 0x00, 0x00, 0x54, // b.lo +4                     (n < argc → .valid)
                        0x00, 0x00, 0x80, 0xD2, // movz x0, #0                 (out of bounds)
                        0xFD, 0x7B, 0xC1, 0xA8, // ldp x29, x30, [sp], #16
                        0xC0, 0x03, 0x5F, 0xD6, // ret
                        // .valid:
                        0x09, 0x05, 0x40, 0xF9, // ldr x9, [x8, #8]            (x9 = argv)
                        0x29, 0x79, 0x62, 0xF8, // ldr x9, [x9, x2, lsl #3]    (x9 = argv[n], src)
                        0x0A, 0x00, 0x41, 0x91, // add x10, x0, #0x40, lsl #12 (linmem base)
                        0x4A, 0xBD, 0x42, 0x91, // add x10, x10, #0xAF, lsl #12 (+ 0xAF000 = dest)
                        // .copy_loop:
                        0x2B, 0x15, 0x40, 0x38, // ldrb w11, [x9], #1          (load byte, post-inc)
                        0x4B, 0x15, 0x00, 0x38, // strb w11, [x10], #1         (store byte, post-inc)
                        0xCB, 0xFF, 0xFF, 0x35, // cbnz w11, -2                (→ .copy_loop)
                        // .done: return wasm offset 0xAF000
                        0x00, 0x00, 0x9E, 0xD2, // movz x0, #0xF000
                        0x40, 0x01, 0xA0, 0xF2, // movk x0, #0xA, lsl #16     (x0 = 0xAF000)
                        0xFD, 0x7B, 0xC1, 0xA8, // ldp x29, x30, [sp], #16
                        0xC0, 0x03, 0x5F, 0xD6, // ret
                    };
                    try module.defineFunctionBytes(func_ids[i], &arm64_arg_ptr, &.{});
                }
            } else {
                try module.defineFunction(func_ids[i], cf);
            }
        }

        // If we have a main function, generate the wrapper and static vmctx
        if (main_func_index != null) {
            try self.generateMainWrapperMachO(&module, data_segments, globals, @intCast(compiled_funcs.len));
        }

        // Write to memory buffer
        var output = std.ArrayListUnmanaged(u8){};
        defer output.deinit(self.allocator);
        try module.finish(output.writer(self.allocator));

        return output.toOwnedSlice(self.allocator);
    }

    /// Generate the _main wrapper and static vmctx data for Mach-O.
    /// The wrapper initializes vmctx and calls __wasm_main.
    fn generateMainWrapperMachO(self: *Driver, module: *object_module.ObjectModule, data_segments: []const wasm_parser.DataSegment, globals: []const wasm_parser.GlobalType, num_funcs: u32) !void {
        // =================================================================
        // Step 1: Declare and define static vmctx data section
        // Layout (total 16MB = 0x1000000):
        //   0x00000 - 0x0FFFF: Padding/reserved
        //   0x10000: Stack pointer global (i32) - initialized to 64KB
        //   0x20000: Heap base pointer (i64) - to be filled by wrapper
        //   0x20008: Heap bound (i64) - ~15.7MB
        //   0x30000: argc (i64) - set by _main wrapper from OS
        //   0x30008: argv (i64, char**) - set by _main wrapper from OS
        //   0x40000 - 0xFFFFFF: Linear memory (~15.7MB heap)
        //     0xEF000 (linmem+0xAF000): arg string buffer for @arg_ptr
        // =================================================================
        const vmctx_size: usize = 0x1000000; // 16MB
        const vmctx_data = try self.allocator.alloc(u8, vmctx_size);
        defer self.allocator.free(vmctx_data);

        // Zero initialize
        @memset(vmctx_data, 0);

        // Copy Wasm data segments into linear memory (starts at 0x40000)
        const linear_memory_base: usize = 0x40000;
        for (data_segments) |segment| {
            const dest_offset = linear_memory_base + segment.offset;
            if (dest_offset + segment.data.len <= vmctx_size) {
                @memcpy(vmctx_data[dest_offset..][0..segment.data.len], segment.data);
            }
        }

        // Initialize globals at offset 0x10000 with fixed 16-byte stride.
        // Reference: Cranelift vmoffsets.rs — VMGlobalDefinition is 16 bytes,
        // and initialize_globals (allocator.rs:754-808) writes init values
        // to vmctx before execution starts.
        // Global 0 (SP) init_value comes from Wasm global section.
        const global_base: usize = 0x10000;
        const global_stride: usize = 16;
        for (globals, 0..) |g, i| {
            const offset = global_base + i * global_stride;
            if (offset + 8 <= vmctx_size) {
                switch (g.val_type) {
                    .i32 => {
                        const val: u32 = @bitCast(@as(i32, @truncate(g.init_value)));
                        @memcpy(vmctx_data[offset..][0..4], std.mem.asBytes(&val));
                    },
                    .i64 => {
                        const val: u64 = @bitCast(g.init_value);
                        @memcpy(vmctx_data[offset..][0..8], std.mem.asBytes(&val));
                    },
                    .f32 => {
                        const val: u32 = @bitCast(@as(i32, @truncate(g.init_value)));
                        @memcpy(vmctx_data[offset..][0..4], std.mem.asBytes(&val));
                    },
                    .f64 => {
                        const val: u64 = @bitCast(g.init_value);
                        @memcpy(vmctx_data[offset..][0..8], std.mem.asBytes(&val));
                    },
                    else => {},
                }
            }
        }

        // Heap bound at offset 0x20008 = ~15.7MB (size of linear memory)
        const heap_bound: u64 = 0x1000000 - 0x40000; // vmctx_size - linear_memory_base
        @memcpy(vmctx_data[0x20008..][0..8], std.mem.asBytes(&heap_bound));

        // Note: heap base pointer at 0x20000 will be patched by wrapper at runtime
        // because we need the absolute address which isn't known until load time

        const vmctx_data_id = try module.declareData("_vmctx_data", .Local, true);
        try module.defineData(vmctx_data_id, vmctx_data);

        // =================================================================
        // Step 2: Generate _main wrapper function
        // This wrapper:
        //   1. Saves argc (x0) and argv (x1) from macOS C runtime
        //   2. Loads address of _vmctx_data
        //   3. Initializes heap base pointer (requires runtime address)
        //   4. Stores argc/argv at vmctx+0x30000/0x30008
        //   5. Calls __wasm_main(vmctx, vmctx)
        //   6. Returns result
        //
        // ARM64 code (18 instructions, 72 bytes):
        //   stp     x29, x30, [sp, #-32]!
        //   mov     x29, sp
        //   stp     x19, x20, [sp, #16]     ; save callee-saved regs
        //   mov     x19, x0                 ; save argc
        //   mov     x20, x1                 ; save argv
        //   adrp    x0, _vmctx_data@PAGE    (reloc at offset 20)
        //   add     x0, x0, _vmctx_data@PAGEOFF (reloc at offset 24)
        //   add     x8, x0, #0x20, lsl #12  ; x8 = vmctx + 0x20000
        //   add     x9, x0, #0x40, lsl #12  ; x9 = vmctx + 0x40000 (heap base)
        //   str     x9, [x8]                ; Store heap base ptr
        //   add     x10, x0, #0x30, lsl #12 ; x10 = vmctx + 0x30000 (args area)
        //   str     x19, [x10]              ; Store argc
        //   str     x20, [x10, #8]          ; Store argv
        //   mov     x1, x0                  ; caller_vmctx = vmctx
        //   bl      __wasm_main             (reloc at offset 56)
        //   ldp     x19, x20, [sp, #16]     ; restore callee-saved regs
        //   ldp     x29, x30, [sp], #32
        //   ret
        // =================================================================
        var wrapper_code = std.ArrayListUnmanaged(u8){};
        defer wrapper_code.deinit(self.allocator);

        // Helper to append little-endian u32
        const appendInst = struct {
            fn f(list: *std.ArrayListUnmanaged(u8), alloc: std.mem.Allocator, inst: u32) !void {
                try list.appendSlice(alloc, &std.mem.toBytes(std.mem.nativeToBig(u32, @byteSwap(inst))));
            }
        }.f;

        // stp x29, x30, [sp, #-32]! (save frame, 32 bytes for x19/x20)
        try appendInst(&wrapper_code, self.allocator, 0xA9BE7BFD);
        // mov x29, sp
        try appendInst(&wrapper_code, self.allocator, 0x910003FD);
        // stp x19, x20, [sp, #16] (save callee-saved regs)
        try appendInst(&wrapper_code, self.allocator, 0xA90153F3);
        // mov x19, x0 (save argc from OS)
        try appendInst(&wrapper_code, self.allocator, 0xAA0003F3);
        // mov x20, x1 (save argv from OS)
        try appendInst(&wrapper_code, self.allocator, 0xAA0103F4);
        // adrp x0, _vmctx_data@PAGE (reloc at offset 20)
        try appendInst(&wrapper_code, self.allocator, 0x90000000);
        // add x0, x0, _vmctx_data@PAGEOFF (reloc at offset 24)
        try appendInst(&wrapper_code, self.allocator, 0x91000000);
        // add x8, x0, #0x20, lsl #12 (x8 = vmctx + 0x20000)
        try appendInst(&wrapper_code, self.allocator, 0x91408008);
        // add x9, x0, #0x40, lsl #12 (x9 = vmctx + 0x40000 = heap base)
        try appendInst(&wrapper_code, self.allocator, 0x91410009);
        // str x9, [x8] (store heap base pointer)
        try appendInst(&wrapper_code, self.allocator, 0xF9000109);
        // add x10, x0, #0x30, lsl #12 (x10 = vmctx + 0x30000, args area)
        try appendInst(&wrapper_code, self.allocator, 0x9140C00A);
        // str x19, [x10] (store argc)
        try appendInst(&wrapper_code, self.allocator, 0xF9000153);
        // str x20, [x10, #8] (store argv)
        try appendInst(&wrapper_code, self.allocator, 0xF9000554);
        // mov x1, x0 (caller_vmctx = vmctx)
        try appendInst(&wrapper_code, self.allocator, 0xAA0003E1);
        // bl __wasm_main (reloc at offset 56)
        try appendInst(&wrapper_code, self.allocator, 0x94000000);
        // ldp x19, x20, [sp, #16] (restore callee-saved regs)
        try appendInst(&wrapper_code, self.allocator, 0xA94153F3);
        // ldp x29, x30, [sp], #32
        try appendInst(&wrapper_code, self.allocator, 0xA8C27BFD);
        // ret
        try appendInst(&wrapper_code, self.allocator, 0xD65F03C0);

        // Declare _main wrapper
        const main_func_id = try module.declareFunction("_main", .Export);

        // Create relocations for the wrapper code
        // Relocation offsets: ADRP at 8, ADD at 12, BL at 32
        const FinalizedMachReloc = buffer_mod.FinalizedMachReloc;
        const FinalizedRelocTarget = buffer_mod.FinalizedRelocTarget;
        const Reloc = buffer_mod.Reloc;
        const ExternalName = buffer_mod.ExternalName;

        // Use indices after all function indices to avoid collision
        // (functions are indexed 0..num_funcs-1)
        const vmctx_ext_idx: u32 = num_funcs;
        const wasm_main_ext_idx: u32 = num_funcs + 1;

        // Create external name references for _vmctx_data and __wasm_main
        const vmctx_name_ref = ExternalName{ .User = .{ .namespace = 0, .index = vmctx_ext_idx } };
        const wasm_main_name_ref = ExternalName{ .User = .{ .namespace = 0, .index = wasm_main_ext_idx } };

        // Register external names for relocation resolution (indices must match the refs above)
        try module.declareExternalName(vmctx_ext_idx, "_vmctx_data");
        try module.declareExternalName(wasm_main_ext_idx, "__wasm_main");

        const relocs = [_]FinalizedMachReloc{
            // ADRP x0, _vmctx_data@PAGE at offset 20 (after 5 setup instrs)
            .{
                .offset = 20,
                .kind = Reloc.Aarch64AdrPrelPgHi21,
                .target = FinalizedRelocTarget{ .ExternalName = vmctx_name_ref },
                .addend = 0,
            },
            // ADD x0, x0, _vmctx_data@PAGEOFF at offset 24
            .{
                .offset = 24,
                .kind = Reloc.Aarch64AddAbsLo12Nc,
                .target = FinalizedRelocTarget{ .ExternalName = vmctx_name_ref },
                .addend = 0,
            },
            // BL __wasm_main at offset 56
            .{
                .offset = 56,
                .kind = Reloc.Arm64Call,
                .target = FinalizedRelocTarget{ .ExternalName = wasm_main_name_ref },
                .addend = 0,
            },
        };

        try module.defineFunctionBytes(main_func_id, wrapper_code.items, &relocs);
    }

    /// Generate ELF object file from compiled functions.
    /// Uses ObjectModule to bridge CompiledCode to ELF format.
    fn generateElf(self: *Driver, compiled_funcs: []const native_compile.CompiledCode, exports: []const wasm_parser.Export, data_segments: []const wasm_parser.DataSegment, globals: []const wasm_parser.GlobalType) ![]u8 {
        var module = object_module.ObjectModule.initWithTarget(
            self.allocator,
            .linux,
            .x86_64,
        );
        defer module.deinit();

        // Build export name map for function lookup
        var export_names = std.StringHashMap(u32).init(self.allocator);
        defer export_names.deinit();
        for (exports, 0..) |exp, i| {
            if (exp.kind == .func) {
                try export_names.put(exp.name, @intCast(i));
            }
        }

        // Track if we need to generate a main wrapper
        var main_func_index: ?usize = null;

        // Pass 1: Declare all functions and external names (Cranelift pattern)
        var elf_func_ids = try self.allocator.alloc(object_module.FuncId, compiled_funcs.len);
        defer self.allocator.free(elf_func_ids);

        for (compiled_funcs, 0..) |_, i| {
            var func_name: []const u8 = "";
            var func_name_allocated = false;
            for (exports) |exp| {
                if (exp.kind == .func and exp.index == i) {
                    func_name = exp.name;
                    break;
                }
            }
            if (func_name.len == 0) {
                func_name = try std.fmt.allocPrint(self.allocator, "func_{d}", .{i});
                func_name_allocated = true;
            }
            defer if (func_name_allocated) self.allocator.free(func_name);

            const is_main = std.mem.eql(u8, func_name, "main");
            if (is_main) {
                main_func_index = i;
            }

            const mangled_name = if (is_main)
                "__wasm_main"
            else
                func_name;

            const linkage: object_module.Linkage = if (is_main)
                .Local
            else if (func_name_allocated)
                .Local
            else
                .Export;

            elf_func_ids[i] = try module.declareFunction(mangled_name, linkage);
            try module.declareExternalName(@intCast(i), mangled_name);
        }

        // Pass 2: Define all functions (relocations can now resolve forward references)
        for (compiled_funcs, 0..) |*cf, i| {
            // Check if this is cot_write — replace with x86-64 Linux syscall
            var is_cot_write = false;
            for (exports) |exp| {
                if (exp.kind == .func and exp.index == i and std.mem.eql(u8, exp.name, "cot_write")) {
                    is_cot_write = true;
                    break;
                }
            }
            if (is_cot_write) {
                // x86-64 Linux syscall for write(fd, ptr, len)
                // Cranelift CC: rdi=vmctx, rsi=caller_vmctx, rdx=fd, rcx=ptr(wasm), r8=len
                // Linux write: rax=1(SYS_write), rdi=fd, rsi=buf, rdx=count
                const x64_write = [_]u8{
                    0x55, // push rbp
                    0x48, 0x89, 0xE5, // mov rbp, rsp
                    0x48, 0x8D, 0xB7, 0x00, 0x00, 0x04, 0x00, // lea rsi, [rdi + 0x40000]  (linmem base)
                    0x48, 0x01, 0xCE, // add rsi, rcx              (real_ptr = linmem + wasm_ptr)
                    0x48, 0x89, 0xD7, // mov rdi, rdx              (fd)
                    0x4C, 0x89, 0xC2, // mov rdx, r8               (len)
                    0x48, 0xC7, 0xC0, 0x01, 0x00, 0x00, 0x00, // mov rax, 1  (SYS_write)
                    0x0F, 0x05, // syscall
                    0x5D, // pop rbp
                    0xC3, // ret
                };
                try module.defineFunctionBytes(elf_func_ids[i], &x64_write, &.{});
            } else {
                try module.defineFunction(elf_func_ids[i], cf);
            }
        }

        // If we have a main function, generate the wrapper and static vmctx
        if (main_func_index != null) {
            try self.generateMainWrapperElf(&module, data_segments, globals, @intCast(compiled_funcs.len));
        }

        // Write to memory buffer
        var output = std.ArrayListUnmanaged(u8){};
        defer output.deinit(self.allocator);
        try module.finish(output.writer(self.allocator));

        return output.toOwnedSlice(self.allocator);
    }

    /// Generate the main wrapper and static vmctx data for ELF (x86-64 Linux).
    /// The wrapper initializes vmctx and calls __wasm_main.
    fn generateMainWrapperElf(self: *Driver, module: *object_module.ObjectModule, data_segments: []const wasm_parser.DataSegment, globals: []const wasm_parser.GlobalType, num_funcs: u32) !void {
        // =================================================================
        // Step 1: Declare and define static vmctx data section
        // Layout (total 16MB = 0x1000000):
        //   0x00000 - 0x0FFFF: Padding/reserved
        //   0x10000: Globals area (16-byte stride per global)
        //   0x20000: Heap base pointer (i64) - to be filled by wrapper
        //   0x20008: Heap bound (i64) - ~15.7MB
        //   0x30000: argc (i64) - set by _main wrapper from OS
        //   0x30008: argv (i64, char**) - set by _main wrapper from OS
        //   0x40000 - 0xFFFFFF: Linear memory (~15.7MB heap)
        //     0xEF000 (linmem+0xAF000): arg string buffer for @arg_ptr
        // =================================================================
        const vmctx_size: usize = 0x1000000; // 16MB
        const vmctx_data = try self.allocator.alloc(u8, vmctx_size);
        defer self.allocator.free(vmctx_data);

        // Zero initialize
        @memset(vmctx_data, 0);

        // Copy Wasm data segments into linear memory (starts at 0x40000)
        const linear_memory_base: usize = 0x40000;
        for (data_segments) |segment| {
            const dest_offset = linear_memory_base + segment.offset;
            if (dest_offset + segment.data.len <= vmctx_size) {
                @memcpy(vmctx_data[dest_offset..][0..segment.data.len], segment.data);
            }
        }

        // Initialize globals at offset 0x10000 with fixed 16-byte stride.
        // Reference: Cranelift vmoffsets.rs — VMGlobalDefinition is 16 bytes.
        const global_base: usize = 0x10000;
        const global_stride: usize = 16;
        for (globals, 0..) |g, i| {
            const offset = global_base + i * global_stride;
            if (offset + 8 <= vmctx_size) {
                switch (g.val_type) {
                    .i32 => {
                        const val: u32 = @bitCast(@as(i32, @truncate(g.init_value)));
                        @memcpy(vmctx_data[offset..][0..4], std.mem.asBytes(&val));
                    },
                    .i64 => {
                        const val: u64 = @bitCast(g.init_value);
                        @memcpy(vmctx_data[offset..][0..8], std.mem.asBytes(&val));
                    },
                    .f32 => {
                        const val: u32 = @bitCast(@as(i32, @truncate(g.init_value)));
                        @memcpy(vmctx_data[offset..][0..4], std.mem.asBytes(&val));
                    },
                    .f64 => {
                        const val: u64 = @bitCast(g.init_value);
                        @memcpy(vmctx_data[offset..][0..8], std.mem.asBytes(&val));
                    },
                    else => {},
                }
            }
        }

        // Heap bound at offset 0x20008 = ~15.7MB (size of linear memory)
        const heap_bound: u64 = 0x1000000 - 0x40000; // vmctx_size - linear_memory_base
        @memcpy(vmctx_data[0x20008..][0..8], std.mem.asBytes(&heap_bound));

        // Note: heap base pointer at 0x20000 will be patched by wrapper at runtime
        // because we need the absolute address which isn't known until load time

        const vmctx_data_id = try module.declareData("vmctx_data", .Local, true);
        try module.defineData(vmctx_data_id, vmctx_data);

        // =================================================================
        // Step 2: Generate main wrapper function
        // This wrapper:
        //   1. Loads address of vmctx_data
        //   2. Initializes heap base pointer (requires runtime address)
        //   3. Calls __wasm_main(vmctx, vmctx)
        //   4. Returns result
        //
        // x86-64 code (System V AMD64 ABI):
        //   push   rbp
        //   mov    rbp, rsp
        //   lea    rdi, [rip + vmctx_data]    ; vmctx in rdi (first arg)
        //   lea    rax, [rdi + 0x40000]       ; heap_base = vmctx + 0x40000
        //   mov    [rdi + 0x20000], rax       ; store heap_base at vmctx+0x20000
        //   mov    rsi, rdi                   ; caller_vmctx = vmctx (second arg)
        //   call   __wasm_main
        //   pop    rbp
        //   ret
        // =================================================================
        var wrapper_code = std.ArrayListUnmanaged(u8){};
        defer wrapper_code.deinit(self.allocator);

        // push rbp (0x55)
        try wrapper_code.append(self.allocator, 0x55);

        // mov rbp, rsp (48 89 e5)
        try wrapper_code.appendSlice(self.allocator, &[_]u8{ 0x48, 0x89, 0xe5 });

        // lea rdi, [rip + vmctx_data] (48 8d 3d XX XX XX XX) - reloc at offset 4+3=7
        // RIP-relative, displacement is at bytes 4-7 (offset 4)
        try wrapper_code.appendSlice(self.allocator, &[_]u8{ 0x48, 0x8d, 0x3d, 0x00, 0x00, 0x00, 0x00 });

        // lea rax, [rdi + 0x40000] (48 8d 87 00 00 04 00)
        try wrapper_code.appendSlice(self.allocator, &[_]u8{ 0x48, 0x8d, 0x87, 0x00, 0x00, 0x04, 0x00 });

        // mov [rdi + 0x20000], rax (48 89 87 00 00 02 00)
        try wrapper_code.appendSlice(self.allocator, &[_]u8{ 0x48, 0x89, 0x87, 0x00, 0x00, 0x02, 0x00 });

        // mov rsi, rdi (48 89 fe)
        try wrapper_code.appendSlice(self.allocator, &[_]u8{ 0x48, 0x89, 0xfe });

        // call __wasm_main (e8 XX XX XX XX) - reloc at offset 25+1=26
        try wrapper_code.appendSlice(self.allocator, &[_]u8{ 0xe8, 0x00, 0x00, 0x00, 0x00 });

        // pop rbp (5d)
        try wrapper_code.append(self.allocator, 0x5d);

        // ret (c3)
        try wrapper_code.append(self.allocator, 0xc3);

        // Declare main wrapper
        const main_func_id = try module.declareFunction("main", .Export);

        // Create relocations for the wrapper code
        const FinalizedMachReloc = buffer_mod.FinalizedMachReloc;
        const FinalizedRelocTarget = buffer_mod.FinalizedRelocTarget;
        const Reloc = buffer_mod.Reloc;
        const ExternalName = buffer_mod.ExternalName;

        // Use indices after all function indices to avoid collision
        const vmctx_ext_idx: u32 = num_funcs;
        const wasm_main_ext_idx: u32 = num_funcs + 1;

        // Create external name references for vmctx_data and __wasm_main
        const vmctx_name_ref = ExternalName{ .User = .{ .namespace = 0, .index = vmctx_ext_idx } };
        const wasm_main_name_ref = ExternalName{ .User = .{ .namespace = 0, .index = wasm_main_ext_idx } };

        // Register external names for relocation resolution
        try module.declareExternalName(vmctx_ext_idx, "vmctx_data");
        try module.declareExternalName(wasm_main_ext_idx, "__wasm_main");

        // Calculate relocation offsets based on instruction layout:
        // push rbp (1) + mov rbp,rsp (3) + lea opcode (3) = 7 for lea displacement
        // + lea (7) + lea (7) + mov (7) + mov (3) + call opcode (1) = 7+7+7+7+3+1 = 29 for call displacement
        const relocs = [_]FinalizedMachReloc{
            // lea rdi, [rip + vmctx_data] - displacement at offset 7
            .{
                .offset = 7,
                .kind = Reloc.X86PCRel4,
                .target = FinalizedRelocTarget{ .ExternalName = vmctx_name_ref },
                .addend = -4, // RIP-relative adjustment
            },
            // call __wasm_main - displacement at offset 29
            .{
                .offset = 29,
                .kind = Reloc.X86CallPCRel4,
                .target = FinalizedRelocTarget{ .ExternalName = wasm_main_name_ref },
                .addend = -4, // Call instruction adjustment
            },
        };

        try module.defineFunctionBytes(main_func_id, wrapper_code.items, &relocs);
    }

    /// Generate WebAssembly binary.
    /// Uses Go-style Linker for module structure with proper SP globals.
    fn generateWasmCode(self: *Driver, funcs: []const ir_mod.Func, type_reg: *types_mod.TypeRegistry) ![]u8 {
        pipeline_debug.log(.codegen, "driver: generating Wasm for {d} functions", .{funcs.len});

        var linker = wasm.Linker.init(self.allocator);
        defer linker.deinit();

        // Configure memory (3 pages = 192KB minimum)
        // Page 0: Stack (grows down from 64KB)
        // Page 1: Heap (starts at 64KB)
        // Page 2+: Globals (starts at 128KB / 0x20000)
        linker.setMemory(3, null);

        // ====================================================================
        // Add CTXT global (closure context pointer, Go: REG_CTXT = Global 1)
        // Must be before arc.addToLinker so heap_ptr shifts to global 2
        // Layout: SP(0), CTXT(1), heap_ptr(2)
        // ====================================================================
        _ = try linker.addGlobal(.{ .val_type = .i64, .mutable = true, .init_i64 = 0 });

        // ====================================================================
        // Add ARC runtime functions first (they get indices 0, 1, 2, ...)
        // Reference: Swift stdlib HeapObject.cpp
        // ====================================================================
        const arc_funcs = try arc.addToLinker(self.allocator, &linker);

        // ====================================================================
        // Add slice runtime functions (Go style)
        // Reference: Go runtime/slice.go
        // ====================================================================
        const slice_funcs = try slice_runtime.addToLinker(self.allocator, &linker, arc_funcs.heap_ptr_global);

        // ====================================================================
        // Add print runtime functions (Go style)
        // Reference: Go runtime/print.go
        // ====================================================================
        const print_funcs = try print_runtime.addToLinker(self.allocator, &linker);

        // ====================================================================
        // Add WASI runtime functions (fd_write)
        // Reference: WASI preview1 fd_write
        // ====================================================================
        const wasi_funcs = try wasi_runtime.addToLinker(self.allocator, &linker);

        // ====================================================================
        // Add test runtime functions (Zig test runner pattern)
        // Reference: Zig test runner output format
        // ====================================================================
        const test_funcs = try test_runtime.addToLinker(self.allocator, &linker, print_funcs.write_idx, print_funcs.eprint_int_idx);

        // Get actual count from linker - never hardcode (Go: len(hostImports))
        const runtime_func_count = linker.funcCount();

        // Set minimum table size of 1 for call_indirect (destructor calls)
        // Table entry 0 is reserved (null/no destructor)
        linker.setTableSize(1);

        // Build function name -> index mapping
        // Runtime functions come first, then user functions
        var func_indices = wasm_gen.FuncIndexMap{};
        defer func_indices.deinit(self.allocator);

        // Add ARC function names to index map (Swift)
        try func_indices.put(self.allocator, arc.ALLOC_NAME, arc_funcs.alloc_idx);
        try func_indices.put(self.allocator, arc.RETAIN_NAME, arc_funcs.retain_idx);
        try func_indices.put(self.allocator, arc.RELEASE_NAME, arc_funcs.release_idx);
        try func_indices.put(self.allocator, arc.DEALLOC_NAME, arc_funcs.dealloc_idx);
        try func_indices.put(self.allocator, arc.REALLOC_NAME, arc_funcs.realloc_idx);
        try func_indices.put(self.allocator, arc.STRING_CONCAT_NAME, arc_funcs.string_concat_idx);
        try func_indices.put(self.allocator, arc.STRING_EQ_NAME, arc_funcs.string_eq_idx);
        try func_indices.put(self.allocator, arc.MEMSET_ZERO_NAME, arc_funcs.memset_zero_idx);
        try func_indices.put(self.allocator, arc.MEMCPY_NAME, arc_funcs.memcpy_idx);

        // Add slice function names to index map (Go)
        try func_indices.put(self.allocator, slice_runtime.GROWSLICE_NAME, slice_funcs.growslice_idx);
        try func_indices.put(self.allocator, slice_runtime.NEXTSLICECAP_NAME, slice_funcs.nextslicecap_idx);

        // Add print function names to index map (Go)
        try func_indices.put(self.allocator, print_runtime.WRITE_NAME, print_funcs.write_idx);
        try func_indices.put(self.allocator, print_runtime.PRINT_INT_NAME, print_funcs.print_int_idx);
        try func_indices.put(self.allocator, print_runtime.EPRINT_INT_NAME, print_funcs.eprint_int_idx);
        try func_indices.put(self.allocator, print_runtime.INT_TO_STRING_NAME, print_funcs.int_to_string_idx);

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

        // Add test function names to index map (Zig)
        try func_indices.put(self.allocator, test_runtime.TEST_PRINT_NAME_NAME, test_funcs.test_print_name_idx);
        try func_indices.put(self.allocator, test_runtime.TEST_PASS_NAME, test_funcs.test_pass_idx);
        try func_indices.put(self.allocator, test_runtime.TEST_FAIL_NAME, test_funcs.test_fail_idx);
        try func_indices.put(self.allocator, test_runtime.TEST_SUMMARY_NAME, test_funcs.test_summary_idx);

        // Add user function names (offset by ARC function count)
        for (funcs, 0..) |*ir_func, i| {
            try func_indices.put(self.allocator, ir_func.name, @intCast(i + runtime_func_count));
        }

        // ====================================================================
        // Build destructor table: map type_name -> table index
        // Reference: Swift stores destructor pointer in type metadata
        // ====================================================================
        var destructor_table = std.StringHashMap(u32).init(self.allocator);
        defer destructor_table.deinit();

        // Metadata address map: type_name -> metadata memory address
        var metadata_addrs = std.StringHashMap(i32).init(self.allocator);
        defer metadata_addrs.deinit();

        // Reserve table index 0 as null (no destructor)
        // This ensures actual destructors start at index 1+
        _ = try linker.addTableFunc(arc_funcs.release_idx); // Placeholder at index 0

        // Find all destructor functions (marked by is_destructor flag during lowering)
        // and add them to the table. Uses semantic metadata instead of name scanning
        // to avoid false positives on generic methods like "List(i64)_deinit".
        for (funcs, 0..) |*ir_func, i| {
            if (ir_func.is_destructor) {
                // Extract type name: "TypeName_deinit" → "TypeName"
                if (std.mem.lastIndexOf(u8, ir_func.name, "_")) |sep| {
                    const type_name = ir_func.name[0..sep];
                    const func_idx: u32 = @intCast(i + runtime_func_count);

                    // Add to table (table_index starts at 1, 0 is reserved for null)
                    const table_idx = try linker.addTableFunc(func_idx);
                    try destructor_table.put(type_name, table_idx);

                    pipeline_debug.log(.codegen, "driver: destructor {s} at table[{d}] = func[{d}]", .{ ir_func.name, table_idx, func_idx });
                }
            }
        }

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

            pipeline_debug.log(.codegen, "driver: metadata for {s} at offset {d}, dtor_idx={d}", .{ entry.key_ptr.*, offset, dtor_idx });
            type_id += 1;
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
                    pipeline_debug.log(.codegen, "driver: string literal at offset {d}: \"{s}\"", .{ offset, str });
                }
            }
        }

        // ====================================================================
        // Pass 2: Generate code for each function
        // ====================================================================
        for (funcs) |*ir_func| {
            // Build SSA
            var ssa_builder = try ssa_builder_mod.SSABuilder.init(self.allocator, ir_func, type_reg);
            errdefer ssa_builder.deinit();

            const ssa_func = try ssa_builder.build();
            defer {
                ssa_func.deinit();
                self.allocator.destroy(ssa_func);
            }
            ssa_builder.deinit();

            // Run Wasm-specific passes (no regalloc needed!)
            // Following Go's pass order:
            // 1. rewritegeneric - ConstString → StringMake (Go: rewritegeneric.go)
            // 2. decompose - phi decomposition for slices/strings (Go: decompose.go)
            // 3. rewritedec - string/slice decomposition (Go: rewritedec.go)
            // 4. schedule - value ordering
            // 5. layout - block ordering (Go: layout.go)
            // 6. lower_wasm - generic → wasm ops (Go: lower.go)
            // NOTE: phi lowering removed - Wasm stack machine handles phis differently
            try rewritegeneric.rewrite(self.allocator, ssa_func, &string_offsets);
            try decompose_builtin.decompose(self.allocator, ssa_func);
            try rewritedec.rewrite(self.allocator, ssa_func);
            try schedule.schedule(ssa_func);
            try layout.layout(ssa_func);
            try lower_wasm.lower(ssa_func);

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
            var params: [16]wasm.ValType = undefined;
            {
                // Build Wasm param types, decomposing compound types (slice, string)
                // into 2 separate i64 entries (ptr, len). This must match the SSA
                // builder's arg decomposition in ssa_builder.zig.
                var wasm_param_idx: usize = 0;
                for (ir_func.params) |param| {
                    const param_type = type_reg.get(param.type_idx);
                    const is_string_or_slice = param.type_idx == types_mod.TypeRegistry.STRING or param_type == .slice;
                    const type_size = type_reg.sizeOf(param.type_idx);
                    const is_large_struct = param_type == .struct_type and type_size > 8 and type_size <= 16;

                    if (is_string_or_slice or is_large_struct) {
                        // Compound type: 2 i64 params (ptr+len or lo+hi)
                        params[wasm_param_idx] = .i64;
                        wasm_param_idx += 1;
                        params[wasm_param_idx] = .i64;
                        wasm_param_idx += 1;
                    } else {
                        const is_float = param.type_idx == types_mod.TypeRegistry.F64 or
                            param.type_idx == types_mod.TypeRegistry.F32;
                        params[wasm_param_idx] = if (is_float) .f64 else .i64;
                        wasm_param_idx += 1;
                    }
                }
            }
            const has_return = ir_func.return_type != types_mod.TypeRegistry.VOID;
            const ret_is_float = ir_func.return_type == types_mod.TypeRegistry.F64 or
                ir_func.return_type == types_mod.TypeRegistry.F32;
            const results: []const wasm.ValType = if (!has_return)
                &[_]wasm.ValType{}
            else if (ret_is_float)
                &[_]wasm.ValType{.f64}
            else
                &[_]wasm.ValType{.i64};

            // Add function type to linker
            const type_idx = try linker.addType(params[0..param_count], results);

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
                        var cp: [16]wasm.ValType = undefined;
                        for (0..n_params) |pi| {
                            // Check if this arg is a float type
                            const arg_val = sv.args[skip + pi];
                            const is_f = arg_val.type_idx == types_mod.TypeRegistry.F64 or
                                arg_val.type_idx == types_mod.TypeRegistry.F32;
                            cp[pi] = if (is_f) .f64 else .i64;
                        }
                        const ret_f = sv.type_idx == types_mod.TypeRegistry.F64 or
                            sv.type_idx == types_mod.TypeRegistry.F32;
                        const res: []const wasm.ValType = if (!has_res)
                            &[_]wasm.ValType{}
                        else if (ret_f)
                            &[_]wasm.ValType{.f64}
                        else
                            &[_]wasm.ValType{.i64};
                        const call_type_idx = try linker.addType(cp[0..n_params], res);
                        sv.aux_int = @intCast(call_type_idx);
                    }
                }
            }

            // Generate function body code using Go-style two-pass architecture
            const body = try wasm.generateFunc(self.allocator, ssa_func, &func_indices, &string_offsets, &metadata_addrs, &func_table_indices);
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

        // Emit Wasm binary using Go-style linker
        // This includes proper sections: type, function, memory, global (SP), export, code, data
        var output: std.ArrayListUnmanaged(u8) = .{};
        try linker.emit(output.writer(self.allocator));
        return output.toOwnedSlice(self.allocator);
    }
};

// ============================================================================
// Helper functions for type conversion (used by native codegen)
// ============================================================================

/// Convert a single wasm ValType to WasmValType (for function signatures).
fn convertWasmValType(val_type: wasm_old.ValType) wasm_func_translator.WasmValType {
    return switch (val_type) {
        .i32 => .i32,
        .i64 => .i64,
        .f32 => .f32,
        .f64 => .f64,
        .funcref => .funcref,
        .externref => .externref,
    };
}

/// Convert wasm ValType to translator's global WasmValType.
/// The translator uses a simpler enum (i32/i64/f32/f64) for globals.
fn convertToTranslatorValType(val_type: wasm_old.ValType) @import("codegen/native/wasm_to_clif/translator.zig").WasmValType {
    const TranslatorValType = @import("codegen/native/wasm_to_clif/translator.zig").WasmValType;
    return switch (val_type) {
        .i32 => TranslatorValType.i32,
        .i64 => TranslatorValType.i64,
        .f32 => TranslatorValType.f32,
        .f64 => TranslatorValType.f64,
        // Reference types default to i64 (pointer size)
        .funcref, .externref => TranslatorValType.i64,
    };
}

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
