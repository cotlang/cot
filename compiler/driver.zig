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
const bench_runtime = @import("codegen/bench_runtime.zig"); // Bench runtime (Go testing.B)

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

const project_mod = @import("project.zig");

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
    test_filter: ?[]const u8 = null,
    bench_mode: bool = false,
    bench_filter: ?[]const u8 = null,
    bench_n: ?i64 = null,
    release_mode: bool = false,

    pub fn init(allocator: Allocator) Driver {
        return .{ .allocator = allocator };
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

    pub fn setBenchMode(self: *Driver, enabled: bool) void {
        self.bench_mode = enabled;
    }

    pub fn setBenchFilter(self: *Driver, filter: []const u8) void {
        self.bench_filter = filter;
    }

    pub fn setBenchN(self: *Driver, n: i64) void {
        self.bench_n = n;
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
        try self.parseFileRecursive(path, &parsed_files, &seen_files);

        // Apply project-level @safe mode from cot.json
        {
            const maybe_loaded = project_mod.loadConfig(self.allocator, null) catch null;
            if (maybe_loaded) |loaded_val| {
                var loaded = loaded_val;
                defer loaded.deinit();
                if (loaded.value().safe orelse false) {
                    for (parsed_files.items) |*pf| {
                        if (pf.tree.file) |*file| {
                            file.safe_mode = true;
                        }
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

        for (parsed_files.items) |*pf| {
            var err_reporter = errors_mod.ErrorReporter.init(&pf.source, null);
            var chk = checker_mod.Checker.init(self.allocator, &pf.tree, &type_reg, &err_reporter, &global_scope, &generic_ctx, self.target);
            defer chk.deinit();
            chk.checkFile() catch |e| {
                return e;
            };
            if (err_reporter.hasErrors()) {
                return error.TypeCheckError;
            }
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
        try self.parseFileRecursive(path, &parsed_files, &seen_files);

        // Apply project-level @safe mode from cot.json
        {
            const maybe_loaded = project_mod.loadConfig(self.allocator, null) catch null;
            if (maybe_loaded) |loaded_val| {
                var loaded = loaded_val;
                defer loaded.deinit();
                if (loaded.value().safe orelse false) {
                    for (parsed_files.items) |*pf| {
                        if (pf.tree.file) |*file| {
                            file.safe_mode = true;
                        }
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

        var total_warnings: u32 = 0;
        for (parsed_files.items) |*pf| {
            var err_reporter = errors_mod.ErrorReporter.init(&pf.source, null);
            var chk = checker_mod.Checker.init(self.allocator, &pf.tree, &type_reg, &err_reporter, &global_scope, &generic_ctx, self.target);
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
        var chk = checker_mod.Checker.init(self.allocator, &tree, &type_reg, &err_reporter, &global_scope, &generic_ctx, self.target);
        defer chk.deinit();
        try chk.checkFile();
        if (err_reporter.hasErrors()) return error.TypeCheckError;

        // Lower to IR
        var lowerer = lower_mod.Lowerer.init(self.allocator, &tree, &type_reg, &err_reporter, &chk, self.target);
        defer lowerer.deinit();
        lowerer.release_mode = self.release_mode;
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

        // Apply project-level @safe mode from cot.json
        {
            const maybe_loaded = project_mod.loadConfig(self.allocator, null) catch null;
            if (maybe_loaded) |loaded_val| {
                var loaded = loaded_val;
                defer loaded.deinit();
                if (loaded.value().safe orelse false) {
                    for (parsed_files.items) |*pf| {
                        if (pf.tree.file) |*file| {
                            file.safe_mode = true;
                        }
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

        for (parsed_files.items) |*pf| {
            var err_reporter = errors_mod.ErrorReporter.init(&pf.source, null);
            var chk = checker_mod.Checker.init(self.allocator, &pf.tree, &type_reg, &err_reporter, &global_scope, &generic_ctx, self.target);
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
        var all_bench_names = std.ArrayListUnmanaged([]const u8){};
        defer all_bench_names.deinit(self.allocator);
        var all_bench_display_names = std.ArrayListUnmanaged([]const u8){};
        defer all_bench_display_names.deinit(self.allocator);

        for (parsed_files.items, 0..) |*pf, i| {
            var lower_err = errors_mod.ErrorReporter.init(&pf.source, null);
            var lowerer = lower_mod.Lowerer.initWithBuilder(self.allocator, &pf.tree, &type_reg, &lower_err, &checkers.items[i], shared_builder, self.target);
            lowerer.release_mode = self.release_mode;
            if (self.test_mode) lowerer.setTestMode(true);
            if (self.bench_mode) lowerer.setBenchMode(true);

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
            if (self.bench_mode) {
                for (lowerer.getBenchNames()) |n| try all_bench_names.append(self.allocator, n);
                for (lowerer.getBenchDisplayNames()) |n| try all_bench_display_names.append(self.allocator, n);
            }
            shared_builder = lowerer.builder;
            lowerer.deinitWithoutBuilder();
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
        if (self.test_mode and all_test_names.items.len > 0) {
            var dummy_err = errors_mod.ErrorReporter.init(&parsed_files.items[0].source, null);
            var runner = lower_mod.Lowerer.initWithBuilder(self.allocator, &parsed_files.items[0].tree, &type_reg, &dummy_err, &checkers.items[0], shared_builder, self.target);
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

        var src = source_mod.Source.init(self.allocator, path_copy, source_text);
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
            .wasi => "WASI",
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
            .freestanding, .wasi => return error.UnsupportedObjectFormat,
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
                        std.mem.eql(u8, exp.name, "cot_arg_ptr") or
                        std.mem.eql(u8, exp.name, "cot_environ_count") or
                        std.mem.eql(u8, exp.name, "cot_environ_len") or
                        std.mem.eql(u8, exp.name, "cot_environ_ptr") or
                        std.mem.eql(u8, exp.name, "cot_net_socket") or
                        std.mem.eql(u8, exp.name, "cot_net_bind") or
                        std.mem.eql(u8, exp.name, "cot_net_listen") or
                        std.mem.eql(u8, exp.name, "cot_net_accept") or
                        std.mem.eql(u8, exp.name, "cot_net_connect") or
                        std.mem.eql(u8, exp.name, "cot_net_set_reuse_addr") or
                        std.mem.eql(u8, exp.name, "cot_kqueue_create") or
                        std.mem.eql(u8, exp.name, "cot_kevent_add") or
                        std.mem.eql(u8, exp.name, "cot_kevent_del") or
                        std.mem.eql(u8, exp.name, "cot_kevent_wait") or
                        std.mem.eql(u8, exp.name, "cot_epoll_create") or
                        std.mem.eql(u8, exp.name, "cot_epoll_add") or
                        std.mem.eql(u8, exp.name, "cot_epoll_del") or
                        std.mem.eql(u8, exp.name, "cot_epoll_wait") or
                        std.mem.eql(u8, exp.name, "cot_set_nonblocking") or
                        std.mem.eql(u8, exp.name, "cot_fork") or
                        std.mem.eql(u8, exp.name, "cot_execve") or
                        std.mem.eql(u8, exp.name, "cot_waitpid") or
                        std.mem.eql(u8, exp.name, "cot_pipe") or
                        std.mem.eql(u8, exp.name, "cot_dup2"))
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
                        // Bounds check: reject paths > 1024 bytes with -ENAMETOOLONG
                        0x7F, 0x00, 0x10, 0xF1, // cmp x3, #1024  (path_len vs PATH_MAX)
                        0x89, 0x00, 0x00, 0x54, // b.ls +4  (path_len <= 1024 → .copy_start)
                        0xC0, 0x07, 0x80, 0x92, // movn x0, #62  (x0 = -63 = -ENAMETOOLONG on macOS)
                        0xFD, 0x7B, 0xC1, 0xA8, // ldp x29, x30, [sp], #16
                        0xC0, 0x03, 0x5F, 0xD6, // ret
                        // .copy_start:
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
                    // ARM64 macOS: monotonic nanoseconds via CNTVCT_EL0
                    // Cranelift CC: x0=vmctx, x1=caller_vmctx (no user args)
                    // Returns: i64 nanoseconds = CNTVCT_EL0 * 125 / 3
                    // Reference: Go runtime/sys_darwin_arm64.s uses libSystem trampolines,
                    // NOT raw SVC #0x80. macOS gettimeofday uses commpage internally.
                    // Raw SVC #0x80 corrupts vmctx memory on Apple Silicon — confirmed bug.
                    // Apple Silicon timebase: 24MHz → numer=125, denom=3 → ns = ticks * 125 / 3
                    const arm64_time = [_]u8{
                        0xDF, 0x3F, 0x03, 0xD5, // isb  (barrier before CNTVCT read)
                        0x40, 0xE0, 0x3B, 0xD5, // mrs x0, CNTVCT_EL0
                        0xA1, 0x0F, 0x80, 0xD2, // movz x1, #125
                        0x00, 0x7C, 0x01, 0x9B, // mul x0, x0, x1
                        0x61, 0x00, 0x80, 0xD2, // movz x1, #3
                        0x00, 0x0C, 0xC1, 0x9A, // udiv x0, x0, x1
                        0xC0, 0x03, 0x5F, 0xD6, // ret
                    };
                    try module.defineFunctionBytes(func_ids[i], &arm64_time, &.{});
                } else if (std.mem.eql(u8, name, "cot_random")) {
                    // ARM64 macOS: getentropy(buf, len) — fill buffer with random bytes
                    // Cranelift CC: x0=vmctx, x1=caller_vmctx, x2=buf(wasm ptr), x3=len
                    // Reference: Go rand_getrandom.go chunk loop pattern
                    // getentropy has 256-byte kernel limit; loop in chunks
                    // On success: x0 = 0. On error: x0 = -errno.
                    const arm64_random = [_]u8{
                        0xFD, 0x7B, 0xBF, 0xA9, // stp x29, x30, [sp, #-16]!
                        0xFD, 0x03, 0x00, 0x91, // mov x29, sp
                        0x08, 0x00, 0x41, 0x91, // add x8, x0, #0x40, lsl #12  (linmem base)
                        0x09, 0x01, 0x02, 0x8B, // add x9, x8, x2              (real buf = linmem + wasm_ptr)
                        0xEA, 0x03, 0x03, 0xAA, // mov x10, x3                 (remaining = len)
                        0x0B, 0x20, 0x80, 0xD2, // movz x11, #256              (chunk size constant)
                        // .loop:
                        0x6A, 0x01, 0x00, 0xB4, // cbz x10, +11  (remaining == 0 → .success)
                        0x5F, 0x01, 0x04, 0xF1, // cmp x10, #256
                        0x4C, 0x31, 0x8B, 0x9A, // csel x12, x10, x11, lo  (x12 = min(remaining, 256))
                        0xE0, 0x03, 0x09, 0xAA, // mov x0, x9               (buf arg)
                        0xE1, 0x03, 0x0C, 0xAA, // mov x1, x12              (len arg)
                        0x90, 0x3E, 0x80, 0xD2, // movz x16, #500           (SYS_getentropy)
                        0x01, 0x10, 0x00, 0xD4, // svc #0x80
                        0xE2, 0x00, 0x00, 0x54, // b.cs +7   (carry set → .error)
                        0x29, 0x01, 0x0C, 0x8B, // add x9, x9, x12          (buf += chunk)
                        0x4A, 0x01, 0x0C, 0xCB, // sub x10, x10, x12        (remaining -= chunk)
                        0xF6, 0xFF, 0xFF, 0x17, // b -10                    (→ .loop)
                        // .success:
                        0xE0, 0x03, 0x1F, 0xAA, // mov x0, xzr              (return 0)
                        0xFD, 0x7B, 0xC1, 0xA8, // ldp x29, x30, [sp], #16
                        0xC0, 0x03, 0x5F, 0xD6, // ret
                        // .error:
                        0xE0, 0x03, 0x00, 0xCB, // neg x0, x0               (negate errno)
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
                        0xA2, 0x00, 0x00, 0x54, // b.cs +5     (carry set = error, skip to .error)
                        // Success: store bytes_written, return ESUCCESS
                        0x09, 0x01, 0x25, 0x8B, // add x9, x8, w5, uxtw  (linmem + nwritten)
                        0x20, 0x01, 0x00, 0xB9, // str w0, [x9]          (store result as i32)
                        0xE0, 0x03, 0x1F, 0xAA, // mov x0, xzr           (return 0)
                        0x02, 0x00, 0x00, 0x14, // b +2                   (skip to .done)
                        // .error: negate errno (Go NEG R0,R0 pattern)
                        0xE0, 0x03, 0x00, 0xCB, // neg x0, x0
                        // .done:
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
                    // ARM64: copy argv[n] into linear memory at wasm offset 0xAF000 + n*4096
                    // Cranelift CC: x0=vmctx, x1=caller_vmctx, x2=n
                    // Reference: Go goenvs() validates argc; Wasmtime uses caller-allocated buffers
                    // Each arg gets a 4096-byte slot; copy limited to 4095 bytes + NUL
                    // Returns 0 if n >= argc (out of bounds).
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
                        0x4C, 0xCC, 0x74, 0xD3, // lsl x12, x2, #12            (x12 = n * 4096)
                        0x0A, 0x00, 0x41, 0x91, // add x10, x0, #0x40, lsl #12 (linmem base)
                        0x4A, 0xBD, 0x42, 0x91, // add x10, x10, #0xAF, lsl #12 (+ 0xAF000)
                        0x4A, 0x01, 0x0C, 0x8B, // add x10, x10, x12           (+ n*4096 = dest)
                        0xED, 0xFF, 0x81, 0xD2, // movz x13, #4095             (max copy bytes)
                        // .copy_loop:
                        0xCD, 0x00, 0x00, 0xB4, // cbz x13, +6                 (limit hit → .truncate)
                        0x2B, 0x15, 0x40, 0x38, // ldrb w11, [x9], #1          (load byte, post-inc)
                        0x4B, 0x15, 0x00, 0x38, // strb w11, [x10], #1         (store byte, post-inc)
                        0x8B, 0x00, 0x00, 0x34, // cbz w11, +4                 (NUL found → .done)
                        0xAD, 0x05, 0x00, 0xD1, // sub x13, x13, #1            (remaining--)
                        0xFB, 0xFF, 0xFF, 0x17, // b -5                         (→ .copy_loop)
                        // .truncate:
                        0x5F, 0x01, 0x00, 0x39, // strb wzr, [x10]             (NUL-terminate at limit)
                        // .done: return wasm offset 0xAF000 + n*4096
                        0x00, 0x00, 0x9E, 0xD2, // movz x0, #0xF000
                        0x40, 0x01, 0xA0, 0xF2, // movk x0, #0xA, lsl #16     (x0 = 0xAF000)
                        0x00, 0x00, 0x0C, 0x8B, // add x0, x0, x12             (+ n*4096)
                        0xFD, 0x7B, 0xC1, 0xA8, // ldp x29, x30, [sp], #16
                        0xC0, 0x03, 0x5F, 0xD6, // ret
                    };
                    try module.defineFunctionBytes(func_ids[i], &arm64_arg_ptr, &.{});
                } else if (std.mem.eql(u8, name, "cot_environ_count")) {
                    // ARM64: walk envp array counting until NULL pointer
                    // Cranelift CC: x0=vmctx, x1=caller_vmctx (no user args)
                    // envp at vmctx+0x30010
                    const arm64_environ_count = [_]u8{
                        0x08, 0xC0, 0x40, 0x91, // add x8, x0, #0x30, lsl #12  (vmctx + 0x30000)
                        0x09, 0x09, 0x40, 0xF9, // ldr x9, [x8, #16]           (x9 = envp)
                        0x00, 0x00, 0x80, 0xD2, // movz x0, #0                 (count = 0)
                        // .loop:
                        0x2A, 0x79, 0x60, 0xF8, // ldr x10, [x9, x0, lsl #3]   (x10 = envp[count])
                        0x6A, 0x00, 0x00, 0xB4, // cbz x10, +3                 (NULL → .done)
                        0x00, 0x04, 0x00, 0x91, // add x0, x0, #1              (count++)
                        0xFD, 0xFF, 0xFF, 0x17, // b -3                         (→ .loop)
                        // .done:
                        0xC0, 0x03, 0x5F, 0xD6, // ret                         (return count in x0)
                    };
                    try module.defineFunctionBytes(func_ids[i], &arm64_environ_count, &.{});
                } else if (std.mem.eql(u8, name, "cot_environ_len")) {
                    // ARM64: strlen(envp[n])
                    // Cranelift CC: x0=vmctx, x1=caller_vmctx, x2=n
                    const arm64_environ_len = [_]u8{
                        0x08, 0xC0, 0x40, 0x91, // add x8, x0, #0x30, lsl #12  (vmctx + 0x30000)
                        0x09, 0x09, 0x40, 0xF9, // ldr x9, [x8, #16]           (x9 = envp)
                        0x29, 0x79, 0x62, 0xF8, // ldr x9, [x9, x2, lsl #3]    (x9 = envp[n])
                        0xE9, 0x00, 0x00, 0xB4, // cbz x9, +7                  (NULL → .null)
                        0x00, 0x00, 0x80, 0xD2, // movz x0, #0                 (len = 0)
                        // .loop:
                        0x2A, 0x69, 0x60, 0x38, // ldrb w10, [x9, x0]          (byte = envp[n][len])
                        0x6A, 0x00, 0x00, 0x34, // cbz w10, +3                 (NUL → .done, return len)
                        0x00, 0x04, 0x00, 0x91, // add x0, x0, #1              (len++)
                        0xFD, 0xFF, 0xFF, 0x17, // b -3                         (→ .loop)
                        // .done:
                        0xC0, 0x03, 0x5F, 0xD6, // ret                         (return len in x0)
                        // .null:
                        0x00, 0x00, 0x80, 0xD2, // movz x0, #0
                        0xC0, 0x03, 0x5F, 0xD6, // ret
                    };
                    try module.defineFunctionBytes(func_ids[i], &arm64_environ_len, &.{});
                } else if (std.mem.eql(u8, name, "cot_environ_ptr")) {
                    // ARM64: copy envp[n] into linmem at 0x7F000 + n*4096, return wasm offset
                    // Reference: arm64_arg_ptr (same pattern, offset 0xAF000→0x7F000, argv→envp)
                    const arm64_environ_ptr = [_]u8{
                        0xFD, 0x7B, 0xBF, 0xA9, // stp x29, x30, [sp, #-16]!
                        0xFD, 0x03, 0x00, 0x91, // mov x29, sp
                        0x08, 0xC0, 0x40, 0x91, // add x8, x0, #0x30, lsl #12  (vmctx + 0x30000)
                        0x09, 0x09, 0x40, 0xF9, // ldr x9, [x8, #16]           (x9 = envp)
                        0x29, 0x79, 0x62, 0xF8, // ldr x9, [x9, x2, lsl #3]    (x9 = envp[n], src)
                        0x49, 0x02, 0x00, 0xB4, // cbz x9, +18                 (NULL → .null)
                        0x4C, 0xCC, 0x74, 0xD3, // lsl x12, x2, #12            (x12 = n * 4096)
                        0x0A, 0x00, 0x41, 0x91, // add x10, x0, #0x40, lsl #12 (linmem base)
                        0x4A, 0xFD, 0x41, 0x91, // add x10, x10, #0x7F, lsl #12 (+ 0x7F000)
                        0x4A, 0x01, 0x0C, 0x8B, // add x10, x10, x12           (+ n*4096 = dest)
                        0xED, 0xFF, 0x81, 0xD2, // movz x13, #4095             (max copy bytes)
                        // .copy_loop:
                        0xCD, 0x00, 0x00, 0xB4, // cbz x13, +6                 (limit hit → .truncate)
                        0x2B, 0x15, 0x40, 0x38, // ldrb w11, [x9], #1          (load byte, post-inc)
                        0x4B, 0x15, 0x00, 0x38, // strb w11, [x10], #1         (store byte, post-inc)
                        0x8B, 0x00, 0x00, 0x34, // cbz w11, +4                 (NUL found → .done)
                        0xAD, 0x05, 0x00, 0xD1, // sub x13, x13, #1            (remaining--)
                        0xFB, 0xFF, 0xFF, 0x17, // b -5                         (→ .copy_loop)
                        // .truncate:
                        0x5F, 0x01, 0x00, 0x39, // strb wzr, [x10]             (NUL-terminate at limit)
                        // .done: return wasm offset 0x7F000 + n*4096
                        0x00, 0x00, 0x9E, 0xD2, // movz x0, #0xF000
                        0xE0, 0x00, 0xA0, 0xF2, // movk x0, #0x7, lsl #16     (x0 = 0x7F000)
                        0x00, 0x00, 0x0C, 0x8B, // add x0, x0, x12             (+ n*4096)
                        0xFD, 0x7B, 0xC1, 0xA8, // ldp x29, x30, [sp], #16
                        0xC0, 0x03, 0x5F, 0xD6, // ret
                        // .null: return 0
                        0x00, 0x00, 0x80, 0xD2, // movz x0, #0
                        0xFD, 0x7B, 0xC1, 0xA8, // ldp x29, x30, [sp], #16
                        0xC0, 0x03, 0x5F, 0xD6, // ret
                    };
                    try module.defineFunctionBytes(func_ids[i], &arm64_environ_ptr, &.{});
                } else if (std.mem.eql(u8, name, "cot_net_socket")) {
                    // ARM64 macOS syscall for socket(domain, type, protocol)
                    // Cranelift CC: x0=vmctx, x1=caller_vmctx, x2=domain, x3=type, x4=protocol
                    // macOS SYS_socket = 97
                    // Returns fd on success, -errno on error.
                    const arm64_socket = [_]u8{
                        0xFD, 0x7B, 0xBF, 0xA9, // stp x29, x30, [sp, #-16]!
                        0xFD, 0x03, 0x00, 0x91, // mov x29, sp
                        0xE0, 0x03, 0x02, 0xAA, // mov x0, x2  (domain)
                        0xE1, 0x03, 0x03, 0xAA, // mov x1, x3  (type)
                        0xE2, 0x03, 0x04, 0xAA, // mov x2, x4  (protocol)
                        0x30, 0x0C, 0x80, 0xD2, // mov x16, #97  (SYS_socket)
                        0x01, 0x10, 0x00, 0xD4, // svc #0x80
                        0x43, 0x00, 0x00, 0x54, // b.cc +2  (success)
                        0xE0, 0x03, 0x00, 0xCB, // neg x0, x0  (error)
                        0xFD, 0x7B, 0xC1, 0xA8, // ldp x29, x30, [sp], #16
                        0xC0, 0x03, 0x5F, 0xD6, // ret
                    };
                    try module.defineFunctionBytes(func_ids[i], &arm64_socket, &.{});
                } else if (std.mem.eql(u8, name, "cot_net_bind")) {
                    // ARM64 macOS syscall for bind(fd, addr_ptr, addr_len)
                    // Cranelift CC: x0=vmctx, x1=caller_vmctx, x2=fd, x3=addr_ptr(wasm), x4=addr_len
                    // addr_ptr is a wasm pointer into linear memory
                    // macOS SYS_bind = 104
                    const arm64_bind = [_]u8{
                        0xFD, 0x7B, 0xBF, 0xA9, // stp x29, x30, [sp, #-16]!
                        0xFD, 0x03, 0x00, 0x91, // mov x29, sp
                        0x08, 0x00, 0x41, 0x91, // add x8, x0, #0x40, lsl #12  (linmem base)
                        0xE0, 0x03, 0x02, 0xAA, // mov x0, x2  (fd)
                        0x01, 0x01, 0x03, 0x8B, // add x1, x8, x3  (real addr = linmem + wasm_ptr)
                        0xE2, 0x03, 0x04, 0xAA, // mov x2, x4  (addr_len)
                        0x10, 0x0D, 0x80, 0xD2, // mov x16, #104  (SYS_bind)
                        0x01, 0x10, 0x00, 0xD4, // svc #0x80
                        0x43, 0x00, 0x00, 0x54, // b.cc +2  (success)
                        0xE0, 0x03, 0x00, 0xCB, // neg x0, x0  (error)
                        0xFD, 0x7B, 0xC1, 0xA8, // ldp x29, x30, [sp], #16
                        0xC0, 0x03, 0x5F, 0xD6, // ret
                    };
                    try module.defineFunctionBytes(func_ids[i], &arm64_bind, &.{});
                } else if (std.mem.eql(u8, name, "cot_net_listen")) {
                    // ARM64 macOS syscall for listen(fd, backlog)
                    // Cranelift CC: x0=vmctx, x1=caller_vmctx, x2=fd, x3=backlog
                    // macOS SYS_listen = 106
                    const arm64_listen = [_]u8{
                        0xFD, 0x7B, 0xBF, 0xA9, // stp x29, x30, [sp, #-16]!
                        0xFD, 0x03, 0x00, 0x91, // mov x29, sp
                        0xE0, 0x03, 0x02, 0xAA, // mov x0, x2  (fd)
                        0xE1, 0x03, 0x03, 0xAA, // mov x1, x3  (backlog)
                        0x50, 0x0D, 0x80, 0xD2, // mov x16, #106  (SYS_listen)
                        0x01, 0x10, 0x00, 0xD4, // svc #0x80
                        0x43, 0x00, 0x00, 0x54, // b.cc +2  (success)
                        0xE0, 0x03, 0x00, 0xCB, // neg x0, x0  (error)
                        0xFD, 0x7B, 0xC1, 0xA8, // ldp x29, x30, [sp], #16
                        0xC0, 0x03, 0x5F, 0xD6, // ret
                    };
                    try module.defineFunctionBytes(func_ids[i], &arm64_listen, &.{});
                } else if (std.mem.eql(u8, name, "cot_net_accept")) {
                    // ARM64 macOS syscall for accept(fd, NULL, NULL)
                    // Cranelift CC: x0=vmctx, x1=caller_vmctx, x2=fd
                    // We pass NULL for addr and addrlen (don't need peer info)
                    // macOS SYS_accept = 30
                    const arm64_accept = [_]u8{
                        0xFD, 0x7B, 0xBF, 0xA9, // stp x29, x30, [sp, #-16]!
                        0xFD, 0x03, 0x00, 0x91, // mov x29, sp
                        0xE0, 0x03, 0x02, 0xAA, // mov x0, x2  (fd)
                        0xE1, 0x03, 0x1F, 0xAA, // mov x1, xzr  (addr = NULL)
                        0xE2, 0x03, 0x1F, 0xAA, // mov x2, xzr  (addrlen = NULL)
                        0xD0, 0x03, 0x80, 0xD2, // mov x16, #30  (SYS_accept)
                        0x01, 0x10, 0x00, 0xD4, // svc #0x80
                        0x43, 0x00, 0x00, 0x54, // b.cc +2  (success)
                        0xE0, 0x03, 0x00, 0xCB, // neg x0, x0  (error)
                        0xFD, 0x7B, 0xC1, 0xA8, // ldp x29, x30, [sp], #16
                        0xC0, 0x03, 0x5F, 0xD6, // ret
                    };
                    try module.defineFunctionBytes(func_ids[i], &arm64_accept, &.{});
                } else if (std.mem.eql(u8, name, "cot_net_connect")) {
                    // ARM64 macOS syscall for connect(fd, addr_ptr, addr_len)
                    // Cranelift CC: x0=vmctx, x1=caller_vmctx, x2=fd, x3=addr_ptr(wasm), x4=addr_len
                    // macOS SYS_connect = 98
                    const arm64_connect = [_]u8{
                        0xFD, 0x7B, 0xBF, 0xA9, // stp x29, x30, [sp, #-16]!
                        0xFD, 0x03, 0x00, 0x91, // mov x29, sp
                        0x08, 0x00, 0x41, 0x91, // add x8, x0, #0x40, lsl #12  (linmem base)
                        0xE0, 0x03, 0x02, 0xAA, // mov x0, x2  (fd)
                        0x01, 0x01, 0x03, 0x8B, // add x1, x8, x3  (real addr = linmem + wasm_ptr)
                        0xE2, 0x03, 0x04, 0xAA, // mov x2, x4  (addr_len)
                        0x50, 0x0C, 0x80, 0xD2, // mov x16, #98  (SYS_connect)
                        0x01, 0x10, 0x00, 0xD4, // svc #0x80
                        0x43, 0x00, 0x00, 0x54, // b.cc +2  (success)
                        0xE0, 0x03, 0x00, 0xCB, // neg x0, x0  (error)
                        0xFD, 0x7B, 0xC1, 0xA8, // ldp x29, x30, [sp], #16
                        0xC0, 0x03, 0x5F, 0xD6, // ret
                    };
                    try module.defineFunctionBytes(func_ids[i], &arm64_connect, &.{});
                } else if (std.mem.eql(u8, name, "cot_net_set_reuse_addr")) {
                    // ARM64 macOS syscall for setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, &val, 4)
                    // Cranelift CC: x0=vmctx, x1=caller_vmctx, x2=fd
                    // SOL_SOCKET=0xFFFF, SO_REUSEADDR=0x0004 on macOS
                    // macOS SYS_setsockopt = 105
                    // We store the value 1 on the stack and pass a pointer to it
                    const arm64_reuse = [_]u8{
                        0xFD, 0x7B, 0xBF, 0xA9, // stp x29, x30, [sp, #-16]!
                        0xFD, 0x03, 0x00, 0x91, // mov x29, sp
                        0x28, 0x00, 0x80, 0xD2, // mov x8, #1  (optval = 1)
                        0xFF, 0x43, 0x00, 0xD1, // sub sp, sp, #16
                        0xE8, 0x03, 0x00, 0xB9, // str w8, [sp]  (store optval on stack)
                        0xE0, 0x03, 0x02, 0xAA, // mov x0, x2  (fd)
                        0xE1, 0xFF, 0x9F, 0xD2, // mov x1, #0xFFFF  (SOL_SOCKET)
                        0x82, 0x00, 0x80, 0xD2, // mov x2, #4  (SO_REUSEADDR)
                        0xE3, 0x03, 0x00, 0x91, // mov x3, sp  (&optval)
                        0x84, 0x00, 0x80, 0xD2, // mov x4, #4  (optlen)
                        0x30, 0x0D, 0x80, 0xD2, // mov x16, #105  (SYS_setsockopt)
                        0x01, 0x10, 0x00, 0xD4, // svc #0x80
                        0x43, 0x00, 0x00, 0x54, // b.cc +2  (success)
                        0xE0, 0x03, 0x00, 0xCB, // neg x0, x0  (error)
                        0xBF, 0x03, 0x00, 0x91, // mov sp, x29  (restore stack)
                        0xFD, 0x7B, 0xC1, 0xA8, // ldp x29, x30, [sp], #16
                        0xC0, 0x03, 0x5F, 0xD6, // ret
                    };
                    try module.defineFunctionBytes(func_ids[i], &arm64_reuse, &.{});
                } else if (std.mem.eql(u8, name, "cot_kqueue_create")) {
                    // ARM64 macOS syscall for kqueue()
                    // No user args. Returns kqueue fd.
                    // macOS SYS_kqueue = 362
                    const arm64_kqueue_create = [_]u8{
                        0xFD, 0x7B, 0xBF, 0xA9, // stp x29, x30, [sp, #-16]!
                        0xFD, 0x03, 0x00, 0x91, // mov x29, sp
                        0x50, 0x2D, 0x80, 0xD2, // movz x16, #362 (SYS_kqueue)
                        0x01, 0x10, 0x00, 0xD4, // svc #0x80
                        0x43, 0x00, 0x00, 0x54, // b.cc +2
                        0xE0, 0x03, 0x00, 0xCB, // neg x0, x0
                        0xFD, 0x7B, 0xC1, 0xA8, // ldp x29, x30, [sp], #16
                        0xC0, 0x03, 0x5F, 0xD6, // ret
                    };
                    try module.defineFunctionBytes(func_ids[i], &arm64_kqueue_create, &.{});
                } else if (std.mem.eql(u8, name, "cot_kevent_add")) {
                    // ARM64 macOS: kevent(kq, changelist, 1, NULL, 0, NULL)
                    // Builds kevent struct on stack with EV_ADD|EV_CLEAR (0x0021)
                    // x2=kq, x3=fd, x4=filter. macOS SYS_kevent = 363
                    const arm64_kevent_add = [_]u8{
                        0xFD, 0x7B, 0xBF, 0xA9, // stp x29, x30, [sp, #-16]!
                        0xFD, 0x03, 0x00, 0x91, // mov x29, sp
                        0xFF, 0x83, 0x00, 0xD1, // sub sp, sp, #32
                        0xE3, 0x03, 0x00, 0xF9, // str x3, [sp, #0]   (ident = fd)
                        0xE4, 0x13, 0x00, 0x79, // strh w4, [sp, #8]  (filter)
                        0x28, 0x04, 0x80, 0x52, // movz w8, #0x0021   (EV_ADD|EV_CLEAR)
                        0xE8, 0x17, 0x00, 0x79, // strh w8, [sp, #10] (flags)
                        0xFF, 0x0F, 0x00, 0xB9, // str wzr, [sp, #12] (fflags = 0)
                        0xFF, 0x0B, 0x00, 0xF9, // str xzr, [sp, #16] (data = 0)
                        0xFF, 0x0F, 0x00, 0xF9, // str xzr, [sp, #24] (udata = 0)
                        0xE0, 0x03, 0x02, 0xAA, // mov x0, x2  (kq)
                        0xE1, 0x03, 0x00, 0x91, // mov x1, sp  (changelist)
                        0x22, 0x00, 0x80, 0xD2, // movz x2, #1 (nchanges)
                        0xE3, 0x03, 0x1F, 0xAA, // mov x3, xzr (eventlist = NULL)
                        0xE4, 0x03, 0x1F, 0xAA, // mov x4, xzr (nevents = 0)
                        0xE5, 0x03, 0x1F, 0xAA, // mov x5, xzr (timeout = NULL)
                        0x70, 0x2D, 0x80, 0xD2, // movz x16, #363 (SYS_kevent)
                        0x01, 0x10, 0x00, 0xD4, // svc #0x80
                        0x43, 0x00, 0x00, 0x54, // b.cc +2
                        0xE0, 0x03, 0x00, 0xCB, // neg x0, x0
                        0xBF, 0x03, 0x00, 0x91, // mov sp, x29
                        0xFD, 0x7B, 0xC1, 0xA8, // ldp x29, x30, [sp], #16
                        0xC0, 0x03, 0x5F, 0xD6, // ret
                    };
                    try module.defineFunctionBytes(func_ids[i], &arm64_kevent_add, &.{});
                } else if (std.mem.eql(u8, name, "cot_kevent_del")) {
                    // ARM64 macOS: kevent(kq, changelist, 1, NULL, 0, NULL)
                    // Builds kevent struct with EV_DELETE (0x0002)
                    // x2=kq, x3=fd, x4=filter. macOS SYS_kevent = 363
                    const arm64_kevent_del = [_]u8{
                        0xFD, 0x7B, 0xBF, 0xA9, // stp x29, x30, [sp, #-16]!
                        0xFD, 0x03, 0x00, 0x91, // mov x29, sp
                        0xFF, 0x83, 0x00, 0xD1, // sub sp, sp, #32
                        0xE3, 0x03, 0x00, 0xF9, // str x3, [sp, #0]   (ident = fd)
                        0xE4, 0x13, 0x00, 0x79, // strh w4, [sp, #8]  (filter)
                        0x48, 0x00, 0x80, 0x52, // movz w8, #0x0002   (EV_DELETE)
                        0xE8, 0x17, 0x00, 0x79, // strh w8, [sp, #10] (flags)
                        0xFF, 0x0F, 0x00, 0xB9, // str wzr, [sp, #12] (fflags = 0)
                        0xFF, 0x0B, 0x00, 0xF9, // str xzr, [sp, #16] (data = 0)
                        0xFF, 0x0F, 0x00, 0xF9, // str xzr, [sp, #24] (udata = 0)
                        0xE0, 0x03, 0x02, 0xAA, // mov x0, x2  (kq)
                        0xE1, 0x03, 0x00, 0x91, // mov x1, sp  (changelist)
                        0x22, 0x00, 0x80, 0xD2, // movz x2, #1 (nchanges)
                        0xE3, 0x03, 0x1F, 0xAA, // mov x3, xzr (eventlist = NULL)
                        0xE4, 0x03, 0x1F, 0xAA, // mov x4, xzr (nevents = 0)
                        0xE5, 0x03, 0x1F, 0xAA, // mov x5, xzr (timeout = NULL)
                        0x70, 0x2D, 0x80, 0xD2, // movz x16, #363 (SYS_kevent)
                        0x01, 0x10, 0x00, 0xD4, // svc #0x80
                        0x43, 0x00, 0x00, 0x54, // b.cc +2
                        0xE0, 0x03, 0x00, 0xCB, // neg x0, x0
                        0xBF, 0x03, 0x00, 0x91, // mov sp, x29
                        0xFD, 0x7B, 0xC1, 0xA8, // ldp x29, x30, [sp], #16
                        0xC0, 0x03, 0x5F, 0xD6, // ret
                    };
                    try module.defineFunctionBytes(func_ids[i], &arm64_kevent_del, &.{});
                } else if (std.mem.eql(u8, name, "cot_kevent_wait")) {
                    // ARM64 macOS: kevent(kq, NULL, 0, eventlist, max_events, NULL)
                    // x2=kq, x3=buf_ptr(wasm), x4=max_events. SYS_kevent = 363
                    // buf_ptr converted to real ptr via linmem at vmctx+0x40000
                    const arm64_kevent_wait = [_]u8{
                        0xFD, 0x7B, 0xBF, 0xA9, // stp x29, x30, [sp, #-16]!
                        0xFD, 0x03, 0x00, 0x91, // mov x29, sp
                        0x08, 0x00, 0x41, 0x91, // add x8, x0, #0x40, lsl #12  (linmem base)
                        0x09, 0x01, 0x03, 0x8B, // add x9, x8, x3  (real eventlist ptr)
                        0xE0, 0x03, 0x02, 0xAA, // mov x0, x2      (kq)
                        0xE1, 0x03, 0x1F, 0xAA, // mov x1, xzr     (changelist = NULL)
                        0xE2, 0x03, 0x1F, 0xAA, // mov x2, xzr     (nchanges = 0)
                        0xE3, 0x03, 0x09, 0xAA, // mov x3, x9      (eventlist)
                        // x4 = max_events (already in x4, untouched)
                        0xE5, 0x03, 0x1F, 0xAA, // mov x5, xzr     (timeout = NULL, block)
                        0x70, 0x2D, 0x80, 0xD2, // movz x16, #363  (SYS_kevent)
                        0x01, 0x10, 0x00, 0xD4, // svc #0x80
                        0x43, 0x00, 0x00, 0x54, // b.cc +2
                        0xE0, 0x03, 0x00, 0xCB, // neg x0, x0
                        0xFD, 0x7B, 0xC1, 0xA8, // ldp x29, x30, [sp], #16
                        0xC0, 0x03, 0x5F, 0xD6, // ret
                    };
                    try module.defineFunctionBytes(func_ids[i], &arm64_kevent_wait, &.{});
                } else if (std.mem.eql(u8, name, "cot_set_nonblocking")) {
                    // ARM64 macOS: fcntl(fd, F_GETFL) then fcntl(fd, F_SETFL, flags|O_NONBLOCK)
                    // x2=fd. macOS SYS_fcntl=92, F_GETFL=3, F_SETFL=4, O_NONBLOCK=4
                    const arm64_set_nonblocking = [_]u8{
                        0xFD, 0x7B, 0xBF, 0xA9, // stp x29, x30, [sp, #-16]!
                        0xFD, 0x03, 0x00, 0x91, // mov x29, sp
                        0xF3, 0x53, 0xBF, 0xA9, // stp x19, x20, [sp, #-16]!
                        0xF3, 0x03, 0x02, 0xAA, // mov x19, x2  (save fd)
                        0xE0, 0x03, 0x13, 0xAA, // mov x0, x19  (fd)
                        0x61, 0x00, 0x80, 0xD2, // movz x1, #3  (F_GETFL)
                        0x90, 0x0B, 0x80, 0xD2, // movz x16, #92 (SYS_fcntl)
                        0x01, 0x10, 0x00, 0xD4, // svc #0x80
                        0x62, 0x01, 0x00, 0x54, // b.cs +11  (error → neg)
                        0x88, 0x00, 0x80, 0xD2, // movz x8, #4  (O_NONBLOCK)
                        0x14, 0x00, 0x08, 0xAA, // orr x20, x0, x8  (flags | O_NONBLOCK)
                        0xE0, 0x03, 0x13, 0xAA, // mov x0, x19  (fd)
                        0x81, 0x00, 0x80, 0xD2, // movz x1, #4  (F_SETFL)
                        0xE2, 0x03, 0x14, 0xAA, // mov x2, x20  (new flags)
                        0x90, 0x0B, 0x80, 0xD2, // movz x16, #92 (SYS_fcntl)
                        0x01, 0x10, 0x00, 0xD4, // svc #0x80
                        0x62, 0x00, 0x00, 0x54, // b.cs +3  (error → neg)
                        0xE0, 0x03, 0x1F, 0xAA, // mov x0, xzr  (return 0)
                        0x02, 0x00, 0x00, 0x14, // b +2  (skip error)
                        0xE0, 0x03, 0x00, 0xCB, // neg x0, x0
                        0xF3, 0x53, 0xC1, 0xA8, // ldp x19, x20, [sp], #16
                        0xFD, 0x7B, 0xC1, 0xA8, // ldp x29, x30, [sp], #16
                        0xC0, 0x03, 0x5F, 0xD6, // ret
                    };
                    try module.defineFunctionBytes(func_ids[i], &arm64_set_nonblocking, &.{});
                } else if (std.mem.eql(u8, name, "cot_fork")) {
                    // ARM64 macOS: fork() syscall
                    // Cranelift CC: x0=vmctx, x1=caller_vmctx (no value args)
                    // macOS SYS_fork = 2. Returns child pid in parent, 0 in child.
                    // macOS fork: x0=other_pid in both, x1=0 (parent) / x1=1 (child)
                    // Must check x1 to distinguish, return 0 in child.
                    const arm64_fork = [_]u8{
                        0xFD, 0x7B, 0xBF, 0xA9, // 0: stp x29, x30, [sp, #-16]!
                        0xFD, 0x03, 0x00, 0x91, // 1: mov x29, sp
                        0x50, 0x00, 0x80, 0xD2, // 2: movz x16, #2  (SYS_fork)
                        0x01, 0x10, 0x00, 0xD4, // 3: svc #0x80
                        0x82, 0x00, 0x00, 0x54, // 4: b.cs +4 → 8  (error)
                        0x81, 0x00, 0x00, 0xB4, // 5: cbz x1, +4 → 9  (parent: x0=child_pid)
                        0x00, 0x00, 0x80, 0xD2, // 6: movz x0, #0  (child: return 0)
                        0x02, 0x00, 0x00, 0x14, // 7: b +2 → 9  (epilogue)
                        0xE0, 0x03, 0x00, 0xCB, // 8: neg x0, x0  (error: -errno)
                        0xFD, 0x7B, 0xC1, 0xA8, // 9: ldp x29, x30, [sp], #16
                        0xC0, 0x03, 0x5F, 0xD6, // 10: ret
                    };
                    try module.defineFunctionBytes(func_ids[i], &arm64_fork, &.{});
                } else if (std.mem.eql(u8, name, "cot_waitpid")) {
                    // ARM64 macOS: wait4(pid, &status, options, NULL) syscall
                    // Cranelift CC: x0=vmctx, x1=caller_vmctx, x2=pid
                    // macOS SYS_wait4 = 7. Returns pid on success, status in stack buf.
                    // We return (status >> 8) & 0xFF as the exit code.
                    const arm64_waitpid = [_]u8{
                        0xFD, 0x7B, 0xBF, 0xA9, // stp x29, x30, [sp, #-16]!
                        0xFD, 0x03, 0x00, 0x91, // mov x29, sp
                        0xFF, 0x43, 0x00, 0xD1, // sub sp, sp, #16  (stack space for status)
                        0xE0, 0x03, 0x02, 0xAA, // mov x0, x2  (pid)
                        0xE1, 0x03, 0x00, 0x91, // mov x1, sp  (status_ptr)
                        0x02, 0x00, 0x80, 0xD2, // movz x2, #0  (options = 0)
                        0x03, 0x00, 0x80, 0xD2, // movz x3, #0  (rusage = NULL)
                        0xF0, 0x00, 0x80, 0xD2, // movz x16, #7  (SYS_wait4)
                        0x01, 0x10, 0x00, 0xD4, // svc #0x80
                        0x63, 0x00, 0x00, 0x54, // b.cc +3  (success → load status)
                        0xE0, 0x03, 0x00, 0xCB, // neg x0, x0  (error: -errno)
                        0x03, 0x00, 0x00, 0x14, // b +3  (skip to cleanup)
                        // Success: extract exit code = (status >> 8) & 0xFF
                        0xE0, 0x03, 0x40, 0xB9, // ldr w0, [sp]  (load status as i32)
                        0x00, 0x3C, 0x48, 0xD3, // ubfx x0, x0, #8, #8  (UBFM x0,x0,#8,#15)
                        0xFF, 0x43, 0x00, 0x91, // add sp, sp, #16  (cleanup)
                        0xFD, 0x7B, 0xC1, 0xA8, // ldp x29, x30, [sp], #16
                        0xC0, 0x03, 0x5F, 0xD6, // ret
                    };
                    try module.defineFunctionBytes(func_ids[i], &arm64_waitpid, &.{});
                } else if (std.mem.eql(u8, name, "cot_pipe")) {
                    // ARM64 macOS: pipe() syscall
                    // Cranelift CC: x0=vmctx, x1=caller_vmctx (no value args)
                    // macOS SYS_pipe = 42. Writes [read_fd, write_fd] to buffer.
                    // Returns packed: (write_fd << 32) | read_fd
                    // macOS pipe: no args, returns x0=read_fd, x1=write_fd in registers.
                    // Pack as (write_fd << 32) | read_fd for Cot.
                    const arm64_pipe = [_]u8{
                        0xFD, 0x7B, 0xBF, 0xA9, // 0: stp x29, x30, [sp, #-16]!
                        0xFD, 0x03, 0x00, 0x91, // 1: mov x29, sp
                        0x50, 0x05, 0x80, 0xD2, // 2: movz x16, #42  (SYS_pipe)
                        0x01, 0x10, 0x00, 0xD4, // 3: svc #0x80
                        0x62, 0x00, 0x00, 0x54, // 4: b.cs +3 → 7  (error)
                        0x00, 0x80, 0x01, 0xAA, // 5: orr x0, x0, x1, lsl #32  (pack fds)
                        0x02, 0x00, 0x00, 0x14, // 6: b +2 → 8  (epilogue)
                        0xE0, 0x03, 0x00, 0xCB, // 7: neg x0, x0  (error: -errno)
                        0xFD, 0x7B, 0xC1, 0xA8, // 8: ldp x29, x30, [sp], #16
                        0xC0, 0x03, 0x5F, 0xD6, // 9: ret
                    };
                    try module.defineFunctionBytes(func_ids[i], &arm64_pipe, &.{});
                } else if (std.mem.eql(u8, name, "cot_dup2")) {
                    // ARM64 macOS: dup2(oldfd, newfd) syscall
                    // Cranelift CC: x0=vmctx, x1=caller_vmctx, x2=oldfd, x3=newfd
                    // macOS SYS_dup2 = 90. Returns newfd on success, -errno on error.
                    const arm64_dup2 = [_]u8{
                        0xFD, 0x7B, 0xBF, 0xA9, // stp x29, x30, [sp, #-16]!
                        0xFD, 0x03, 0x00, 0x91, // mov x29, sp
                        0xE0, 0x03, 0x02, 0xAA, // mov x0, x2  (oldfd)
                        0xE1, 0x03, 0x03, 0xAA, // mov x1, x3  (newfd)
                        0x50, 0x0B, 0x80, 0xD2, // movz x16, #90  (SYS_dup2)
                        0x01, 0x10, 0x00, 0xD4, // svc #0x80
                        0x43, 0x00, 0x00, 0x54, // b.cc +2  (success)
                        0xE0, 0x03, 0x00, 0xCB, // neg x0, x0  (error)
                        0xFD, 0x7B, 0xC1, 0xA8, // ldp x29, x30, [sp], #16
                        0xC0, 0x03, 0x5F, 0xD6, // ret
                    };
                    try module.defineFunctionBytes(func_ids[i], &arm64_dup2, &.{});
                } else if (std.mem.eql(u8, name, "cot_execve")) {
                    // ARM64 macOS: execve(path, argv, envp) syscall
                    // Cranelift CC: x0=vmctx, x1=caller_vmctx, x2=path_ptr, x3=argv_ptr, x4=envp_ptr
                    // All pointers are wasm linear memory offsets — add linmem base.
                    // macOS SYS_execve = 59. Does not return on success.
                    // execve with argv/envp pointer fixup: wasm addresses → real addresses
                    // argv[i] and envp[i] are wasm offsets that must have linmem_base added.
                    const arm64_execve = [_]u8{
                        0xFD, 0x7B, 0xBF, 0xA9, // 0: stp x29, x30, [sp, #-16]!
                        0xFD, 0x03, 0x00, 0x91, // 1: mov x29, sp
                        0x08, 0x00, 0x41, 0x91, // 2: add x8, x0, #0x40, lsl #12  (linmem base)
                        0x00, 0x01, 0x02, 0x8B, // 3: add x0, x8, x2  (path = linmem + wasm_ptr)
                        0x01, 0x01, 0x03, 0x8B, // 4: add x1, x8, x3  (argv = linmem + wasm_argv)
                        0x02, 0x01, 0x04, 0x8B, // 5: add x2, x8, x4  (envp = linmem + wasm_envp)
                        // Fixup argv pointers (each entry is a wasm addr that needs linmem_base)
                        0xEA, 0x03, 0x01, 0xAA, // 6: mov x10, x1
                        0x4B, 0x01, 0x40, 0xF9, // 7: ldr x11, [x10]  (argv_loop)
                        0xAB, 0x00, 0x00, 0xB4, // 8: cbz x11, +5 → 13
                        0x6B, 0x01, 0x08, 0x8B, // 9: add x11, x11, x8
                        0x4B, 0x01, 0x00, 0xF9, // 10: str x11, [x10]
                        0x4A, 0x21, 0x00, 0x91, // 11: add x10, x10, #8
                        0xFB, 0xFF, 0xFF, 0x17, // 12: b -5 → 7
                        // Fixup envp pointers
                        0xEA, 0x03, 0x02, 0xAA, // 13: mov x10, x2
                        0x4B, 0x01, 0x40, 0xF9, // 14: ldr x11, [x10]  (envp_loop)
                        0xAB, 0x00, 0x00, 0xB4, // 15: cbz x11, +5 → 20
                        0x6B, 0x01, 0x08, 0x8B, // 16: add x11, x11, x8
                        0x4B, 0x01, 0x00, 0xF9, // 17: str x11, [x10]
                        0x4A, 0x21, 0x00, 0x91, // 18: add x10, x10, #8
                        0xFB, 0xFF, 0xFF, 0x17, // 19: b -5 → 14
                        // Call execve (only returns on error)
                        0x70, 0x07, 0x80, 0xD2, // 20: movz x16, #59  (SYS_execve)
                        0x01, 0x10, 0x00, 0xD4, // 21: svc #0x80
                        0xE0, 0x03, 0x00, 0xCB, // 22: neg x0, x0  (error: -errno)
                        0xFD, 0x7B, 0xC1, 0xA8, // 23: ldp x29, x30, [sp], #16
                        0xC0, 0x03, 0x5F, 0xD6, // 24: ret
                    };
                    try module.defineFunctionBytes(func_ids[i], &arm64_execve, &.{});
                } else if (std.mem.eql(u8, name, "cot_epoll_create") or
                    std.mem.eql(u8, name, "cot_epoll_add") or
                    std.mem.eql(u8, name, "cot_epoll_del") or
                    std.mem.eql(u8, name, "cot_epoll_wait"))
                {
                    // epoll is Linux-only — return -1 on macOS
                    const arm64_stub_neg1 = [_]u8{
                        0x00, 0x00, 0x80, 0x92, // movn x0, #0  (x0 = -1)
                        0xC0, 0x03, 0x5F, 0xD6, // ret
                    };
                    try module.defineFunctionBytes(func_ids[i], &arm64_stub_neg1, &.{});
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
        //   0x30010: envp (i64, char**) - set by _main wrapper from OS
        //   0x40000 - 0xFFFFFF: Linear memory (~15.7MB heap)
        //     0xEF000 (linmem+0xAF000): arg string buffer for @arg_ptr
        //     0xBF000 (linmem+0x7F000): environ string buffer for @environ_ptr
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
        //   1. Saves argc (x0), argv (x1), envp (x2) from macOS C runtime
        //   2. Loads address of _vmctx_data
        //   3. Initializes heap base pointer (requires runtime address)
        //   4. Stores argc/argv/envp at vmctx+0x30000/0x30008/0x30010
        //   5. Calls __wasm_main(vmctx, vmctx)
        //   6. Returns result
        //
        // ARM64 code (22 instructions, 88 bytes):
        //   stp     x29, x30, [sp, #-48]!
        //   mov     x29, sp
        //   stp     x19, x20, [sp, #16]     ; save callee-saved regs
        //   str     x21, [sp, #32]          ; save x21
        //   mov     x19, x0                 ; save argc
        //   mov     x20, x1                 ; save argv
        //   mov     x21, x2                 ; save envp
        //   adrp    x0, _vmctx_data@PAGE    (reloc at offset 28)
        //   add     x0, x0, _vmctx_data@PAGEOFF (reloc at offset 32)
        //   add     x8, x0, #0x20, lsl #12  ; x8 = vmctx + 0x20000
        //   add     x9, x0, #0x40, lsl #12  ; x9 = vmctx + 0x40000 (heap base)
        //   str     x9, [x8]                ; Store heap base ptr
        //   add     x10, x0, #0x30, lsl #12 ; x10 = vmctx + 0x30000 (args area)
        //   str     x19, [x10]              ; Store argc
        //   str     x20, [x10, #8]          ; Store argv
        //   str     x21, [x10, #16]         ; Store envp
        //   mov     x1, x0                  ; caller_vmctx = vmctx
        //   bl      __wasm_main             (reloc at offset 68)
        //   ldp     x19, x20, [sp, #16]     ; restore callee-saved regs
        //   ldr     x21, [sp, #32]          ; restore x21
        //   ldp     x29, x30, [sp], #48
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

        // stp x29, x30, [sp, #-48]! (save frame, 48 bytes for x19/x20/x21)
        try appendInst(&wrapper_code, self.allocator, 0xA9BD7BFD);
        // mov x29, sp
        try appendInst(&wrapper_code, self.allocator, 0x910003FD);
        // stp x19, x20, [sp, #16] (save callee-saved regs)
        try appendInst(&wrapper_code, self.allocator, 0xA90153F3);
        // str x21, [sp, #32] (save x21)
        try appendInst(&wrapper_code, self.allocator, 0xF90013F5);
        // mov x19, x0 (save argc from OS)
        try appendInst(&wrapper_code, self.allocator, 0xAA0003F3);
        // mov x20, x1 (save argv from OS)
        try appendInst(&wrapper_code, self.allocator, 0xAA0103F4);
        // mov x21, x2 (save envp from OS)
        try appendInst(&wrapper_code, self.allocator, 0xAA0203F5);
        // adrp x0, _vmctx_data@PAGE (reloc at offset 28)
        try appendInst(&wrapper_code, self.allocator, 0x90000000);
        // add x0, x0, _vmctx_data@PAGEOFF (reloc at offset 32)
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
        // str x21, [x10, #16] (store envp)
        try appendInst(&wrapper_code, self.allocator, 0xF9000955);
        // mov x1, x0 (caller_vmctx = vmctx)
        try appendInst(&wrapper_code, self.allocator, 0xAA0003E1);
        // bl __wasm_main (reloc at offset 68)
        try appendInst(&wrapper_code, self.allocator, 0x94000000);
        // ldp x19, x20, [sp, #16] (restore callee-saved regs)
        try appendInst(&wrapper_code, self.allocator, 0xA94153F3);
        // ldr x21, [sp, #32] (restore x21)
        try appendInst(&wrapper_code, self.allocator, 0xF94013F5);
        // ldp x29, x30, [sp], #48
        try appendInst(&wrapper_code, self.allocator, 0xA8C37BFD);
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
            // ADRP x0, _vmctx_data@PAGE at offset 28 (after 7 setup instrs)
            .{
                .offset = 28,
                .kind = Reloc.Aarch64AdrPrelPgHi21,
                .target = FinalizedRelocTarget{ .ExternalName = vmctx_name_ref },
                .addend = 0,
            },
            // ADD x0, x0, _vmctx_data@PAGEOFF at offset 32
            .{
                .offset = 32,
                .kind = Reloc.Aarch64AddAbsLo12Nc,
                .target = FinalizedRelocTarget{ .ExternalName = vmctx_name_ref },
                .addend = 0,
            },
            // BL __wasm_main at offset 68
            .{
                .offset = 68,
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
            // Check for native overrides (exported runtime stubs replaced with x86-64 syscalls)
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
                        std.mem.eql(u8, exp.name, "cot_arg_ptr") or
                        std.mem.eql(u8, exp.name, "cot_environ_count") or
                        std.mem.eql(u8, exp.name, "cot_environ_len") or
                        std.mem.eql(u8, exp.name, "cot_environ_ptr") or
                        std.mem.eql(u8, exp.name, "cot_net_socket") or
                        std.mem.eql(u8, exp.name, "cot_net_bind") or
                        std.mem.eql(u8, exp.name, "cot_net_listen") or
                        std.mem.eql(u8, exp.name, "cot_net_accept") or
                        std.mem.eql(u8, exp.name, "cot_net_connect") or
                        std.mem.eql(u8, exp.name, "cot_net_set_reuse_addr") or
                        std.mem.eql(u8, exp.name, "cot_kqueue_create") or
                        std.mem.eql(u8, exp.name, "cot_kevent_add") or
                        std.mem.eql(u8, exp.name, "cot_kevent_del") or
                        std.mem.eql(u8, exp.name, "cot_kevent_wait") or
                        std.mem.eql(u8, exp.name, "cot_epoll_create") or
                        std.mem.eql(u8, exp.name, "cot_epoll_add") or
                        std.mem.eql(u8, exp.name, "cot_epoll_del") or
                        std.mem.eql(u8, exp.name, "cot_epoll_wait") or
                        std.mem.eql(u8, exp.name, "cot_set_nonblocking") or
                        std.mem.eql(u8, exp.name, "cot_fork") or
                        std.mem.eql(u8, exp.name, "cot_execve") or
                        std.mem.eql(u8, exp.name, "cot_waitpid") or
                        std.mem.eql(u8, exp.name, "cot_pipe") or
                        std.mem.eql(u8, exp.name, "cot_dup2"))
                    {
                        override_name = exp.name;
                        break;
                    }
                }
            }
            if (override_name) |name| {
                if (std.mem.eql(u8, name, "cot_write") or std.mem.eql(u8, name, "cot_fd_write_simple")) {
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
                } else if (std.mem.eql(u8, name, "cot_fd_read_simple")) {
                    // x86-64 Linux syscall for read(fd, buf, count)
                    // Cranelift CC: rdi=vmctx, rsi=caller_vmctx, rdx=fd, rcx=buf(wasm), r8=len
                    // Linux read: rax=0(SYS_read), rdi=fd, rsi=buf, rdx=count
                    // Returns bytes read on success, -errno on error.
                    const x64_read = [_]u8{
                        0x55, // push rbp
                        0x48, 0x89, 0xE5, // mov rbp, rsp
                        0x48, 0x8D, 0xB7, 0x00, 0x00, 0x04, 0x00, // lea rsi, [rdi + 0x40000]  (linmem base)
                        0x48, 0x01, 0xCE, // add rsi, rcx              (real_buf = linmem + wasm_ptr)
                        0x48, 0x89, 0xD7, // mov rdi, rdx              (fd)
                        0x4C, 0x89, 0xC2, // mov rdx, r8               (count)
                        0x48, 0x31, 0xC0, // xor rax, rax              (SYS_read = 0)
                        0x0F, 0x05, // syscall
                        0x5D, // pop rbp
                        0xC3, // ret
                    };
                    try module.defineFunctionBytes(elf_func_ids[i], &x64_read, &.{});
                } else if (std.mem.eql(u8, name, "cot_fd_close")) {
                    // x86-64 Linux syscall for close(fd)
                    // Cranelift CC: rdi=vmctx, rsi=caller_vmctx, rdx=fd
                    // Linux close: rax=3(SYS_close), rdi=fd
                    // Returns 0 on success, -errno on error.
                    const x64_close = [_]u8{
                        0x55, // push rbp
                        0x48, 0x89, 0xE5, // mov rbp, rsp
                        0x48, 0x89, 0xD7, // mov rdi, rdx              (fd)
                        0x48, 0xC7, 0xC0, 0x03, 0x00, 0x00, 0x00, // mov rax, 3  (SYS_close)
                        0x0F, 0x05, // syscall
                        0x5D, // pop rbp
                        0xC3, // ret
                    };
                    try module.defineFunctionBytes(elf_func_ids[i], &x64_close, &.{});
                } else if (std.mem.eql(u8, name, "cot_fd_seek")) {
                    // x86-64 Linux syscall for lseek(fd, offset, whence)
                    // Cranelift CC: rdi=vmctx, rsi=caller_vmctx, rdx=fd, rcx=offset, r8=whence
                    // Linux lseek: rax=8(SYS_lseek), rdi=fd, rsi=offset, rdx=whence
                    // Returns new offset on success, -errno on error.
                    const x64_seek = [_]u8{
                        0x55, // push rbp
                        0x48, 0x89, 0xE5, // mov rbp, rsp
                        0x48, 0x89, 0xD7, // mov rdi, rdx              (fd)
                        0x48, 0x89, 0xCE, // mov rsi, rcx              (offset)
                        0x4C, 0x89, 0xC2, // mov rdx, r8               (whence)
                        0x48, 0xC7, 0xC0, 0x08, 0x00, 0x00, 0x00, // mov rax, 8  (SYS_lseek)
                        0x0F, 0x05, // syscall
                        0x5D, // pop rbp
                        0xC3, // ret
                    };
                    try module.defineFunctionBytes(elf_func_ids[i], &x64_seek, &.{});
                } else if (std.mem.eql(u8, name, "cot_fd_open")) {
                    // x86-64 Linux syscall for openat(AT_FDCWD, path, flags, mode)
                    // Cranelift CC: rdi=vmctx, rsi=caller_vmctx, rdx=path_ptr(wasm), rcx=path_len, r8=flags
                    // Must null-terminate path: copy to stack buffer, append \0
                    // Flags use macOS values (stdlib canonical), translated to Linux here:
                    //   macOS O_CREAT(0x200) → Linux(0x40)
                    //   macOS O_TRUNC(0x400) → Linux(0x200)
                    //   macOS O_APPEND(0x8)  → Linux(0x400)
                    // Returns fd on success, -errno on error.
                    const x64_open = [_]u8{
                        0x55, //  0: push rbp
                        0x48, 0x89, 0xE5, //  1: mov rbp, rsp
                        0x48, 0x8D, 0x87, 0x00, 0x00, 0x04, 0x00, //  4: lea rax, [rdi + 0x40000]  (linmem base)
                        0x48, 0x01, 0xD0, // 11: add rax, rdx              (real path src)
                        0x49, 0x89, 0xCA, // 14: mov r10, rcx              (save path_len)
                        0x48, 0x81, 0xF9, 0x00, 0x04, 0x00, 0x00, // 17: cmp rcx, 1024
                        0x76, 0x0C, // 24: jbe +12                  (→ .copy_start at 38)
                        0x48, 0xC7, 0xC0, 0xDC, 0xFF, 0xFF, 0xFF, // 26: mov rax, -36  (-ENAMETOOLONG)
                        0x5D, // 33: pop rbp
                        0xC3, // 34: ret
                        0x90, 0x90, 0x90, // 35: nop nop nop (padding to .copy_start)
                        // .copy_start (offset 38):
                        0x48, 0x81, 0xEC, 0x10, 0x04, 0x00, 0x00, // 38: sub rsp, 1040
                        0x49, 0x89, 0xE3, // 45: mov r11, rsp              (dest)
                        // .copy_loop (offset 48):
                        0x4D, 0x85, 0xD2, // 48: test r10, r10
                        0x74, 0x12, // 51: jz +18                  (→ .null_term at 71)
                        0x44, 0x0F, 0xB6, 0x08, // 53: movzx r9d, byte [rax]
                        0x45, 0x88, 0x0B, // 57: mov [r11], r9b
                        0x48, 0xFF, 0xC0, // 60: inc rax
                        0x49, 0xFF, 0xC3, // 63: inc r11
                        0x49, 0xFF, 0xCA, // 66: dec r10
                        0xEB, 0xE9, // 69: jmp -23                 (→ .copy_loop at 48)
                        // .null_term (offset 71):
                        0x41, 0xC6, 0x03, 0x00, // 71: mov byte [r11], 0
                        // openat(AT_FDCWD, path, translated_flags, mode)
                        0x48, 0xC7, 0xC7, 0x9C, 0xFF, 0xFF, 0xFF, // 75: mov rdi, -100  (AT_FDCWD)
                        0x48, 0x89, 0xE6, // 82: mov rsi, rsp              (path on stack)
                        // --- Flag translation: macOS canonical → Linux ---
                        0x44, 0x89, 0xC0, // 85: mov eax, r8d              (copy macOS flags)
                        0x83, 0xE0, 0x03, // 88: and eax, 3                (keep access mode O_RDONLY/O_WRONLY/O_RDWR)
                        0x41, 0x0F, 0xBA, 0xE0, 0x09, // 91: bt r8d, 9     (test macOS O_CREAT 0x200)
                        0x73, 0x03, // 96: jnc +3                  (skip if not set)
                        0x83, 0xC8, 0x40, // 98: or eax, 0x40              (Linux O_CREAT)
                        0x41, 0x0F, 0xBA, 0xE0, 0x0A, //101: bt r8d, 10    (test macOS O_TRUNC 0x400)
                        0x73, 0x05, //106: jnc +5                  (skip 5-byte or eax,imm32)
                        0x0D, 0x00, 0x02, 0x00, 0x00, //108: or eax, 0x200 (Linux O_TRUNC)
                        0x41, 0x0F, 0xBA, 0xE0, 0x03, //113: bt r8d, 3     (test macOS O_APPEND 0x8)
                        0x73, 0x05, //118: jnc +5                  (skip 5-byte or eax,imm32)
                        0x0D, 0x00, 0x04, 0x00, 0x00, //120: or eax, 0x400 (Linux O_APPEND)
                        0x41, 0x0F, 0xBA, 0xE0, 0x0B, //125: bt r8d, 11    (test macOS O_EXCL 0x800)
                        0x73, 0x03, //130: jnc +3                  (skip if not set)
                        0x83, 0xC8, 0x80, //132: or eax, 0x80              (Linux O_EXCL)
                        0x89, 0xC2, //135: mov edx, eax             (Linux flags → rdx)
                        // --- End flag translation ---
                        0x49, 0xC7, 0xC2, 0xA4, 0x01, 0x00, 0x00, //137: mov r10, 420  (mode 0644, arg4=r10 for syscall)
                        0x48, 0xC7, 0xC0, 0x01, 0x01, 0x00, 0x00, //144: mov rax, 257  (SYS_openat)
                        0x0F, 0x05, //151: syscall
                        0x48, 0x89, 0xEC, //153: mov rsp, rbp  (restore stack)
                        0x5D, //156: pop rbp
                        0xC3, //157: ret
                    };
                    try module.defineFunctionBytes(elf_func_ids[i], &x64_open, &.{});
                } else if (std.mem.eql(u8, name, "cot_time")) {
                    // x86-64 Linux: clock_gettime(CLOCK_REALTIME, &timespec) → nanoseconds
                    // Cranelift CC: rdi=vmctx, rsi=caller_vmctx (no user args)
                    // Returns: i64 nanoseconds = tv_sec * 1_000_000_000 + tv_nsec
                    const x64_time = [_]u8{
                        0x55, //  0: push rbp
                        0x48, 0x89, 0xE5, //  1: mov rbp, rsp
                        0x48, 0x83, 0xEC, 0x10, //  4: sub rsp, 16            (timespec on stack)
                        0x31, 0xFF, //  8: xor edi, edi            (CLOCK_REALTIME = 0)
                        0x48, 0x89, 0xE6, // 10: mov rsi, rsp             (&timespec)
                        0x48, 0xC7, 0xC0, 0xE4, 0x00, 0x00, 0x00, // 13: mov rax, 228  (SYS_clock_gettime)
                        0x0F, 0x05, // 20: syscall
                        0x48, 0x8B, 0x04, 0x24, // 22: mov rax, [rsp]         (tv_sec)
                        0x48, 0x69, 0xC0, 0x00, 0xCA, 0x9A, 0x3B, // 26: imul rax, 1000000000
                        0x48, 0x03, 0x44, 0x24, 0x08, // 33: add rax, [rsp+8]     (+ tv_nsec)
                        0x48, 0x83, 0xC4, 0x10, // 38: add rsp, 16
                        0x5D, // 42: pop rbp
                        0xC3, // 43: ret
                    };
                    try module.defineFunctionBytes(elf_func_ids[i], &x64_time, &.{});
                } else if (std.mem.eql(u8, name, "cot_random")) {
                    // x86-64 Linux: getrandom(buf, count, flags) — fill buffer with random bytes
                    // Cranelift CC: rdi=vmctx, rsi=caller_vmctx, rdx=buf(wasm ptr), rcx=len
                    // SYS_getrandom=318, no chunk limit needed but loop handles partial reads
                    // Returns 0 on success, -errno on error.
                    const x64_random = [_]u8{
                        0x55, //  0: push rbp
                        0x48, 0x89, 0xE5, //  1: mov rbp, rsp
                        0x4C, 0x8D, 0x8F, 0x00, 0x00, 0x04, 0x00, //  4: lea r9, [rdi + 0x40000]  (linmem base)
                        0x49, 0x01, 0xD1, // 11: add r9, rdx               (real buf)
                        0x49, 0x89, 0xCA, // 14: mov r10, rcx              (remaining len)
                        // .loop (offset 17):
                        0x4D, 0x85, 0xD2, // 17: test r10, r10
                        0x74, 0x1E, // 20: jz +30                  (→ .success at 52)
                        0x4C, 0x89, 0xCF, // 22: mov rdi, r9               (buf)
                        0x4C, 0x89, 0xD6, // 25: mov rsi, r10              (count)
                        0x31, 0xD2, // 28: xor edx, edx             (flags = 0)
                        0x48, 0xC7, 0xC0, 0x3E, 0x01, 0x00, 0x00, // 30: mov rax, 318  (SYS_getrandom)
                        0x0F, 0x05, // 37: syscall
                        0x48, 0x85, 0xC0, // 39: test rax, rax
                        0x78, 0x0C, // 42: js +12                  (→ .error at 56)
                        0x49, 0x01, 0xC1, // 44: add r9, rax               (buf += bytes)
                        0x49, 0x29, 0xC2, // 47: sub r10, rax              (remaining -= bytes)
                        0xEB, 0xDD, // 50: jmp -35                 (→ .loop at 17)
                        // .success (offset 52):
                        0x31, 0xC0, // 52: xor eax, eax             (return 0)
                        0x5D, // 54: pop rbp
                        0xC3, // 55: ret
                        // .error (offset 56):
                        0x5D, // 56: pop rbp                    (rax already negative)
                        0xC3, // 57: ret
                    };
                    try module.defineFunctionBytes(elf_func_ids[i], &x64_random, &.{});
                } else if (std.mem.eql(u8, name, "cot_exit")) {
                    // x86-64 Linux: exit_group(code) — never returns
                    // Cranelift CC: rdi=vmctx, rsi=caller_vmctx, rdx=code
                    // SYS_exit_group=231 (kills all threads)
                    const x64_exit = [_]u8{
                        0x48, 0x89, 0xD7, // mov rdi, rdx              (exit code)
                        0x48, 0xC7, 0xC0, 0xE7, 0x00, 0x00, 0x00, // mov rax, 231  (SYS_exit_group)
                        0x0F, 0x05, // syscall
                    };
                    try module.defineFunctionBytes(elf_func_ids[i], &x64_exit, &.{});
                } else if (std.mem.eql(u8, name, "wasi_fd_write")) {
                    // x86-64 Linux syscall for WASI fd_write(fd, iovs, iovs_len, nwritten)
                    // Cranelift CC: rdi=vmctx, rsi=caller_vmctx, rdx=fd, rcx=iovs, r8=iovs_len, r9=nwritten
                    // Fast path: adapter always sends 1 iovec
                    // Returns 0 (ESUCCESS) on success, -errno on error.
                    // Register plan: r10=linmem, r11=iovs addr (then reused),
                    // ecx=iovec.buf, r8d=iovec.buf_len (all caller-saved)
                    const x64_wasi_write = [_]u8{
                        0x55, //  0: push rbp
                        0x48, 0x89, 0xE5, //  1: mov rbp, rsp
                        0x4C, 0x8D, 0x97, 0x00, 0x00, 0x04, 0x00, //  4: lea r10, [rdi + 0x40000]  (r10 = linmem base)
                        0x4D, 0x8D, 0x1C, 0x0A, // 11: lea r11, [r10 + rcx]    (r11 = linmem + iovs)
                        0x41, 0x8B, 0x0B, // 15: mov ecx, [r11]            (ecx = iovec.buf offset, i32)
                        0x45, 0x8B, 0x43, 0x04, // 18: mov r8d, [r11 + 4]      (r8d = iovec.buf_len, i32)
                        // SYS_write(fd, buf, len)
                        0x48, 0x89, 0xD7, // 22: mov rdi, rdx              (fd)
                        0x49, 0x8D, 0x34, 0x0A, // 25: lea rsi, [r10 + rcx]    (real buf = linmem + buf_offset)
                        0x44, 0x89, 0xC2, // 29: mov edx, r8d              (len = buf_len)
                        0x48, 0xC7, 0xC0, 0x01, 0x00, 0x00, 0x00, // 32: mov rax, 1  (SYS_write)
                        0x0F, 0x05, // 39: syscall
                        // Check result
                        0x48, 0x85, 0xC0, // 41: test rax, rax
                        0x78, 0x0A, // 44: js +10                  (→ .error at 56)
                        // Success: store bytes_written at linmem+nwritten
                        0x4B, 0x8D, 0x0C, 0x0A, // 46: lea rcx, [r10 + r9]    (linmem + nwritten)
                        0x89, 0x01, // 50: mov [rcx], eax           (store result as i32)
                        0x31, 0xC0, // 52: xor eax, eax             (return 0 = ESUCCESS)
                        0x5D, // 54: pop rbp
                        0xC3, // 55: ret
                        // .error (offset 56): rax already has -errno from Linux
                        0x5D, // 56: pop rbp
                        0xC3, // 57: ret
                    };
                    try module.defineFunctionBytes(elf_func_ids[i], &x64_wasi_write, &.{});
                } else if (std.mem.eql(u8, name, "cot_args_count")) {
                    // x86-64: read argc from vmctx+0x30000
                    // Cranelift CC: rdi=vmctx, rsi=caller_vmctx (no user args)
                    const x64_args_count = [_]u8{
                        0x48, 0x8B, 0x87, 0x00, 0x00, 0x03, 0x00, // mov rax, [rdi + 0x30000]  (argc)
                        0xC3, // ret
                    };
                    try module.defineFunctionBytes(elf_func_ids[i], &x64_args_count, &.{});
                } else if (std.mem.eql(u8, name, "cot_arg_len")) {
                    // x86-64: strlen(argv[n]) with bounds check
                    // Cranelift CC: rdi=vmctx, rsi=caller_vmctx, rdx=n
                    // Returns strlen or 0 if n >= argc.
                    const x64_arg_len = [_]u8{
                        0x48, 0x8B, 0x87, 0x00, 0x00, 0x03, 0x00, //  0: mov rax, [rdi + 0x30000]  (argc)
                        0x48, 0x39, 0xC2, //  7: cmp rdx, rax              (n vs argc)
                        0x72, 0x04, // 10: jb +4                   (n < argc → .valid at 16)
                        0x31, 0xC0, // 12: xor eax, eax             (return 0)
                        0xC3, // 14: ret
                        0x90, // 15: nop padding
                        // .valid (offset 16):
                        0x48, 0x8B, 0x87, 0x08, 0x00, 0x03, 0x00, // 16: mov rax, [rdi + 0x30008]  (argv)
                        0x48, 0x8B, 0x04, 0xD0, // 23: mov rax, [rax + rdx*8]  (argv[n])
                        0x31, 0xC9, // 27: xor ecx, ecx             (len = 0)
                        // .strlen_loop (offset 29):
                        0x80, 0x3C, 0x08, 0x00, // 29: cmp byte [rax + rcx], 0
                        0x74, 0x05, // 33: je +5                   (→ .done at 40)
                        0x48, 0xFF, 0xC1, // 35: inc rcx
                        0xEB, 0xF5, // 38: jmp -11                 (→ .strlen_loop at 29)
                        // .done (offset 40):
                        0x48, 0x89, 0xC8, // 40: mov rax, rcx             (return len)
                        0xC3, // 43: ret
                    };
                    try module.defineFunctionBytes(elf_func_ids[i], &x64_arg_len, &.{});
                } else if (std.mem.eql(u8, name, "cot_arg_ptr")) {
                    // x86-64: copy argv[n] into linear memory at wasm offset 0xAF000 + n*4096
                    // Cranelift CC: rdi=vmctx, rsi=caller_vmctx, rdx=n
                    // Each arg gets a 4096-byte slot; copy limited to 4095 bytes + NUL
                    // Returns wasm offset (0xAF000 + n*4096) or 0 if n >= argc.
                    const x64_arg_ptr = [_]u8{
                        0x55, //  0: push rbp
                        0x48, 0x89, 0xE5, //  1: mov rbp, rsp
                        0x48, 0x8B, 0x87, 0x00, 0x00, 0x03, 0x00, //  4: mov rax, [rdi + 0x30000]  (argc)
                        0x48, 0x39, 0xC2, // 11: cmp rdx, rax              (n vs argc)
                        0x72, 0x05, // 14: jb +5                   (n < argc → .valid at 21)
                        0x31, 0xC0, // 16: xor eax, eax             (return 0)
                        0x5D, // 18: pop rbp
                        0xC3, // 19: ret
                        0x90, // 20: nop padding
                        // .valid (offset 21):
                        0x48, 0x8B, 0x87, 0x08, 0x00, 0x03, 0x00, // 21: mov rax, [rdi + 0x30008]  (argv)
                        0x4C, 0x8B, 0x04, 0xD0, // 28: mov r8, [rax + rdx*8]   (r8 = argv[n], src)
                        // dest = linmem + 0xAF000 + n*4096
                        0x48, 0x89, 0xD0, // 32: mov rax, rdx              (n)
                        0x48, 0xC1, 0xE0, 0x0C, // 35: shl rax, 12             (n * 4096)
                        0x4C, 0x8D, 0x8F, 0x00, 0x00, 0x04, 0x00, // 39: lea r9, [rdi + 0x40000]  (linmem base)
                        0x49, 0x81, 0xC1, 0x00, 0xF0, 0x0A, 0x00, // 46: add r9, 0xAF000
                        0x49, 0x01, 0xC1, // 53: add r9, rax               (dest = linmem + 0xAF000 + n*4096)
                        0x48, 0x89, 0xD1, // 56: mov rcx, rdx              (save n for return value)
                        0x41, 0xBA, 0xFF, 0x0F, 0x00, 0x00, // 59: mov r10d, 4095          (max copy)
                        // .copy_loop (offset 65):
                        0x4D, 0x85, 0xD2, // 65: test r10, r10
                        0x74, 0x16, // 68: jz +22                  (→ .truncate at 92)
                        0x41, 0x0F, 0xB6, 0x00, // 70: movzx eax, byte [r8]
                        0x41, 0x88, 0x01, // 74: mov [r9], al
                        0x85, 0xC0, // 77: test eax, eax
                        0x74, 0x0F, // 79: jz +15                  (NUL found → .done at 96)
                        0x49, 0xFF, 0xC0, // 81: inc r8
                        0x49, 0xFF, 0xC1, // 84: inc r9
                        0x49, 0xFF, 0xCA, // 87: dec r10
                        0xEB, 0xE5, // 90: jmp -27                 (→ .copy_loop at 65)
                        // .truncate (offset 92):
                        0x41, 0xC6, 0x01, 0x00, // 92: mov byte [r9], 0  (NUL-terminate)
                        // .done (offset 96): return wasm offset 0xAF000 + n*4096
                        0x48, 0xC7, 0xC0, 0x00, 0xF0, 0x0A, 0x00, // 96: mov rax, 0xAF000
                        0x48, 0xC1, 0xE1, 0x0C, //103: shl rcx, 12  (n * 4096)
                        0x48, 0x01, 0xC8, //107: add rax, rcx  (0xAF000 + n*4096)
                        0x5D, //110: pop rbp
                        0xC3, //111: ret
                    };
                    try module.defineFunctionBytes(elf_func_ids[i], &x64_arg_ptr, &.{});
                } else if (std.mem.eql(u8, name, "cot_environ_count")) {
                    // x86-64: count envp entries by walking until NULL pointer
                    // Cranelift CC: rdi=vmctx, rsi=caller_vmctx (no user args)
                    // vmctx+0x30010 = envp (stored by main wrapper)
                    const x64_environ_count = [_]u8{
                        0x55, //  0: push rbp
                        0x48, 0x89, 0xE5, //  1: mov rbp, rsp
                        0x48, 0x8B, 0x87, 0x10, 0x00, 0x03, 0x00, //  4: mov rax, [rdi + 0x30010]  (envp)
                        0x31, 0xC9, // 11: xor ecx, ecx             (count = 0)
                        // .loop (offset 13):
                        0x48, 0x8B, 0x14, 0xC8, // 13: mov rdx, [rax + rcx*8]  (*envp++)
                        0x48, 0x85, 0xD2, // 17: test rdx, rdx
                        0x74, 0x05, // 20: jz +5                    (NULL → .done at 27)
                        0x48, 0xFF, 0xC1, // 22: inc rcx                (count++)
                        0xEB, 0xF2, // 25: jmp -14                  (→ .loop at 13)
                        // .done (offset 27):
                        0x48, 0x89, 0xC8, // 27: mov rax, rcx          (return count)
                        0x5D, // 30: pop rbp
                        0xC3, // 31: ret
                    };
                    try module.defineFunctionBytes(elf_func_ids[i], &x64_environ_count, &.{});
                } else if (std.mem.eql(u8, name, "cot_environ_len")) {
                    // x86-64: strlen(envp[n])
                    // Cranelift CC: rdi=vmctx, rsi=caller_vmctx, rdx=n
                    const x64_environ_len = [_]u8{
                        0x55, //  0: push rbp
                        0x48, 0x89, 0xE5, //  1: mov rbp, rsp
                        0x48, 0x8B, 0x87, 0x10, 0x00, 0x03, 0x00, //  4: mov rax, [rdi + 0x30010]  (envp)
                        0x4C, 0x8B, 0x04, 0xD0, // 11: mov r8, [rax + rdx*8]   (r8 = envp[n])
                        0x4D, 0x85, 0xC0, // 15: test r8, r8
                        0x75, 0x04, // 18: jnz +4                   (non-NULL → .valid at 24)
                        0x31, 0xC0, // 20: xor eax, eax             (return 0)
                        0x5D, // 22: pop rbp
                        0xC3, // 23: ret
                        // .valid (offset 24):
                        0x31, 0xC0, // 24: xor eax, eax             (len = 0)
                        // .strlen_loop (offset 26):
                        0x41, 0x0F, 0xB6, 0x0C, 0x00, // 26: movzx ecx, byte [r8 + rax]
                        0x85, 0xC9, // 31: test ecx, ecx
                        0x74, 0x05, // 33: jz +5                    (NUL → .done at 40)
                        0x48, 0xFF, 0xC0, // 35: inc rax                (len++)
                        0xEB, 0xF2, // 38: jmp -14                  (→ .strlen_loop at 26)
                        // .done (offset 40):
                        0x5D, // 40: pop rbp
                        0xC3, // 41: ret
                    };
                    try module.defineFunctionBytes(elf_func_ids[i], &x64_environ_len, &.{});
                } else if (std.mem.eql(u8, name, "cot_environ_ptr")) {
                    // x86-64: copy envp[n] into linear memory at wasm offset 0x7F000 + n*4096
                    // Cranelift CC: rdi=vmctx, rsi=caller_vmctx, rdx=n
                    // Same pattern as x64_arg_ptr but uses envp from vmctx+0x30010
                    // and writes to 0x7F000 instead of 0xAF000
                    const x64_environ_ptr = [_]u8{
                        0x55, //  0: push rbp
                        0x48, 0x89, 0xE5, //  1: mov rbp, rsp
                        0x48, 0x8B, 0x87, 0x10, 0x00, 0x03, 0x00, //  4: mov rax, [rdi + 0x30010]  (envp)
                        0x4C, 0x8B, 0x04, 0xD0, // 11: mov r8, [rax + rdx*8]   (r8 = envp[n], src)
                        0x4D, 0x85, 0xC0, // 15: test r8, r8
                        0x75, 0x04, // 18: jnz +4                   (non-NULL → .valid at 24)
                        0x31, 0xC0, // 20: xor eax, eax             (return 0)
                        0x5D, // 22: pop rbp
                        0xC3, // 23: ret
                        // .valid (offset 24):
                        // dest = linmem + 0x7F000 + n*4096
                        0x48, 0x89, 0xD0, // 24: mov rax, rdx              (n)
                        0x48, 0xC1, 0xE0, 0x0C, // 27: shl rax, 12             (n * 4096)
                        0x4C, 0x8D, 0x8F, 0x00, 0x00, 0x04, 0x00, // 31: lea r9, [rdi + 0x40000]  (linmem base)
                        0x49, 0x81, 0xC1, 0x00, 0xF0, 0x07, 0x00, // 38: add r9, 0x7F000
                        0x49, 0x01, 0xC1, // 45: add r9, rax               (dest = linmem + 0x7F000 + n*4096)
                        0x48, 0x89, 0xD1, // 48: mov rcx, rdx              (save n for return value)
                        0x41, 0xBA, 0xFF, 0x0F, 0x00, 0x00, // 51: mov r10d, 4095          (max copy)
                        // .copy_loop (offset 57):
                        0x4D, 0x85, 0xD2, // 57: test r10, r10
                        0x74, 0x16, // 60: jz +22                  (→ .truncate at 84)
                        0x41, 0x0F, 0xB6, 0x00, // 62: movzx eax, byte [r8]
                        0x41, 0x88, 0x01, // 66: mov [r9], al
                        0x85, 0xC0, // 69: test eax, eax
                        0x74, 0x0F, // 71: jz +15                  (NUL found → .done at 88)
                        0x49, 0xFF, 0xC0, // 73: inc r8
                        0x49, 0xFF, 0xC1, // 76: inc r9
                        0x49, 0xFF, 0xCA, // 79: dec r10
                        0xEB, 0xE5, // 82: jmp -27                 (→ .copy_loop at 57)
                        // .truncate (offset 84):
                        0x41, 0xC6, 0x01, 0x00, // 84: mov byte [r9], 0  (NUL-terminate)
                        // .done (offset 88): return wasm offset 0x7F000 + n*4096
                        0x48, 0xC7, 0xC0, 0x00, 0xF0, 0x07, 0x00, // 88: mov rax, 0x7F000
                        0x48, 0xC1, 0xE1, 0x0C, // 95: shl rcx, 12  (n * 4096)
                        0x48, 0x01, 0xC8, // 99: add rax, rcx  (0x7F000 + n*4096)
                        0x5D, //102: pop rbp
                        0xC3, //103: ret
                    };
                    try module.defineFunctionBytes(elf_func_ids[i], &x64_environ_ptr, &.{});
                } else if (std.mem.eql(u8, name, "cot_net_socket")) {
                    // x86-64 Linux syscall for socket(domain, type, protocol)
                    // Cranelift CC: rdi=vmctx, rsi=caller_vmctx, rdx=domain, rcx=type, r8=protocol
                    // Linux socket: rax=41(SYS_socket), rdi=domain, rsi=type, rdx=protocol
                    const x64_socket = [_]u8{
                        0x55, // push rbp
                        0x48, 0x89, 0xE5, // mov rbp, rsp
                        0x48, 0x89, 0xD7, // mov rdi, rdx  (domain)
                        0x48, 0x89, 0xCE, // mov rsi, rcx  (type)
                        0x4C, 0x89, 0xC2, // mov rdx, r8   (protocol)
                        0x48, 0xC7, 0xC0, 0x29, 0x00, 0x00, 0x00, // mov rax, 41  (SYS_socket)
                        0x0F, 0x05, // syscall
                        0x5D, // pop rbp
                        0xC3, // ret
                    };
                    try module.defineFunctionBytes(elf_func_ids[i], &x64_socket, &.{});
                } else if (std.mem.eql(u8, name, "cot_net_bind")) {
                    // x86-64 Linux syscall for bind(fd, addr, addrlen)
                    // Cranelift CC: rdi=vmctx, rsi=caller_vmctx, rdx=fd, rcx=addr(wasm), r8=addrlen
                    // Linux bind: rax=49(SYS_bind), rdi=fd, rsi=addr, rdx=addrlen
                    const x64_bind = [_]u8{
                        0x55, // push rbp
                        0x48, 0x89, 0xE5, // mov rbp, rsp
                        0x4C, 0x8D, 0x8F, 0x00, 0x00, 0x04, 0x00, // lea r9, [rdi + 0x40000]  (linmem base)
                        0x49, 0x01, 0xC9, // add r9, rcx  (real addr = linmem + wasm_ptr)
                        0x48, 0x89, 0xD7, // mov rdi, rdx  (fd)
                        0x4C, 0x89, 0xCE, // mov rsi, r9   (addr)
                        0x4C, 0x89, 0xC2, // mov rdx, r8   (addrlen)
                        0x48, 0xC7, 0xC0, 0x31, 0x00, 0x00, 0x00, // mov rax, 49  (SYS_bind)
                        0x0F, 0x05, // syscall
                        0x5D, // pop rbp
                        0xC3, // ret
                    };
                    try module.defineFunctionBytes(elf_func_ids[i], &x64_bind, &.{});
                } else if (std.mem.eql(u8, name, "cot_net_listen")) {
                    // x86-64 Linux syscall for listen(fd, backlog)
                    // Cranelift CC: rdi=vmctx, rsi=caller_vmctx, rdx=fd, rcx=backlog
                    // Linux listen: rax=50(SYS_listen), rdi=fd, rsi=backlog
                    const x64_listen = [_]u8{
                        0x55, // push rbp
                        0x48, 0x89, 0xE5, // mov rbp, rsp
                        0x48, 0x89, 0xD7, // mov rdi, rdx  (fd)
                        0x48, 0x89, 0xCE, // mov rsi, rcx  (backlog)
                        0x48, 0xC7, 0xC0, 0x32, 0x00, 0x00, 0x00, // mov rax, 50  (SYS_listen)
                        0x0F, 0x05, // syscall
                        0x5D, // pop rbp
                        0xC3, // ret
                    };
                    try module.defineFunctionBytes(elf_func_ids[i], &x64_listen, &.{});
                } else if (std.mem.eql(u8, name, "cot_net_accept")) {
                    // x86-64 Linux syscall for accept(fd, NULL, NULL)
                    // Cranelift CC: rdi=vmctx, rsi=caller_vmctx, rdx=fd
                    // Linux accept: rax=43(SYS_accept), rdi=fd, rsi=addr(NULL), rdx=addrlen(NULL)
                    const x64_accept = [_]u8{
                        0x55, // push rbp
                        0x48, 0x89, 0xE5, // mov rbp, rsp
                        0x48, 0x89, 0xD7, // mov rdi, rdx  (fd)
                        0x31, 0xF6, // xor esi, esi  (addr = NULL)
                        0x31, 0xD2, // xor edx, edx  (addrlen = NULL)
                        0x48, 0xC7, 0xC0, 0x2B, 0x00, 0x00, 0x00, // mov rax, 43  (SYS_accept)
                        0x0F, 0x05, // syscall
                        0x5D, // pop rbp
                        0xC3, // ret
                    };
                    try module.defineFunctionBytes(elf_func_ids[i], &x64_accept, &.{});
                } else if (std.mem.eql(u8, name, "cot_net_connect")) {
                    // x86-64 Linux syscall for connect(fd, addr, addrlen)
                    // Cranelift CC: rdi=vmctx, rsi=caller_vmctx, rdx=fd, rcx=addr(wasm), r8=addrlen
                    // Linux connect: rax=42(SYS_connect), rdi=fd, rsi=addr, rdx=addrlen
                    const x64_connect = [_]u8{
                        0x55, // push rbp
                        0x48, 0x89, 0xE5, // mov rbp, rsp
                        0x4C, 0x8D, 0x8F, 0x00, 0x00, 0x04, 0x00, // lea r9, [rdi + 0x40000]  (linmem base)
                        0x49, 0x01, 0xC9, // add r9, rcx  (real addr = linmem + wasm_ptr)
                        0x48, 0x89, 0xD7, // mov rdi, rdx  (fd)
                        0x4C, 0x89, 0xCE, // mov rsi, r9   (addr)
                        0x4C, 0x89, 0xC2, // mov rdx, r8   (addrlen)
                        0x48, 0xC7, 0xC0, 0x2A, 0x00, 0x00, 0x00, // mov rax, 42  (SYS_connect)
                        0x0F, 0x05, // syscall
                        0x5D, // pop rbp
                        0xC3, // ret
                    };
                    try module.defineFunctionBytes(elf_func_ids[i], &x64_connect, &.{});
                } else if (std.mem.eql(u8, name, "cot_net_set_reuse_addr")) {
                    // x86-64 Linux syscall for setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, &val, 4)
                    // Cranelift CC: rdi=vmctx, rsi=caller_vmctx, rdx=fd
                    // Linux: SOL_SOCKET=1, SO_REUSEADDR=2
                    // SYS_setsockopt = 54
                    const x64_reuse = [_]u8{
                        0x55, //  0: push rbp
                        0x48, 0x89, 0xE5, //  1: mov rbp, rsp
                        0x48, 0x83, 0xEC, 0x10, //  4: sub rsp, 16
                        0xC7, 0x04, 0x24, 0x01, 0x00, 0x00, 0x00, //  8: mov dword [rsp], 1  (optval = 1)
                        0x48, 0x89, 0xD7, // 15: mov rdi, rdx  (fd)
                        0xBE, 0x01, 0x00, 0x00, 0x00, // 18: mov esi, 1  (SOL_SOCKET)
                        0xBA, 0x02, 0x00, 0x00, 0x00, // 23: mov edx, 2  (SO_REUSEADDR)
                        0x48, 0x89, 0xE1, // 28: mov rcx, rsp  (&optval, arg4 in rcx for syscall→r10)
                        0x49, 0x89, 0xCA, // 31: mov r10, rcx  (Linux syscall: arg4 in r10)
                        0x41, 0xB8, 0x04, 0x00, 0x00, 0x00, // 34: mov r8d, 4  (optlen)
                        0x48, 0xC7, 0xC0, 0x36, 0x00, 0x00, 0x00, // 40: mov rax, 54  (SYS_setsockopt)
                        0x0F, 0x05, // 47: syscall
                        0x48, 0x83, 0xC4, 0x10, // 49: add rsp, 16
                        0x5D, // 53: pop rbp
                        0xC3, // 54: ret
                    };
                    try module.defineFunctionBytes(elf_func_ids[i], &x64_reuse, &.{});
                } else if (std.mem.eql(u8, name, "cot_epoll_create")) {
                    // x86-64 Linux: epoll_create1(0)
                    // SYS_epoll_create1 = 291
                    const x64_epoll_create = [_]u8{
                        0x55, // push rbp
                        0x48, 0x89, 0xE5, // mov rbp, rsp
                        0x31, 0xFF, // xor edi, edi  (flags = 0)
                        0x48, 0xC7, 0xC0, 0x23, 0x01, 0x00, 0x00, // mov rax, 291 (SYS_epoll_create1)
                        0x0F, 0x05, // syscall
                        0x5D, // pop rbp
                        0xC3, // ret
                    };
                    try module.defineFunctionBytes(elf_func_ids[i], &x64_epoll_create, &.{});
                } else if (std.mem.eql(u8, name, "cot_epoll_add")) {
                    // x86-64 Linux: epoll_ctl(epfd, EPOLL_CTL_ADD, fd, &event)
                    // rdx=epfd, rcx=fd, r8=events_mask
                    // SYS_epoll_ctl = 233, EPOLL_CTL_ADD = 1
                    // Build struct epoll_event on stack: events(u32@0), data.fd(i32@4)
                    const x64_epoll_add = [_]u8{
                        0x55, //  0: push rbp
                        0x48, 0x89, 0xE5, //  1: mov rbp, rsp
                        0x48, 0x83, 0xEC, 0x10, //  4: sub rsp, 16
                        0x44, 0x89, 0x04, 0x24, //  8: mov [rsp], r8d  (events = events_mask)
                        0x89, 0x4C, 0x24, 0x04, // 12: mov [rsp+4], ecx (data.fd = fd)
                        0x48, 0x89, 0xD7, // 16: mov rdi, rdx  (epfd)
                        0xBE, 0x01, 0x00, 0x00, 0x00, // 19: mov esi, 1  (EPOLL_CTL_ADD)
                        0x48, 0x89, 0xCA, // 24: mov rdx, rcx  (fd)
                        0x49, 0x89, 0xE2, // 27: mov r10, rsp  (&event, arg4)
                        0x48, 0xC7, 0xC0, 0xE9, 0x00, 0x00, 0x00, // 30: mov rax, 233 (SYS_epoll_ctl)
                        0x0F, 0x05, // 37: syscall
                        0x48, 0x83, 0xC4, 0x10, // 39: add rsp, 16
                        0x5D, // 43: pop rbp
                        0xC3, // 44: ret
                    };
                    try module.defineFunctionBytes(elf_func_ids[i], &x64_epoll_add, &.{});
                } else if (std.mem.eql(u8, name, "cot_epoll_del")) {
                    // x86-64 Linux: epoll_ctl(epfd, EPOLL_CTL_DEL, fd, NULL)
                    // rdx=epfd, rcx=fd. SYS_epoll_ctl = 233, EPOLL_CTL_DEL = 2
                    const x64_epoll_del = [_]u8{
                        0x55, // push rbp
                        0x48, 0x89, 0xE5, // mov rbp, rsp
                        0x48, 0x89, 0xD7, // mov rdi, rdx  (epfd)
                        0xBE, 0x02, 0x00, 0x00, 0x00, // mov esi, 2  (EPOLL_CTL_DEL)
                        0x48, 0x89, 0xCA, // mov rdx, rcx  (fd)
                        0x4D, 0x31, 0xD2, // xor r10, r10  (event = NULL)
                        0x48, 0xC7, 0xC0, 0xE9, 0x00, 0x00, 0x00, // mov rax, 233 (SYS_epoll_ctl)
                        0x0F, 0x05, // syscall
                        0x5D, // pop rbp
                        0xC3, // ret
                    };
                    try module.defineFunctionBytes(elf_func_ids[i], &x64_epoll_del, &.{});
                } else if (std.mem.eql(u8, name, "cot_epoll_wait")) {
                    // x86-64 Linux: epoll_wait(epfd, events, maxevents, timeout=-1)
                    // rdx=epfd, rcx=buf_ptr(wasm), r8=max_events
                    // SYS_epoll_wait = 232, timeout=-1 (block indefinitely)
                    const x64_epoll_wait = [_]u8{
                        0x55, // push rbp
                        0x48, 0x89, 0xE5, // mov rbp, rsp
                        0x4C, 0x8D, 0x8F, 0x00, 0x00, 0x04, 0x00, // lea r9, [rdi + 0x40000] (linmem)
                        0x49, 0x01, 0xC9, // add r9, rcx  (real buf ptr)
                        0x48, 0x89, 0xD7, // mov rdi, rdx  (epfd)
                        0x4C, 0x89, 0xCE, // mov rsi, r9   (events buf)
                        0x4C, 0x89, 0xC2, // mov rdx, r8   (maxevents)
                        0x49, 0xC7, 0xC2, 0xFF, 0xFF, 0xFF, 0xFF, // mov r10, -1  (timeout = block)
                        0x48, 0xC7, 0xC0, 0xE8, 0x00, 0x00, 0x00, // mov rax, 232 (SYS_epoll_wait)
                        0x0F, 0x05, // syscall
                        0x5D, // pop rbp
                        0xC3, // ret
                    };
                    try module.defineFunctionBytes(elf_func_ids[i], &x64_epoll_wait, &.{});
                } else if (std.mem.eql(u8, name, "cot_set_nonblocking")) {
                    // x86-64 Linux: fcntl(fd, F_GETFL) + fcntl(fd, F_SETFL, flags|O_NONBLOCK)
                    // rdx=fd. SYS_fcntl=72, F_GETFL=3, F_SETFL=4, O_NONBLOCK=0x800
                    const x64_set_nonblocking = [_]u8{
                        0x55, //  0: push rbp
                        0x48, 0x89, 0xE5, //  1: mov rbp, rsp
                        0x41, 0x54, //  4: push r12  (save fd)
                        0x49, 0x89, 0xD4, //  6: mov r12, rdx  (fd)
                        0x4C, 0x89, 0xE7, //  9: mov rdi, r12  (fd)
                        0xBE, 0x03, 0x00, 0x00, 0x00, // 12: mov esi, 3  (F_GETFL)
                        0x48, 0xC7, 0xC0, 0x48, 0x00, 0x00, 0x00, // 17: mov rax, 72 (SYS_fcntl)
                        0x0F, 0x05, // 24: syscall
                        0x48, 0x85, 0xC0, // 26: test rax, rax
                        0x78, 0x15, // 29: js +21  (→ error at 52)
                        0x48, 0x0D, 0x00, 0x08, 0x00, 0x00, // 31: or rax, 0x800 (O_NONBLOCK)
                        0x4C, 0x89, 0xE7, // 37: mov rdi, r12  (fd)
                        0xBE, 0x04, 0x00, 0x00, 0x00, // 40: mov esi, 4  (F_SETFL)
                        0x48, 0x89, 0xC2, // 45: mov rdx, rax  (new flags)
                        0x48, 0xC7, 0xC0, 0x48, 0x00, 0x00, 0x00, // 48: mov rax, 72 (SYS_fcntl)
                        0x0F, 0x05, // 55: syscall
                        // 57: (fall through — rax has result or error)
                        0x41, 0x5C, // 57: pop r12
                        0x5D, // 59: pop rbp
                        0xC3, // 60: ret
                    };
                    try module.defineFunctionBytes(elf_func_ids[i], &x64_set_nonblocking, &.{});
                } else if (std.mem.eql(u8, name, "cot_fork")) {
                    // x86-64 Linux: fork() syscall (SYS_fork = 57)
                    const x64_fork = [_]u8{
                        0x55, // push rbp
                        0x48, 0x89, 0xE5, // mov rbp, rsp
                        0x48, 0xC7, 0xC0, 0x39, 0x00, 0x00, 0x00, // mov rax, 57 (SYS_fork)
                        0x0F, 0x05, // syscall
                        0x5D, // pop rbp
                        0xC3, // ret
                    };
                    try module.defineFunctionBytes(elf_func_ids[i], &x64_fork, &.{});
                } else if (std.mem.eql(u8, name, "cot_waitpid")) {
                    // x86-64 Linux: wait4(pid, &status, options, NULL) (SYS_wait4 = 61)
                    // Cranelift CC: rdi=vmctx, rsi=caller_vmctx, rdx=pid
                    // Returns exit code = (status >> 8) & 0xFF
                    const x64_waitpid = [_]u8{
                        0x55, //  0: push rbp
                        0x48, 0x89, 0xE5, //  1: mov rbp, rsp
                        0x48, 0x83, 0xEC, 0x10, //  4: sub rsp, 16  (status on stack)
                        0x48, 0x89, 0xD7, //  8: mov rdi, rdx  (pid)
                        0x48, 0x89, 0xE6, // 11: mov rsi, rsp  (status_ptr)
                        0x31, 0xD2, // 14: xor edx, edx  (options = 0)
                        0x49, 0xC7, 0xC2, 0x00, 0x00, 0x00, 0x00, // 16: mov r10, 0  (rusage = NULL)
                        0x48, 0xC7, 0xC0, 0x3D, 0x00, 0x00, 0x00, // 23: mov rax, 61  (SYS_wait4)
                        0x0F, 0x05, // 30: syscall
                        0x48, 0x85, 0xC0, // 32: test rax, rax
                        0x78, 0x09, // 35: js +9  (error → return rax)
                        0x8B, 0x04, 0x24, // 37: mov eax, [rsp]  (load status as i32)
                        0xC1, 0xE8, 0x08, // 40: shr eax, 8  (status >> 8)
                        0x25, 0xFF, 0x00, 0x00, 0x00, // 43: and eax, 0xFF  (& 0xFF)
                        0x48, 0x83, 0xC4, 0x10, // 48: add rsp, 16
                        0x5D, // 52: pop rbp
                        0xC3, // 53: ret
                    };
                    try module.defineFunctionBytes(elf_func_ids[i], &x64_waitpid, &.{});
                } else if (std.mem.eql(u8, name, "cot_pipe")) {
                    // x86-64 Linux: pipe2(fds_ptr, 0) (SYS_pipe2 = 293)
                    // Returns packed: (write_fd << 32) | read_fd  (as i64 from stack)
                    const x64_pipe = [_]u8{
                        0x55, // push rbp
                        0x48, 0x89, 0xE5, // mov rbp, rsp
                        0x48, 0x83, 0xEC, 0x10, // sub rsp, 16  (stack for fds)
                        0x48, 0x89, 0xE7, // mov rdi, rsp  (fds_ptr)
                        0x31, 0xF6, // xor esi, esi  (flags = 0)
                        0x48, 0xC7, 0xC0, 0x25, 0x01, 0x00, 0x00, // mov rax, 293  (SYS_pipe2)
                        0x0F, 0x05, // syscall
                        0x48, 0x85, 0xC0, // test rax, rax
                        0x78, 0x04, // js +4  (error → return rax)
                        0x48, 0x8B, 0x04, 0x24, // mov rax, [rsp]  (packed fds as i64)
                        0x48, 0x83, 0xC4, 0x10, // add rsp, 16
                        0x5D, // pop rbp
                        0xC3, // ret
                    };
                    try module.defineFunctionBytes(elf_func_ids[i], &x64_pipe, &.{});
                } else if (std.mem.eql(u8, name, "cot_dup2")) {
                    // x86-64 Linux: dup2(oldfd, newfd) (SYS_dup2 = 33)
                    // Cranelift CC: rdi=vmctx, rsi=caller_vmctx, rdx=oldfd, rcx=newfd
                    const x64_dup2 = [_]u8{
                        0x55, // push rbp
                        0x48, 0x89, 0xE5, // mov rbp, rsp
                        0x48, 0x89, 0xD7, // mov rdi, rdx  (oldfd)
                        0x48, 0x89, 0xCE, // mov rsi, rcx  (newfd)
                        0x48, 0xC7, 0xC0, 0x21, 0x00, 0x00, 0x00, // mov rax, 33  (SYS_dup2)
                        0x0F, 0x05, // syscall
                        0x5D, // pop rbp
                        0xC3, // ret
                    };
                    try module.defineFunctionBytes(elf_func_ids[i], &x64_dup2, &.{});
                } else if (std.mem.eql(u8, name, "cot_execve")) {
                    // x86-64 Linux: execve(path, argv, envp) (SYS_execve = 59)
                    // Cranelift CC: rdi=vmctx, rsi=caller_vmctx, rdx=path, rcx=argv, r8=envp
                    // All pointers are wasm offsets — add linmem base (vmctx + 0x40000)
                    // execve with argv/envp pointer fixup (wasm offsets → real addresses)
                    const x64_execve = [_]u8{
                        0x55, //  0: push rbp
                        0x48, 0x89, 0xE5, //  1: mov rbp, rsp
                        0x4C, 0x8D, 0x8F, 0x00, 0x00, 0x04, 0x00, //  4: lea r9, [rdi + 0x40000] (linmem)
                        0x4C, 0x01, 0xCA, // 11: add rdx, r9  (path)
                        0x4C, 0x01, 0xC9, // 14: add rcx, r9  (argv)
                        0x4D, 0x01, 0xC8, // 17: add r8, r9   (envp)
                        // Fixup argv pointers
                        0x49, 0x89, 0xCA, // 20: mov r10, rcx
                        0x4D, 0x8B, 0x1A, // 23: ldr r11, [r10]  (argv_loop)
                        0x4D, 0x85, 0xDB, // 26: test r11, r11
                        0x74, 0x0C, // 29: jz +12 → 43 (argv_done)
                        0x4D, 0x01, 0xCB, // 31: add r11, r9
                        0x4D, 0x89, 0x1A, // 34: mov [r10], r11
                        0x49, 0x83, 0xC2, 0x08, // 37: add r10, 8
                        0xEB, 0xEC, // 41: jmp -20 → 23 (argv_loop)
                        // Fixup envp pointers
                        0x4D, 0x89, 0xC2, // 43: mov r10, r8  (argv_done)
                        0x4D, 0x8B, 0x1A, // 46: ldr r11, [r10]  (envp_loop)
                        0x4D, 0x85, 0xDB, // 49: test r11, r11
                        0x74, 0x0C, // 52: jz +12 → 66 (envp_done)
                        0x4D, 0x01, 0xCB, // 54: add r11, r9
                        0x4D, 0x89, 0x1A, // 57: mov [r10], r11
                        0x49, 0x83, 0xC2, 0x08, // 60: add r10, 8
                        0xEB, 0xEC, // 64: jmp -20 → 46 (envp_loop)
                        // Call execve
                        0x48, 0x89, 0xD7, // 66: mov rdi, rdx (path)
                        0x48, 0x89, 0xCE, // 69: mov rsi, rcx (argv)
                        0x4C, 0x89, 0xC2, // 72: mov rdx, r8  (envp)
                        0x48, 0xC7, 0xC0, 0x3B, 0x00, 0x00, 0x00, // 75: mov rax, 59 (SYS_execve)
                        0x0F, 0x05, // 82: syscall
                        0x5D, // 84: pop rbp
                        0xC3, // 85: ret
                    };
                    try module.defineFunctionBytes(elf_func_ids[i], &x64_execve, &.{});
                } else if (std.mem.eql(u8, name, "cot_kqueue_create") or
                    std.mem.eql(u8, name, "cot_kevent_add") or
                    std.mem.eql(u8, name, "cot_kevent_del") or
                    std.mem.eql(u8, name, "cot_kevent_wait"))
                {
                    // kqueue is macOS-only — return -1 on Linux
                    const x64_stub_neg1 = [_]u8{
                        0x48, 0xC7, 0xC0, 0xFF, 0xFF, 0xFF, 0xFF, // mov rax, -1
                        0xC3, // ret
                    };
                    try module.defineFunctionBytes(elf_func_ids[i], &x64_stub_neg1, &.{});
                }
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
        //   0x30010: envp (i64, char**) - set by _main wrapper from OS
        //   0x40000 - 0xFFFFFF: Linear memory (~15.7MB heap)
        //     0xBF000 (linmem+0x7F000): environ string buffer for @environ_ptr
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
        //   1. Saves argc/argv/envp (before lea rdi clobbers them)
        //   2. Loads address of vmctx_data
        //   3. Initializes heap base pointer (requires runtime address)
        //   4. Stores argc/argv/envp into vmctx+0x30000/0x30008/0x30010
        //   5. Calls __wasm_main(vmctx, vmctx)
        //   6. Returns result
        //
        // On Linux, main(int argc, char **argv, char **envp): edi=argc, rsi=argv, rdx=envp
        //
        // x86-64 code (System V AMD64 ABI):
        //   push   rbp                        ; 1  byte  (offset 0)
        //   mov    rbp, rsp                   ; 3  bytes (offset 1)
        //   push   rdi                        ; 1  byte  (offset 4)  save argc
        //   push   rsi                        ; 1  byte  (offset 5)  save argv
        //   push   rdx                        ; 1  byte  (offset 6)  save envp
        //   lea    rdi, [rip + vmctx_data]    ; 7  bytes (offset 7, reloc at 10)
        //   lea    rax, [rdi + 0x40000]       ; 7  bytes (offset 14)
        //   mov    [rdi + 0x20000], rax       ; 7  bytes (offset 21)
        //   pop    rax                        ; 1  byte  (offset 28) restore envp
        //   mov    [rdi + 0x30010], rax       ; 7  bytes (offset 29) store envp
        //   pop    rax                        ; 1  byte  (offset 36) restore argv
        //   mov    [rdi + 0x30008], rax       ; 7  bytes (offset 37) store argv
        //   pop    rax                        ; 1  byte  (offset 44) restore argc
        //   cdqe                              ; 2  bytes (offset 45) sign-extend eax→rax
        //   mov    [rdi + 0x30000], rax       ; 7  bytes (offset 47) store argc
        //   mov    rsi, rdi                   ; 3  bytes (offset 54)
        //   call   __wasm_main                ; 5  bytes (offset 57, reloc at 58)
        //   pop    rbp                        ; 1  byte  (offset 62)
        //   ret                               ; 1  byte  (offset 63)
        // Total: 64 bytes
        // =================================================================
        var wrapper_code = std.ArrayListUnmanaged(u8){};
        defer wrapper_code.deinit(self.allocator);

        // push rbp (0x55)                                     offset 0
        try wrapper_code.append(self.allocator, 0x55);

        // mov rbp, rsp (48 89 e5)                             offset 1
        try wrapper_code.appendSlice(self.allocator, &[_]u8{ 0x48, 0x89, 0xe5 });

        // push rdi (0x57) — save argc before lea overwrites rdi   offset 4
        try wrapper_code.append(self.allocator, 0x57);

        // push rsi (0x56) — save argv before mov rsi,rdi      offset 5
        try wrapper_code.append(self.allocator, 0x56);

        // push rdx (0x52) — save envp                          offset 6
        try wrapper_code.append(self.allocator, 0x52);

        // lea rdi, [rip + vmctx_data] (48 8d 3d XX XX XX XX)  offset 7, reloc disp at 10
        try wrapper_code.appendSlice(self.allocator, &[_]u8{ 0x48, 0x8d, 0x3d, 0x00, 0x00, 0x00, 0x00 });

        // lea rax, [rdi + 0x40000] (48 8d 87 00 00 04 00)     offset 14
        try wrapper_code.appendSlice(self.allocator, &[_]u8{ 0x48, 0x8d, 0x87, 0x00, 0x00, 0x04, 0x00 });

        // mov [rdi + 0x20000], rax (48 89 87 00 00 02 00)     offset 21
        try wrapper_code.appendSlice(self.allocator, &[_]u8{ 0x48, 0x89, 0x87, 0x00, 0x00, 0x02, 0x00 });

        // pop rax (0x58) — restore envp                        offset 28
        try wrapper_code.append(self.allocator, 0x58);

        // mov [rdi + 0x30010], rax (48 89 87 10 00 03 00)     offset 29 store envp
        try wrapper_code.appendSlice(self.allocator, &[_]u8{ 0x48, 0x89, 0x87, 0x10, 0x00, 0x03, 0x00 });

        // pop rax (0x58) — restore argv                        offset 36
        try wrapper_code.append(self.allocator, 0x58);

        // mov [rdi + 0x30008], rax (48 89 87 08 00 03 00)     offset 37
        try wrapper_code.appendSlice(self.allocator, &[_]u8{ 0x48, 0x89, 0x87, 0x08, 0x00, 0x03, 0x00 });

        // pop rax (0x58) — restore argc                        offset 44
        try wrapper_code.append(self.allocator, 0x58);

        // cdqe (48 98) — sign-extend eax to rax                offset 45
        try wrapper_code.appendSlice(self.allocator, &[_]u8{ 0x48, 0x98 });

        // mov [rdi + 0x30000], rax (48 89 87 00 00 03 00)     offset 47
        try wrapper_code.appendSlice(self.allocator, &[_]u8{ 0x48, 0x89, 0x87, 0x00, 0x00, 0x03, 0x00 });

        // mov rsi, rdi (48 89 fe)                              offset 54
        try wrapper_code.appendSlice(self.allocator, &[_]u8{ 0x48, 0x89, 0xfe });

        // call __wasm_main (e8 XX XX XX XX)                    offset 57, reloc disp at 58
        try wrapper_code.appendSlice(self.allocator, &[_]u8{ 0xe8, 0x00, 0x00, 0x00, 0x00 });

        // pop rbp (5d)                                         offset 62
        try wrapper_code.append(self.allocator, 0x5d);

        // ret (c3)                                             offset 63
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

        const relocs = [_]FinalizedMachReloc{
            // lea rdi, [rip + vmctx_data] - displacement at offset 10
            .{
                .offset = 10,
                .kind = Reloc.X86PCRel4,
                .target = FinalizedRelocTarget{ .ExternalName = vmctx_name_ref },
                .addend = -4, // RIP-relative adjustment
            },
            // call __wasm_main - displacement at offset 58
            .{
                .offset = 58,
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

        const is_wasm_gc = self.target.isWasmGC();

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
        // WasmGC: Register GC struct types in linker BEFORE runtime functions.
        // GC types get indices 0..N-1, shifting all function type indices by N.
        // This must happen before ARC/runtime function assembly so that
        // call_indirect type indices in those functions are correct.
        // ====================================================================
        if (is_wasm_gc) {
            const wasm_link = @import("codegen/wasm/link.zig");
            for (type_reg.types.items) |t| {
                if (t == .struct_type) {
                    const st = t.struct_type;
                    var gc_fields = try self.allocator.alloc(wasm_link.GcFieldType, st.fields.len);
                    defer self.allocator.free(gc_fields);
                    for (st.fields, 0..) |field, fi| {
                        const is_float = field.type_idx == types_mod.TypeRegistry.F64 or
                            field.type_idx == types_mod.TypeRegistry.F32;
                        gc_fields[fi] = .{
                            .val_type = if (is_float) .f64 else .i64,
                            .mutable = true,
                        };
                    }
                    _ = try linker.addGcStructType(st.name, gc_fields);
                }
            }
        }

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
        // Note: cot_write is a stub on Wasm (native overrides with ARM64 syscall).
        // Wasm test output is handled host-side in main.zig testCommand.
        // Reference: Go runtime/print.go
        // ====================================================================
        const print_funcs = try print_runtime.addToLinker(self.allocator, &linker);

        // ====================================================================
        // Add WASI runtime functions (fd_write)
        // Reference: WASI preview1 fd_write
        // ====================================================================
        const wasi_funcs = try wasi_runtime.addToLinker(self.allocator, &linker, self.target);

        // ====================================================================
        // Add test runtime functions (Zig test runner pattern)
        // Reference: Zig test runner output format
        // ====================================================================
        const test_funcs = try test_runtime.addToLinker(self.allocator, &linker, print_funcs.write_idx, print_funcs.eprint_int_idx, wasi_funcs.time_idx);

        // ====================================================================
        // Add bench runtime functions (Go testing.B calibration pattern)
        // ====================================================================
        const bench_funcs = try bench_runtime.addToLinker(self.allocator, &linker, print_funcs.write_idx, print_funcs.eprint_int_idx, wasi_funcs.time_idx, self.bench_n);

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

        // Add test function names to index map (Zig)
        try func_indices.put(self.allocator, test_runtime.TEST_BEGIN_NAME, test_funcs.test_begin_idx);
        try func_indices.put(self.allocator, test_runtime.TEST_PRINT_NAME_NAME, test_funcs.test_print_name_idx);
        try func_indices.put(self.allocator, test_runtime.TEST_PASS_NAME, test_funcs.test_pass_idx);
        try func_indices.put(self.allocator, test_runtime.TEST_FAIL_NAME, test_funcs.test_fail_idx);
        try func_indices.put(self.allocator, test_runtime.TEST_SUMMARY_NAME, test_funcs.test_summary_idx);
        try func_indices.put(self.allocator, test_runtime.TEST_STORE_FAIL_VALUES_NAME, test_funcs.test_store_fail_values_idx);

        // Add bench function names to index map (Go testing.B)
        try func_indices.put(self.allocator, bench_runtime.BENCH_PRINT_NAME_NAME, bench_funcs.bench_print_name_idx);
        try func_indices.put(self.allocator, bench_runtime.BENCH_CALIBRATE_START_NAME, bench_funcs.bench_calibrate_start_idx);
        try func_indices.put(self.allocator, bench_runtime.BENCH_CALIBRATE_END_NAME, bench_funcs.bench_calibrate_end_idx);
        try func_indices.put(self.allocator, bench_runtime.BENCH_MEASURE_START_NAME, bench_funcs.bench_measure_start_idx);
        try func_indices.put(self.allocator, bench_runtime.BENCH_MEASURE_END_NAME, bench_funcs.bench_measure_end_idx);
        try func_indices.put(self.allocator, bench_runtime.BENCH_GET_N_NAME, bench_funcs.bench_get_n_idx);
        try func_indices.put(self.allocator, bench_runtime.BENCH_SUMMARY_NAME, bench_funcs.bench_summary_idx);

        // Add user function names (offset by ARC function count)
        for (funcs, 0..) |*ir_func, i| {
            try func_indices.put(self.allocator, ir_func.name, @intCast(i + runtime_func_count));
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

        if (!is_wasm_gc) {
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

                pipeline_debug.log(.codegen, "driver: metadata for {s} at offset {d}, dtor_idx={d}", .{ entry.key_ptr.*, offset, dtor_idx });
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
                    pipeline_debug.log(.codegen, "driver: string literal at offset {d}: \"{s}\"", .{ offset, str });
                }
            }
        }

        // ====================================================================
        // Pass 2: Generate code for each function
        // ====================================================================
        for (funcs) |*ir_func| {
            // Build SSA
            var ssa_builder = try ssa_builder_mod.SSABuilder.init(self.allocator, ir_func, type_reg, self.target);
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
            var params: [32]wasm.ValType = undefined;
            // WasmGC: parallel array for GC-aware param types (ref types for structs)
            var gc_params: [32]wasm.WasmType = undefined;
            {
                // Build Wasm param types, decomposing compound types (slice, string)
                // into 2 separate i64 entries (ptr, len). This must match the SSA
                // builder's arg decomposition in ssa_builder.zig.
                // WasmGC: no decomposition — structs are single (ref null $T) values.
                var wasm_param_idx: usize = 0;
                for (ir_func.params) |param| {
                    const param_type = type_reg.get(param.type_idx);
                    const is_string_or_slice = !is_wasm_gc and (param.type_idx == types_mod.TypeRegistry.STRING or param_type == .slice);
                    const type_size = type_reg.sizeOf(param.type_idx);
                    const is_large_struct = !is_wasm_gc and param_type == .struct_type and type_size > 8;

                    // WasmGC: struct and pointer-to-struct params are single (ref null $typeidx)
                    // Reference: Kotlin/Dart WasmGC — struct refs in params, including self: *Type
                    const is_gc_struct_param = is_wasm_gc and (param_type == .struct_type or
                        (param_type == .pointer and type_reg.get(param_type.pointer.elem) == .struct_type));
                    if (is_gc_struct_param) {
                        const st = if (param_type == .struct_type)
                            param_type.struct_type
                        else
                            type_reg.get(param_type.pointer.elem).struct_type;
                        const gc_type_idx = linker.gc_struct_name_map.get(st.name) orelse 0;
                        gc_params[wasm_param_idx] = wasm.WasmType.gcRefNull(gc_type_idx);
                        params[wasm_param_idx] = .i64; // placeholder for param_count
                        wasm_param_idx += 1;
                    } else if (is_string_or_slice) {
                        // String/slice: 2 i64 params (ptr+len)
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
            const ret_is_compound = !is_wasm_gc and (ir_func.return_type == types_mod.TypeRegistry.STRING or ret_type_info == .slice);
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
                // Convert results to WasmType
                var gc_results: [4]wasm.WasmType = undefined;
                for (results, 0..) |r, ri| gc_results[ri] = wasm.WasmType.fromVal(r);
                break :blk try linker.addTypeWasm(gc_params[0..param_count], gc_results[0..results.len]);
            } else try linker.addType(params[0..param_count], results);

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
            const gc_name_map = if (is_wasm_gc) &linker.gc_struct_name_map else null;
            const body = try wasm.generateFunc(self.allocator, ssa_func, &func_indices, &string_offsets, &metadata_addrs, &func_table_indices, gc_name_map);
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
