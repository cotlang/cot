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
const arc = @import("codegen/arc.zig"); // ARC runtime

// Native codegen modules (Cranelift-style AOT compiler)
const native_compile = @import("codegen/native/compile.zig");
const wasm_parser = @import("codegen/native/wasm_parser.zig");
const wasm_to_clif = @import("codegen/native/wasm_to_clif/translator.zig");
const wasm_decoder = @import("codegen/native/wasm_to_clif/decoder.zig");
const wasm_func_translator = @import("codegen/native/wasm_to_clif/func_translator.zig");
const clif = @import("ir/clif/mod.zig");
const macho = @import("codegen/native/macho.zig");
const elf = @import("codegen/native/elf.zig");

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
        var chk = checker_mod.Checker.init(self.allocator, &tree, &type_reg, &err_reporter, &global_scope);
        defer chk.deinit();
        try chk.checkFile();
        if (err_reporter.hasErrors()) return error.TypeCheckError;

        // Lower to IR
        var lowerer = lower_mod.Lowerer.init(self.allocator, &tree, &type_reg, &err_reporter, &chk);
        defer lowerer.deinit();
        var ir_result = try lowerer.lower();
        defer ir_result.deinit();
        if (err_reporter.hasErrors()) return error.LowerError;

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

        var checkers = std.ArrayListUnmanaged(checker_mod.Checker){};
        defer {
            for (checkers.items) |*chk| chk.deinit();
            checkers.deinit(self.allocator);
        }

        for (parsed_files.items) |*pf| {
            var err_reporter = errors_mod.ErrorReporter.init(&pf.source, null);
            var chk = checker_mod.Checker.init(self.allocator, &pf.tree, &type_reg, &err_reporter, &global_scope);
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
            const full_path = try std.fs.path.join(self.allocator, &.{ file_dir, import_path });
            defer self.allocator.free(full_path);
            try self.parseFileRecursive(full_path, parsed_files, seen_files);
        }

        try parsed_files.append(self.allocator, .{ .path = path_copy, .source_text = source_text, .source = src, .tree = tree });
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

        // Create reusable translator context
        var func_translator = wasm_func_translator.WasmFuncTranslator.init(self.allocator);
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
            defer self.allocator.free(wasm_ops);

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

            pipeline_debug.log(.codegen, "driver: translated function {d} to CLIF", .{func_idx});

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
            .macos => try self.generateMachO(compiled_funcs.items, wasm_module.exports),
            .linux => try self.generateElf(compiled_funcs.items, wasm_module.exports),
            .freestanding => return error.UnsupportedObjectFormat,
        };

        return object_bytes;
    }

    /// Generate Mach-O object file from compiled functions.
    fn generateMachO(self: *Driver, compiled_funcs: []const native_compile.CompiledCode, exports: []const wasm_parser.Export) ![]u8 {
        _ = exports;

        // Collect all machine code into a single text section
        var total_size: usize = 0;
        for (compiled_funcs) |cf| {
            total_size += cf.codeSize();
        }

        var code = try self.allocator.alloc(u8, total_size);
        var offset: usize = 0;
        for (compiled_funcs) |cf| {
            const code_bytes = cf.code();
            @memcpy(code[offset..][0..code_bytes.len], code_bytes);
            offset += code_bytes.len;
        }

        // For now, return raw code - full Mach-O generation will use macho.zig
        // TODO: Use macho.zig to wrap in proper Mach-O format
        return code;
    }

    /// Generate ELF object file from compiled functions.
    fn generateElf(self: *Driver, compiled_funcs: []const native_compile.CompiledCode, exports: []const wasm_parser.Export) ![]u8 {
        _ = exports;

        // Collect all machine code into a single text section
        var total_size: usize = 0;
        for (compiled_funcs) |cf| {
            total_size += cf.codeSize();
        }

        var code = try self.allocator.alloc(u8, total_size);
        var offset: usize = 0;
        for (compiled_funcs) |cf| {
            const code_bytes = cf.code();
            @memcpy(code[offset..][0..code_bytes.len], code_bytes);
            offset += code_bytes.len;
        }

        // For now, return raw code - full ELF generation will use elf.zig
        // TODO: Use elf.zig to wrap in proper ELF format
        return code;
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
        // Add ARC runtime functions first (they get indices 0, 1, 2, ...)
        // ====================================================================
        const arc_funcs = try arc.addToLinker(self.allocator, &linker);
        // Get actual count from linker - never hardcode (Go: len(hostImports))
        const arc_func_count = linker.funcCount();

        // Set minimum table size of 1 for call_indirect (destructor calls)
        // Table entry 0 is reserved (null/no destructor)
        linker.setTableSize(1);

        // Build function name -> index mapping
        // ARC functions come first, then user functions
        var func_indices = wasm_gen.FuncIndexMap{};
        defer func_indices.deinit(self.allocator);

        // Add ARC function names to index map
        try func_indices.put(self.allocator, arc.ALLOC_NAME, arc_funcs.alloc_idx);
        try func_indices.put(self.allocator, arc.RETAIN_NAME, arc_funcs.retain_idx);
        try func_indices.put(self.allocator, arc.RELEASE_NAME, arc_funcs.release_idx);
        try func_indices.put(self.allocator, arc.STRING_CONCAT_NAME, arc_funcs.string_concat_idx);
        try func_indices.put(self.allocator, arc.APPEND_NAME, arc_funcs.append_idx);
        try func_indices.put(self.allocator, arc.MEMSET_ZERO_NAME, arc_funcs.memset_zero_idx);

        // Add user function names (offset by ARC function count)
        for (funcs, 0..) |*ir_func, i| {
            try func_indices.put(self.allocator, ir_func.name, @intCast(i + arc_func_count));
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

        // Find all *_deinit functions and add them to the table
        for (funcs, 0..) |*ir_func, i| {
            if (std.mem.endsWith(u8, ir_func.name, "_deinit")) {
                // Extract type name from "TypeName_deinit"
                const type_name = ir_func.name[0 .. ir_func.name.len - 7]; // Remove "_deinit"
                const func_idx: u32 = @intCast(i + arc_func_count);

                // Add to table (table_index starts at 1, 0 is reserved for null)
                const table_idx = try linker.addTableFunc(func_idx);
                try destructor_table.put(type_name, table_idx);

                pipeline_debug.log(.codegen, "driver: destructor {s} at table[{d}] = func[{d}]", .{ ir_func.name, table_idx, func_idx });
            }
        }

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
            for (0..param_count) |i| {
                params[i] = .i64;
            }
            const has_return = ir_func.return_type != types_mod.TypeRegistry.VOID;
            const results: []const wasm.ValType = if (has_return) &[_]wasm.ValType{.i64} else &[_]wasm.ValType{};

            // Add function type to linker
            const type_idx = try linker.addType(params[0..param_count], results);

            // Generate function body code using Go-style two-pass architecture
            const body = try wasm.generateFunc(self.allocator, ssa_func, &func_indices, &string_offsets, &metadata_addrs);
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

/// Convert a single wasm ValType to WasmValType.
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
