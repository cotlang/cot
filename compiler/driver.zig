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
const regalloc_mod = @import("codegen/native/regalloc.zig");
const stackalloc_mod = @import("codegen/native/stackalloc.zig");
const expand_calls = @import("codegen/native/expand_calls.zig");
const decompose = @import("codegen/native/decompose.zig");
const wasm_parser = @import("codegen/native/wasm_parser.zig");
const wasm_to_ssa = @import("codegen/native/wasm_to_ssa.zig");
const generic_codegen = @import("codegen/native/generic.zig");
const arm64_codegen = @import("codegen/native/arm64.zig");
const amd64_codegen = @import("codegen/native/amd64.zig");
const macho = @import("codegen/native/macho.zig");
const elf = @import("codegen/native/elf.zig");
const dwarf = @import("codegen/native/dwarf.zig");
const schedule = @import("ssa/passes/schedule.zig");
const layout = @import("ssa/passes/layout.zig");
const lower_wasm = @import("ssa/passes/lower_wasm.zig");
const target_mod = @import("core/target.zig");
const pipeline_debug = @import("pipeline_debug.zig");

// Wasm codegen
const wasm_old = @import("codegen/wasm.zig"); // Old module builder (for CodeBuilder)
const wasm = @import("codegen/wasm/wasm.zig"); // New Go-style package (for Linker)
const wasm_gen = @import("codegen/wasm_gen.zig");

// Native codegen modules - will be added in Round 5
// const arm64_codegen = @import("codegen/arm64.zig");
// const amd64_codegen = @import("codegen/amd64.zig");

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

        // Native target: AOT compilation path (Wasm → SSA → Native)
        // Step 1: Generate Wasm bytecode first
        const wasm_bytes = try self.generateWasmCode(funcs, type_reg);
        defer self.allocator.free(wasm_bytes);

        pipeline_debug.log(.codegen, "driver: AOT compiling {d} bytes of Wasm to native", .{wasm_bytes.len});

        // Step 2: Parse Wasm binary
        var wasm_module = try wasm_parser.parse(self.allocator, wasm_bytes);
        defer wasm_module.deinit();

        pipeline_debug.log(.codegen, "driver: parsed {d} functions from Wasm", .{wasm_module.code.len});

        // Step 3: Initialize native codegen based on target architecture
        var arm64_gen: ?arm64_codegen.ARM64CodeGen = null;
        var amd64_gen: ?amd64_codegen.AMD64CodeGen = null;
        defer if (arm64_gen) |*g| g.deinit();
        defer if (amd64_gen) |*g| g.deinit();

        if (self.target.arch == .arm64) {
            arm64_gen = arm64_codegen.ARM64CodeGen.init(self.allocator);
            arm64_gen.?.setTypeRegistry(type_reg);
            arm64_gen.?.setDebugInfo(source_file, source_text);
        } else if (self.target.arch == .amd64) {
            amd64_gen = amd64_codegen.AMD64CodeGen.init(self.allocator);
            amd64_gen.?.setTypeRegistry(type_reg);
            amd64_gen.?.setDebugInfo(source_file, source_text);
        } else {
            pipeline_debug.log(.codegen, "driver: unsupported native target: {s}", .{@tagName(self.target.arch)});
            return error.UnsupportedTarget;
        }

        // Step 4: Convert each Wasm function to SSA and generate native code
        var converter = wasm_to_ssa.WasmToSSA.init(self.allocator, &wasm_module);
        defer converter.deinit();

        for (0..wasm_module.code.len) |func_idx| {
            const ssa_func = converter.convert(func_idx) catch |e| {
                pipeline_debug.log(.codegen, "driver: failed to convert function {d}: {any}", .{ func_idx, e });
                continue;
            };
            defer {
                ssa_func.deinit();
                self.allocator.destroy(ssa_func);
            }

            pipeline_debug.log(.codegen, "driver: converted function {d} '{s}' to SSA", .{ func_idx, ssa_func.name });

            // Run SSA passes for native codegen
            try expand_calls.expandCalls(ssa_func, type_reg);
            try decompose.decompose(ssa_func, type_reg);
            try schedule.schedule(ssa_func);

            var regalloc_state = try regalloc_mod.regalloc(self.allocator, ssa_func, self.target);
            defer regalloc_state.deinit();

            const stack_result = try stackalloc_mod.stackalloc(ssa_func, regalloc_state.getSpillLive());
            const frame_size = stack_result.frame_size;

            // Generate native code
            if (arm64_gen) |*gen| {
                gen.setRegAllocState(&regalloc_state);
                gen.setFrameSize(frame_size);
                gen.generateBinary(ssa_func, ssa_func.name) catch |e| {
                    pipeline_debug.log(.codegen, "driver: ARM64 codegen failed for {s}: {any}", .{ ssa_func.name, e });
                    continue;
                };
                pipeline_debug.log(.codegen, "driver: generated ARM64 code for '{s}'", .{ssa_func.name});
            } else if (amd64_gen) |*gen| {
                gen.setRegAllocState(&regalloc_state);
                gen.setFrameSize(frame_size);
                gen.generateBinary(ssa_func, ssa_func.name) catch |e| {
                    pipeline_debug.log(.codegen, "driver: AMD64 codegen failed for {s}: {any}", .{ ssa_func.name, e });
                    continue;
                };
                pipeline_debug.log(.codegen, "driver: generated AMD64 code for '{s}'", .{ssa_func.name});
            }
        }

        // Step 5: Finalize and produce object file
        // NOTE: ARM64CodeGen.finalize() and AMD64CodeGen.finalize() already produce
        // complete object files (Mach-O or ELF respectively), so we return directly.
        if (arm64_gen) |*gen| {
            const object_file = try gen.finalize();
            pipeline_debug.log(.codegen, "driver: finalized {d} bytes of ARM64 Mach-O object", .{object_file.len});
            return object_file;
        } else if (amd64_gen) |*gen| {
            const object_file = try gen.finalize();
            pipeline_debug.log(.codegen, "driver: finalized {d} bytes of AMD64 ELF object", .{object_file.len});
            return object_file;
        }

        return error.NoCodeGenerated;
    }

    /// Generate WebAssembly binary.
    /// Uses Go-style Linker for module structure with proper SP globals.
    fn generateWasmCode(self: *Driver, funcs: []const ir_mod.Func, type_reg: *types_mod.TypeRegistry) ![]u8 {
        pipeline_debug.log(.codegen, "driver: generating Wasm for {d} functions", .{funcs.len});

        var linker = wasm.Linker.init(self.allocator);
        defer linker.deinit();

        // Configure memory (1 page = 64KB minimum)
        linker.setMemory(1, null);

        // Build function name -> index mapping
        var func_indices = wasm_gen.FuncIndexMap{};
        defer func_indices.deinit(self.allocator);
        for (funcs, 0..) |*ir_func, i| {
            try func_indices.put(self.allocator, ir_func.name, @intCast(i));
        }

        // Process each function
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
            try schedule.schedule(ssa_func);
            try layout.layout(ssa_func);  // Order blocks (Go's layout.go)
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

            // Generate function body code (still using old wasm_gen for now)
            // Note: ARC runtime functions not yet wired into main driver
            const body = try wasm_gen.genFuncWithIndices(self.allocator, ssa_func, &func_indices, null);
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
        // This includes proper sections: type, function, memory, global (SP), export, code
        var output: std.ArrayListUnmanaged(u8) = .{};
        try linker.emit(output.writer(self.allocator));
        return output.toOwnedSlice(self.allocator);
    }
};

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
