//! Package Compiler
//!
//! Compiles packages in dependency order, producing one .cbo per package,
//! then links them together into a final executable.
//!
//! Cache Invalidation:
//! Uses content-based hashing for reliable cache invalidation:
//! - Source file content changes are detected via xxHash
//! - Configuration (cot.json) changes invalidate cache
//! - Compiler version changes invalidate cache
//! - Dependency changes transitively invalidate dependent packages

const std = @import("std");
const Allocator = std.mem.Allocator;

const cot = @import("../root.zig");
const ir = cot.ir;
const bytecode = cot.bytecode;
const Module = bytecode.Module;
const bytecode_mod = bytecode.module;
const Constant = bytecode_mod.Constant;
const package = @import("package.zig");
const build_cache = @import("build/cache.zig");

const Package = package.Package;
const PackageManager = package.PackageManager;
const ExportTable = package.ExportTable;
const ExportedSymbol = package.ExportedSymbol;
const SymbolKind = package.SymbolKind;

const BuildCache = build_cache.BuildCache;
const CacheManifest = build_cache.CacheManifest;
const BuildMode = build_cache.BuildMode;

/// Exported types from a compiled package (for cross-package type resolution)
pub const ExportedTypes = struct {
    /// Map struct names to their IR types
    struct_types: std.StringHashMapUnmanaged(*const ir.StructType),
    /// Map union names to their IR types
    union_types: std.StringHashMapUnmanaged(*const ir.UnionType),
    /// Map enum names to their variant values
    enum_types: std.StringHashMapUnmanaged(std.StringHashMap(i64)),
    /// Map function names to their return types
    fn_return_types: std.StringHashMapUnmanaged(ir.Type),

    pub fn init() ExportedTypes {
        return .{
            .struct_types = .empty,
            .union_types = .empty,
            .enum_types = .empty,
            .fn_return_types = .empty,
        };
    }

    pub fn deinit(self: *ExportedTypes, allocator: Allocator) void {
        self.struct_types.deinit(allocator);
        self.union_types.deinit(allocator);
        // Free inner enum variant maps
        var it = self.enum_types.iterator();
        while (it.next()) |entry| {
            entry.value_ptr.deinit();
        }
        self.enum_types.deinit(allocator);
        self.fn_return_types.deinit(allocator);
    }
};

/// Result of compiling a single package
pub const CompiledPackage = struct {
    name: []const u8,
    bytecode: []u8,
    exports: ExportTable,
    /// Routine indices that are entry points (exported functions)
    exported_routines: std.StringHashMapUnmanaged(u32),
    /// Exported types for cross-package type resolution
    exported_types: ExportedTypes,
    /// Keep IR module alive for type pointers
    ir_module: ?*ir.Module,

    pub fn deinit(self: *CompiledPackage, allocator: Allocator) void {
        allocator.free(self.name);
        allocator.free(self.bytecode);
        self.exports.deinit();
        self.exported_routines.deinit(allocator);
        self.exported_types.deinit(allocator);
        // Note: We intentionally don't deinit ir_module here because the module's
        // internal data (strings, AST nodes) may have complex ownership. Since
        // this is typically called at program exit, the OS will reclaim the memory.
        // TODO: Implement proper ownership tracking for IR module cleanup.
        _ = self.ir_module;
    }
};

/// Compiles packages in a workspace
pub const WorkspaceCompiler = struct {
    allocator: Allocator,
    pm: *PackageManager,
    compiled: std.StringHashMapUnmanaged(*CompiledPackage),
    cache_dir: ?[]const u8,

    /// Build cache for content-based invalidation
    build_cache: ?*BuildCache,

    /// Build mode (affects cache key)
    build_mode: BuildMode,

    /// Verbose logging
    verbose: bool,

    pub fn init(allocator: Allocator, pm: *PackageManager) WorkspaceCompiler {
        return .{
            .allocator = allocator,
            .pm = pm,
            .compiled = .empty,
            .cache_dir = null,
            .build_cache = null,
            .build_mode = .debug,
            .verbose = false,
        };
    }

    pub fn deinit(self: *WorkspaceCompiler) void {
        var it = self.compiled.iterator();
        while (it.next()) |entry| {
            entry.value_ptr.*.deinit(self.allocator);
            self.allocator.destroy(entry.value_ptr.*);
        }
        self.compiled.deinit(self.allocator);

        if (self.build_cache) |cache| {
            cache.deinit();
            self.allocator.destroy(cache);
        }

        if (self.cache_dir) |dir| {
            self.allocator.free(dir);
        }
    }

    /// Set cache directory for compiled packages
    pub fn setCacheDir(self: *WorkspaceCompiler, dir: []const u8) !void {
        if (self.cache_dir) |old| {
            self.allocator.free(old);
        }
        self.cache_dir = try self.allocator.dupe(u8, dir);

        // Initialize build cache
        if (self.build_cache) |old_cache| {
            old_cache.deinit();
            self.allocator.destroy(old_cache);
        }

        const cache_ptr = try self.allocator.create(BuildCache);
        cache_ptr.* = try BuildCache.init(self.allocator, dir);
        self.build_cache = cache_ptr;
    }

    /// Set build mode (debug/release)
    pub fn setBuildMode(self: *WorkspaceCompiler, is_release: bool) void {
        self.build_mode = BuildMode.fromBool(is_release);
    }

    /// Enable verbose logging
    pub fn setVerbose(self: *WorkspaceCompiler, verbose: bool) void {
        self.verbose = verbose;
    }

    /// Get config path for a package
    fn getConfigPath(self: *WorkspaceCompiler, pkg: *Package) !?[]const u8 {
        // Look for cot.json in the package directory
        const config_path = try std.fs.path.join(self.allocator, &.{ pkg.path, "cot.json" });
        std.fs.cwd().access(config_path, .{}) catch {
            self.allocator.free(config_path);
            return null;
        };
        return config_path;
    }

    /// Collect cache keys from compiled dependencies
    fn collectDependencyKeys(self: *WorkspaceCompiler, pkg: *Package) !std.StringHashMapUnmanaged(u64) {
        var dep_keys: std.StringHashMapUnmanaged(u64) = .empty;
        errdefer dep_keys.deinit(self.allocator);

        for (pkg.dependencies.items) |dep_name| {
            // Get cache key from already-compiled dependency
            if (self.build_cache) |cache| {
                if (try cache.getCacheKey(dep_name)) |key| {
                    try dep_keys.put(self.allocator, dep_name, key);
                }
            }
        }

        return dep_keys;
    }

    /// Try to load a package from cache (using content-based validation)
    fn loadFromCache(self: *WorkspaceCompiler, pkg: *Package) !?*CompiledPackage {
        const cache = self.build_cache orelse return null;

        // Get config path
        const config_path = try self.getConfigPath(pkg);
        defer if (config_path) |p| self.allocator.free(p);

        // Collect dependency cache keys
        var dep_keys = try self.collectDependencyKeys(pkg);
        defer dep_keys.deinit(self.allocator);

        // Validate cache
        const status = try cache.isCacheValid(
            pkg.name,
            pkg.source_files.items,
            config_path,
            self.build_mode,
            &dep_keys,
        );

        if (!status.valid) {
            if (self.verbose) {
                std.debug.print("  Cache miss for '{s}': {s}\n", .{
                    pkg.name,
                    status.reason.toString(),
                });
            }
            return null;
        }

        if (self.verbose) {
            std.debug.print("  Cache hit for '{s}' (key: {x:0>16})\n", .{ pkg.name, status.cache_key });
        }

        // Load bytecode from cache
        const cache_dir = self.cache_dir orelse return null;
        const bytecode_filename = try std.fmt.allocPrint(self.allocator, "{s}.cbo", .{pkg.name});
        defer self.allocator.free(bytecode_filename);
        const bytecode_path = try std.fs.path.join(self.allocator, &.{
            cache_dir,
            pkg.name,
            bytecode_filename,
        });
        defer self.allocator.free(bytecode_path);

        const cache_file = std.fs.cwd().openFile(bytecode_path, .{}) catch return null;
        defer cache_file.close();

        const bytecode_data = cache_file.readToEndAlloc(self.allocator, 100 * 1024 * 1024) catch return null;

        // Create CompiledPackage from cached bytecode
        const compiled_ptr = try self.allocator.create(CompiledPackage);
        compiled_ptr.* = .{
            .name = try self.allocator.dupe(u8, pkg.name),
            .bytecode = bytecode_data,
            .exports = ExportTable.init(self.allocator),
            .exported_routines = .empty,
            .exported_types = ExportedTypes.init(),
            .ir_module = null,
        };

        return compiled_ptr;
    }

    /// Save a compiled package to cache
    fn saveToCache(self: *WorkspaceCompiler, pkg: *Package, compiled: *CompiledPackage) void {
        const cache = self.build_cache orelse return;
        const cache_dir = self.cache_dir orelse return;

        // Create package cache directory
        const pkg_cache_dir = std.fs.path.join(self.allocator, &.{ cache_dir, pkg.name }) catch return;
        defer self.allocator.free(pkg_cache_dir);
        std.fs.cwd().makePath(pkg_cache_dir) catch {};

        // Write bytecode
        const bytecode_filename = std.fmt.allocPrint(self.allocator, "{s}.cbo", .{pkg.name}) catch return;
        defer self.allocator.free(bytecode_filename);
        const cache_path = std.fs.path.join(self.allocator, &.{ pkg_cache_dir, bytecode_filename }) catch return;
        defer self.allocator.free(cache_path);

        const cache_file = std.fs.cwd().createFile(cache_path, .{}) catch return;
        defer cache_file.close();
        cache_file.writeAll(compiled.bytecode) catch return;

        // Get config path
        const config_path = self.getConfigPath(pkg) catch null;
        defer if (config_path) |p| self.allocator.free(p);

        // Collect dependency keys
        var dep_keys = self.collectDependencyKeys(pkg) catch return;
        defer dep_keys.deinit(self.allocator);

        // Create and save manifest
        var manifest = cache.createManifest(
            pkg.name,
            pkg.source_files.items,
            config_path,
            self.build_mode,
            &dep_keys,
        ) catch return;
        defer manifest.deinit(self.allocator);

        cache.saveManifest(&manifest) catch return;

        if (self.verbose) {
            std.debug.print("  Cached '{s}' (key: {x:0>16})\n", .{ pkg.name, manifest.cache_key });
        }
    }

    /// Compile all packages in dependency order
    pub fn compileAll(self: *WorkspaceCompiler) !void {
        // Ensure build order is computed
        try self.pm.computeBuildOrder();

        // Compile each package in order
        for (self.pm.build_order.items) |pkg_name| {
            const pkg = self.pm.getPackage(pkg_name) orelse continue;
            try self.compilePackage(pkg);
        }
    }

    /// Compile a single package
    pub fn compilePackage(self: *WorkspaceCompiler, pkg: *Package) !void {
        // Skip if already compiled
        if (self.compiled.contains(pkg.name)) return;

        // Ensure all dependencies are compiled first
        for (pkg.dependencies.items) |dep_name| {
            if (self.pm.getPackage(dep_name)) |dep_pkg| {
                try self.compilePackage(dep_pkg);
            }
        }

        // Try to load from cache first
        if (self.cache_dir != null) {
            if (try self.loadFromCache(pkg)) |cached| {
                try self.compiled.put(self.allocator, pkg.name, cached);
                pkg.compiled = true;
                return;
            }
        }

        // Build DependencyTypeContext from compiled dependencies
        var dep_types = cot.ir_lower.DependencyTypeContext.init(self.allocator);
        defer dep_types.deinit();

        for (pkg.dependencies.items) |dep_name| {
            if (self.compiled.get(dep_name)) |compiled_dep| {
                // Import struct types
                var struct_it = compiled_dep.exported_types.struct_types.iterator();
                while (struct_it.next()) |entry| {
                    dep_types.struct_types.put(entry.key_ptr.*, entry.value_ptr.*) catch {};
                }
                // Import union types
                var union_it = compiled_dep.exported_types.union_types.iterator();
                while (union_it.next()) |entry| {
                    dep_types.union_types.put(entry.key_ptr.*, entry.value_ptr.*) catch {};
                }
                // Import enum types (need to clone the variant maps)
                var enum_it = compiled_dep.exported_types.enum_types.iterator();
                while (enum_it.next()) |entry| {
                    var variants = std.StringHashMap(i64).init(self.allocator);
                    var var_it = entry.value_ptr.iterator();
                    while (var_it.next()) |var_entry| {
                        variants.put(var_entry.key_ptr.*, var_entry.value_ptr.*) catch {};
                    }
                    dep_types.enum_types.put(entry.key_ptr.*, variants) catch {};
                }
                // Import function return types
                var fn_it = compiled_dep.exported_types.fn_return_types.iterator();
                while (fn_it.next()) |entry| {
                    dep_types.fn_return_types.put(entry.key_ptr.*, entry.value_ptr.*) catch {};
                }
            }
        }

        // Compile the package
        const result = try compilePackageFiles(
            self.allocator,
            pkg,
            &dep_types,
        );

        // Store result
        const compiled_ptr = try self.allocator.create(CompiledPackage);
        compiled_ptr.* = result;
        try self.compiled.put(self.allocator, pkg.name, compiled_ptr);

        // Save to cache
        if (self.cache_dir != null) {
            self.saveToCache(pkg, compiled_ptr);
        }

        // Mark package as compiled
        pkg.compiled = true;
    }

    /// Link all compiled packages into final executable
    pub fn link(self: *WorkspaceCompiler, entry_package: []const u8, output_path: []const u8) !void {
        _ = self.compiled.get(entry_package) orelse {
            return error.EntryPackageNotCompiled;
        };

        // Create static linker
        var linker = PackageLinker.init(self.allocator);
        defer linker.deinit();

        // Add all compiled packages in dependency order
        for (self.pm.build_order.items) |pkg_name| {
            if (self.compiled.get(pkg_name)) |compiled_pkg| {
                try linker.addPackage(compiled_pkg);
            }
        }

        // Link into final module and write to output
        var merged = try linker.link(entry_package);
        defer merged.deinit();

        const file = try std.fs.cwd().createFile(output_path, .{});
        defer file.close();

        var write_buffer: [8192]u8 = undefined;
        var buffered = file.writer(&write_buffer);
        try merged.serialize(&buffered.interface);
        try buffered.interface.flush();
    }

    /// Get compiled package by name
    pub fn getCompiled(self: *const WorkspaceCompiler, name: []const u8) ?*CompiledPackage {
        return self.compiled.get(name);
    }
};

/// Static linker that merges multiple compiled packages into a single executable
pub const PackageLinker = struct {
    allocator: Allocator,
    packages: std.ArrayListUnmanaged(*CompiledPackage),

    // Merged module components
    constants: std.ArrayListUnmanaged(Constant),
    types: std.ArrayListUnmanaged(bytecode_mod.TypeDef),
    routines: std.ArrayListUnmanaged(bytecode_mod.RoutineDef),
    code: std.ArrayListUnmanaged(u8),
    exports: std.ArrayListUnmanaged(bytecode_mod.ExportEntry),
    vtables: std.ArrayListUnmanaged(bytecode_mod.VTableDef),

    // Mapping tables for remapping indices during merge
    const_remap: std.AutoHashMapUnmanaged(u32, u16), // (pkg_idx << 16 | old_idx) -> new_idx
    routine_remap: std.AutoHashMapUnmanaged(u32, u16), // (pkg_idx << 16 | old_idx) -> new_idx

    pub fn init(allocator: Allocator) PackageLinker {
        return .{
            .allocator = allocator,
            .packages = .empty,
            .constants = .empty,
            .types = .empty,
            .routines = .empty,
            .code = .empty,
            .exports = .empty,
            .vtables = .empty,
            .const_remap = .empty,
            .routine_remap = .empty,
        };
    }

    pub fn deinit(self: *PackageLinker) void {
        self.packages.deinit(self.allocator);
        self.constants.deinit(self.allocator);
        self.types.deinit(self.allocator);
        self.routines.deinit(self.allocator);
        self.code.deinit(self.allocator);
        self.exports.deinit(self.allocator);
        self.vtables.deinit(self.allocator);
        self.const_remap.deinit(self.allocator);
        self.routine_remap.deinit(self.allocator);
    }

    /// Add a compiled package to be linked
    pub fn addPackage(self: *PackageLinker, pkg: *CompiledPackage) !void {
        try self.packages.append(self.allocator, pkg);
    }

    /// Link all packages into a single Module
    pub fn link(self: *PackageLinker, entry_package: []const u8) !Module {
        // Deserialize and merge all packages
        for (self.packages.items, 0..) |pkg, pkg_idx| {
            try self.mergePackage(pkg, @intCast(pkg_idx));
        }

        // Find entry point (main function in entry package)
        var entry_point: u32 = 0xFFFFFFFF;
        for (self.packages.items, 0..) |pkg, pkg_idx| {
            if (std.mem.eql(u8, pkg.name, entry_package)) {
                // Look for "main" routine in this package
                const key: u32 = @as(u32, @intCast(pkg_idx)) << 16;
                var fbs = std.io.fixedBufferStream(pkg.bytecode);
                const reader = fbs.reader();
                var pkg_module = Module.deserialize(self.allocator, reader) catch continue;
                defer pkg_module.deinit();

                for (pkg_module.routines, 0..) |routine, r_idx| {
                    const name_const = pkg_module.getConstant(routine.name_index) orelse continue;
                    const name = switch (name_const) {
                        .identifier => |n| n,
                        .string => |s| s,
                        else => continue,
                    };
                    if (std.mem.eql(u8, name, "main")) {
                        const old_key = key | @as(u32, @intCast(r_idx));
                        if (self.routine_remap.get(old_key)) |new_idx| {
                            entry_point = self.routines.items[new_idx].code_offset;
                        }
                        break;
                    }
                }
                break;
            }
        }

        // Build the merged module
        var result = Module.init(self.allocator);
        result.header.entry_point = entry_point;
        result.constants = try self.allocator.dupe(Constant, self.constants.items);
        result.types = try self.allocator.dupe(bytecode_mod.TypeDef, self.types.items);
        result.routines = try self.allocator.dupe(bytecode_mod.RoutineDef, self.routines.items);
        result.code = try self.allocator.dupe(u8, self.code.items);
        result.exports = try self.allocator.dupe(bytecode_mod.ExportEntry, self.exports.items);
        result.vtables = try self.allocator.dupe(bytecode_mod.VTableDef, self.vtables.items);

        return result;
    }

    /// Merge a single package's bytecode into the linked output
    fn mergePackage(self: *PackageLinker, pkg: *CompiledPackage, pkg_idx: u16) !void {
        // Deserialize the package bytecode
        var fbs = std.io.fixedBufferStream(pkg.bytecode);
        const reader = fbs.reader();
        var pkg_module = try Module.deserialize(self.allocator, reader);
        defer pkg_module.deinit();

        const const_base: u16 = @intCast(self.constants.items.len);
        const routine_base: u16 = @intCast(self.routines.items.len);
        const code_base: u32 = @intCast(self.code.items.len);

        // Merge constants and build remap table
        for (pkg_module.constants, 0..) |constant, old_idx| {
            const key: u32 = @as(u32, pkg_idx) << 16 | @as(u32, @intCast(old_idx));
            const new_idx: u16 = @intCast(self.constants.items.len);
            try self.const_remap.put(self.allocator, key, new_idx);

            // Duplicate strings since pkg_module will be freed
            const duped_const = try self.dupeConstant(constant);
            try self.constants.append(self.allocator, duped_const);
        }

        // Merge types (remap name_index and field name_index)
        for (pkg_module.types) |type_def| {
            var new_type = type_def;
            new_type.name_index = self.remapConstant(pkg_idx, type_def.name_index);

            // Remap field name indices
            if (type_def.fields.len > 0) {
                var new_fields = try self.allocator.alloc(bytecode_mod.FieldDef, type_def.fields.len);
                for (type_def.fields, 0..) |field, i| {
                    new_fields[i] = field;
                    new_fields[i].name_index = self.remapConstant(pkg_idx, field.name_index);
                }
                new_type.fields = new_fields;
            }
            try self.types.append(self.allocator, new_type);
        }

        // Merge routines and build remap table
        for (pkg_module.routines, 0..) |routine, old_idx| {
            const key: u32 = @as(u32, pkg_idx) << 16 | @as(u32, @intCast(old_idx));
            const new_idx: u16 = @intCast(self.routines.items.len);
            try self.routine_remap.put(self.allocator, key, new_idx);

            var new_routine = routine;
            new_routine.name_index = self.remapConstant(pkg_idx, routine.name_index) + const_base;
            new_routine.code_offset = routine.code_offset + code_base;

            // Remap param name indices
            if (routine.params.len > 0) {
                var new_params = try self.allocator.alloc(bytecode_mod.ParamDef, routine.params.len);
                for (routine.params, 0..) |param, i| {
                    new_params[i] = param;
                    new_params[i].name_index = self.remapConstant(pkg_idx, param.name_index) + const_base;
                }
                new_routine.params = new_params;
            }

            // Remap local name indices
            if (routine.locals.len > 0) {
                var new_locals = try self.allocator.alloc(bytecode_mod.LocalDef, routine.locals.len);
                for (routine.locals, 0..) |local, i| {
                    new_locals[i] = local;
                    new_locals[i].name_index = self.remapConstant(pkg_idx, local.name_index) + const_base;
                }
                new_routine.locals = new_locals;
            }

            try self.routines.append(self.allocator, new_routine);
        }

        // Copy code (call_dynamic uses name-based lookup, no patching needed)
        try self.code.appendSlice(self.allocator, pkg_module.code);

        // Merge exports (remap indices)
        for (pkg_module.exports) |exp| {
            var new_exp = exp;
            new_exp.name_index = self.remapConstant(pkg_idx, exp.name_index) + const_base;
            new_exp.index = exp.index + routine_base;
            try self.exports.append(self.allocator, new_exp);
        }

        // Merge vtables
        for (pkg_module.vtables) |vt| {
            var new_vt = vt;
            // Duplicate vtable strings
            new_vt.trait_name = try self.allocator.dupe(u8, vt.trait_name);
            new_vt.type_name = try self.allocator.dupe(u8, vt.type_name);
            if (vt.methods.len > 0) {
                var new_methods = try self.allocator.alloc(bytecode_mod.VTableMethod, vt.methods.len);
                for (vt.methods, 0..) |method, i| {
                    new_methods[i] = .{
                        .method_name = try self.allocator.dupe(u8, method.method_name),
                        .fn_name = try self.allocator.dupe(u8, method.fn_name),
                    };
                }
                new_vt.methods = new_methods;
            }
            try self.vtables.append(self.allocator, new_vt);
        }
    }

    /// Remap a constant index from package-local to merged
    fn remapConstant(self: *PackageLinker, pkg_idx: u16, old_idx: u16) u16 {
        const key: u32 = @as(u32, pkg_idx) << 16 | @as(u32, old_idx);
        return self.const_remap.get(key) orelse old_idx;
    }

    /// Duplicate a constant for the merged module
    fn dupeConstant(self: *PackageLinker, constant: Constant) !Constant {
        return switch (constant) {
            .string => |s| .{ .string = try self.allocator.dupe(u8, s) },
            .fixed_string => |a| .{ .fixed_string = .{
                .data = try self.allocator.dupe(u8, a.data),
                .size = a.size,
            } },
            .identifier => |s| .{ .identifier = try self.allocator.dupe(u8, s) },
            else => constant,
        };
    }
};

/// Compile all files in a package
fn compilePackageFiles(
    backing_allocator: Allocator,
    pkg: *Package,
    dep_types: *cot.ir_lower.DependencyTypeContext,
) !CompiledPackage {
    // Use backing allocator for everything since IR module must persist
    const pkg_allocator = backing_allocator;

    const compiler = cot.compiler;
    const DiagnosticCollector = compiler.DiagnosticCollector;

    // Initialize diagnostic collector
    var collector = DiagnosticCollector.init(pkg_allocator);

    // Create NodeStore and StringInterner
    var strings = cot.base.StringInterner.init(pkg_allocator);
    var store = cot.ast.NodeStore.init(pkg_allocator, &strings);

    // Collect all statements from package files
    var all_stmts: std.ArrayListUnmanaged(cot.ast.StmtIdx) = .empty;

    // Parse all source files in the package
    for (pkg.source_files.items) |source_file| {
        const stmts = try parseFileIntoStore(pkg_allocator, source_file, &store, &strings, &collector);
        try all_stmts.appendSlice(pkg_allocator, stmts);

        if (collector.hasErrors()) {
            compiler.formatter.printToStderr(&collector, .{ .use_color = true });
            return error.CompilationFailed;
        }
    }

    const top_level = all_stmts.items;

    // Get first source file for error reporting
    const primary_file = if (pkg.source_files.items.len > 0)
        pkg.source_files.items[0]
    else
        pkg.name;

    // Run compile-time evaluation
    var evaluator = cot.comptime_eval.Evaluator.init(pkg_allocator, &store, &strings);
    defer evaluator.deinit();
    evaluator.setSourceFile(primary_file);

    const processed_stmts = evaluator.process(top_level) catch |err| {
        std.debug.print("Compile-time evaluation error in package '{s}': {}\n", .{ pkg.name, err });
        return error.CompilationFailed;
    };
    defer pkg_allocator.free(processed_stmts);

    // Lower AST to IR with dependency types
    const lower_result = cot.ir_lower.lowerWithDetails(
        pkg_allocator,
        &store,
        &strings,
        processed_stmts,
        primary_file,
        .{ .dependency_types = dep_types },
    );

    const ir_module = switch (lower_result) {
        .ok => |module| module,
        .err => |e| {
            if (e.detail) |detail| {
                std.debug.print("IR lowering error in package '{s}':\n", .{pkg.name});
                std.debug.print("  {s}:{d}:{d}: error: {s}\n", .{ primary_file, detail.line, detail.column, detail.message });
                if (detail.context.len > 0) {
                    std.debug.print("  while {s}\n", .{detail.context});
                }
            } else {
                std.debug.print("IR lowering error in package '{s}': {}\n", .{ pkg.name, e.kind });
            }
            return error.CompilationFailed;
        },
    };

    // Type check
    const TypeChecker = compiler.TypeChecker;
    var type_checker = TypeChecker.init(pkg_allocator, &collector, ir_module, primary_file);
    type_checker.check();

    if (collector.hasErrors()) {
        compiler.formatter.printToStderr(&collector, .{ .use_color = true });
        return error.CompilationFailed;
    }

    // Optimize
    _ = cot.ir_optimize.optimize(ir_module, .{});

    // Emit bytecode
    var emitter = cot.ir_emit_bytecode.BytecodeEmitter.init(pkg_allocator);
    var mod = emitter.emit(ir_module) catch |err| {
        std.debug.print("Bytecode emission error in package '{s}': {}\n", .{ pkg.name, err });
        return error.CompilationFailed;
    };

    // Serialize module to bytecode buffer
    var bytecode_list: std.ArrayListUnmanaged(u8) = .empty;
    mod.serialize(bytecode_list.writer(backing_allocator).any()) catch |err| {
        std.debug.print("Bytecode serialization error in package '{s}': {}\n", .{ pkg.name, err });
        return error.CompilationFailed;
    };
    const pkg_bytecode = bytecode_list.toOwnedSlice(backing_allocator) catch return error.OutOfMemory;

    // Build export table
    var exports = ExportTable.init(backing_allocator);

    // Extract exported symbols from IR module
    // For now, export all top-level functions
    for (ir_module.functions.items, 0..) |func, idx| {
        const name = func.name;
        // Skip internal/generated routines
        if (std.mem.startsWith(u8, name, "_") or std.mem.startsWith(u8, name, "$")) continue;

        try exports.put(name, .{
            .name = name,
            .kind = .function,
            .offset = @intCast(idx),
            .type_desc = null,
        });
    }

    // Extract exported types from IR module for dependents
    var exported_types = ExportedTypes.init();

    // Export struct types (from IR module)
    for (ir_module.structs.items) |struct_type| {
        try exported_types.struct_types.put(backing_allocator, struct_type.name, struct_type);
    }

    // Export union types
    for (ir_module.unions.items) |union_type| {
        try exported_types.union_types.put(backing_allocator, union_type.name, union_type);
    }

    // Export function return types (for type checking in dependents)
    for (ir_module.functions.items) |func| {
        const name = func.name;
        if (std.mem.startsWith(u8, name, "_") or std.mem.startsWith(u8, name, "$")) continue;
        try exported_types.fn_return_types.put(backing_allocator, name, func.signature.return_type);
    }

    // Export enum types (now stored in module.enums)
    var enum_it = ir_module.enums.iterator();
    while (enum_it.next()) |entry| {
        // Clone the variant map for export
        var variants = std.StringHashMap(i64).init(backing_allocator);
        var var_it = entry.value_ptr.variants.iterator();
        while (var_it.next()) |var_entry| {
            variants.put(var_entry.key_ptr.*, var_entry.value_ptr.*) catch {};
        }
        try exported_types.enum_types.put(backing_allocator, entry.key_ptr.*, variants);
    }

    return .{
        .name = try backing_allocator.dupe(u8, pkg.name),
        .bytecode = pkg_bytecode,
        .exports = exports,
        .exported_routines = .empty,
        .exported_types = exported_types,
        .ir_module = ir_module, // Keep alive for type pointers
    };
}

/// Parse a file into the NodeStore
fn parseFileIntoStore(
    allocator: Allocator,
    filename: []const u8,
    store: *cot.ast.NodeStore,
    strings: *cot.base.StringInterner,
    collector: anytype,
) ![]const cot.ast.StmtIdx {
    // Read source file
    const file = std.fs.cwd().openFile(filename, .{}) catch |err| {
        std.debug.print("Cannot open file '{s}': {}\n", .{ filename, err });
        return error.FileNotFound;
    };
    defer file.close();

    const source = file.readToEndAlloc(allocator, 10 * 1024 * 1024) catch |err| {
        std.debug.print("Cannot read file '{s}': {}\n", .{ filename, err });
        return error.ReadError;
    };

    // Lex
    var lexer = cot.lexer.Lexer.init(source);
    const tokens = lexer.tokenize(allocator) catch |err| {
        std.debug.print("Lexer error in '{s}': {}\n", .{ filename, err });
        return error.ParseError;
    };

    // Parse
    var parser = cot.parser.Parser.init(allocator, tokens, store, strings);
    defer parser.deinit();
    const stmts = parser.parse() catch |err| {
        std.debug.print("Parse error in '{s}': {}\n", .{ filename, err });
        return error.ParseError;
    };

    // Collect any parser errors
    if (parser.errors.items.len > 0) {
        for (parser.errors.items) |parse_err| {
            collector.addError(
                .E300_undefined_label,
                filename,
                cot.compiler.diagnostics.SourceRange.fromLoc(@intCast(parse_err.line), @intCast(parse_err.column)),
                "{s}",
                .{parse_err.message},
            );
        }
    }

    return stmts;
}

// Tests
test "workspace compiler init" {
    const allocator = std.testing.allocator;

    var pm = try PackageManager.init(allocator, "/tmp/test");
    defer pm.deinit();

    var wc = WorkspaceCompiler.init(allocator, &pm);
    defer wc.deinit();
}
