//! Build Pipeline
//!
//! Orchestrates building of workspace projects.
//!
//! Build modes:
//! - Development: Individual .cotc files in .cot-out/.cache/ for fast incremental builds
//! - Release: Bundled executables (no extension) or library packages (.cotpkg)
//!
//! Output conventions (all under workspace root .cot-out/):
//! - Apps (release): .cot-out/apps/<name>/<name> (no extension, like Go/Rust)
//! - Libraries (release): .cot-out/packages/<name>/<name>.cotpkg (package file)
//! - Development cache: .cot-out/.cache/<name>/<name>.cotc (compiled modules)

const std = @import("std");
const config = @import("../config.zig");
const workspace = @import("../workspace.zig");
const resolver = @import("../resolver.zig");
const discovery = @import("../discovery.zig");
const bundler = @import("bundler.zig");
const Allocator = std.mem.Allocator;
const builtin = @import("builtin");

// Import compiler components
const cot = @import("cot");
const ast = cot.ast;
const schema = cot.schema;

/// File extensions
pub const Extensions = struct {
    /// Compiled module (development cache)
    pub const compiled_module = ".cotc";
    /// Library package
    pub const library_package = ".cotpkg";
    /// Executable extension (platform-dependent)
    pub const executable = if (builtin.os.tag == .windows) ".exe" else "";
};

/// Build options
pub const BuildOptions = struct {
    /// Specific project to build (null = build all)
    project: ?[]const u8 = null,
    /// Output directory override
    output_dir: ?[]const u8 = null,
    /// Verbose output
    verbose: bool = false,
    /// Clean build (remove cache/output first)
    clean: bool = false,
    /// Full rebuild (remove ALL caches and recompile)
    rebuild: bool = false,
    /// Release mode (bundle everything into single file)
    release: bool = false,
};

/// Build result for a single file
pub const FileResult = struct {
    source: []const u8,
    output: []const u8,
    success: bool,
    error_message: ?[]const u8 = null,
};

/// Build result for a project
pub const ProjectResult = struct {
    name: []const u8,
    path: []const u8,
    files: std.ArrayList(FileResult),
    success: bool,
    total_files: usize = 0,
    compiled_files: usize = 0,
    failed_files: usize = 0,
    bundled: bool = false,
    bundle_path: ?[]const u8 = null,
    manifest_path: ?[]const u8 = null,
    errors: std.ArrayList([]const u8) = .{},

    pub fn init(allocator: Allocator, name: []const u8, path: []const u8) !ProjectResult {
        return .{
            .name = try allocator.dupe(u8, name),
            .path = try allocator.dupe(u8, path),
            .files = .{},
            .success = true,
        };
    }

    pub fn deinit(self: *ProjectResult, allocator: Allocator) void {
        allocator.free(self.name);
        allocator.free(self.path);
        for (self.files.items) |*f| {
            allocator.free(f.source);
            allocator.free(f.output);
            if (f.error_message) |e| allocator.free(e);
        }
        self.files.deinit(allocator);
        if (self.bundle_path) |p| allocator.free(p);
        if (self.manifest_path) |p| allocator.free(p);
        for (self.errors.items) |e| allocator.free(e);
        self.errors.deinit(allocator);
    }
};

/// Build result for entire workspace
pub const BuildResult = struct {
    projects: std.ArrayList(ProjectResult),
    total_projects: usize = 0,
    successful_projects: usize = 0,
    failed_projects: usize = 0,
    allocator: Allocator,

    pub fn init(allocator: Allocator) BuildResult {
        return .{
            .projects = .{},
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *BuildResult) void {
        for (self.projects.items) |*p| p.deinit(self.allocator);
        self.projects.deinit(self.allocator);
    }

    pub fn isSuccess(self: *const BuildResult) bool {
        return self.failed_projects == 0;
    }
};

/// Build pipeline
pub const Pipeline = struct {
    allocator: Allocator,
    options: BuildOptions,

    /// Cached schema metadata for current project (loaded once per project)
    schema_metadata: ?schema.SchemaMetadata = null,
    schema_file: ?schema.SchemaFile = null,

    const Self = @This();

    pub fn init(allocator: Allocator, options: BuildOptions) Self {
        return .{
            .allocator = allocator,
            .options = options,
            .schema_metadata = null,
            .schema_file = null,
        };
    }

    /// Clean up any cached schema data
    pub fn deinit(self: *Self) void {
        self.clearSchemaCache();
    }

    /// Clear the schema cache between projects
    fn clearSchemaCache(self: *Self) void {
        if (self.schema_metadata) |*sm| {
            sm.deinit();
            self.schema_metadata = null;
        }
        if (self.schema_file) |*sf| {
            sf.deinit();
            self.schema_file = null;
        }
    }

    /// Load schema for a project if configured
    fn loadProjectSchema(self: *Self, project: *const workspace.Project) !void {
        // Clear any previous schema
        self.clearSchemaCache();

        // Check if project has schema configuration
        const schema_config = project.config.schema orelse return;

        // Build full path to schema file
        const schema_path = try std.fs.path.join(self.allocator, &.{
            project.path,
            schema_config.file,
        });
        defer self.allocator.free(schema_path);

        // Try to load schema file
        var schema_file = schema.parseSchemaFile(self.allocator, schema_path) catch |err| {
            // Schema file not found or invalid - this is not a fatal error
            if (self.options.verbose) {
                std.debug.print("Warning: Could not load schema from {s}: {}\n", .{ schema_path, err });
            }
            return;
        };
        self.schema_file = schema_file;

        // Create schema metadata for runtime access
        self.schema_metadata = schema.SchemaMetadata.init(self.allocator, &schema_file) catch {
            return; // Silent failure - schema metadata creation failed
        };

        if (self.options.verbose) {
            std.debug.print("Loaded schema from {s} (database: {s}, version: {d})\n", .{
                schema_path,
                schema_file.database_name,
                schema_file.version,
            });
        }
    }

    /// Clean all cached files in the workspace (for --rebuild)
    fn cleanAllCaches(self: *Self, ws: *const workspace.Workspace) !void {
        if (self.options.verbose) {
            std.debug.print("Cleaning all caches...\n", .{});
        }

        // Delete entire .cot-out directory at workspace root
        const cot_out_dir = try std.fs.path.join(self.allocator, &.{ ws.root_path, ".cot-out" });
        defer self.allocator.free(cot_out_dir);
        std.fs.cwd().deleteTree(cot_out_dir) catch {};

        // Also clean legacy locations (bin/, .cot-cache/) for migration
        for (ws.apps.items) |project| {
            try self.cleanLegacyProjectCaches(project.path);
        }
        for (ws.packages.items) |project| {
            try self.cleanLegacyProjectCaches(project.path);
        }

        if (self.options.verbose) {
            std.debug.print("Cache cleanup complete.\n", .{});
        }
    }

    /// Clean legacy cache locations from a project (for migration from old structure)
    fn cleanLegacyProjectCaches(self: *Self, project_path: []const u8) !void {
        // Delete old .cot-cache directory
        const cache_dir = try std.fs.path.join(self.allocator, &.{ project_path, ".cot-cache" });
        defer self.allocator.free(cache_dir);
        std.fs.cwd().deleteTree(cache_dir) catch {};

        // Delete old bin directory
        const bin_dir = try std.fs.path.join(self.allocator, &.{ project_path, "bin" });
        defer self.allocator.free(bin_dir);
        std.fs.cwd().deleteTree(bin_dir) catch {};

        // Find and delete all .cbo files in the project directory
        try self.deleteFilesWithExtension(project_path, ".cbo");
    }

    /// Delete all files with a specific extension in a directory (recursively)
    fn deleteFilesWithExtension(self: *Self, dir_path: []const u8, extension: []const u8) !void {
        var dir = std.fs.cwd().openDir(dir_path, .{ .iterate = true }) catch return;
        defer dir.close();

        var walker = dir.walk(self.allocator) catch return;
        defer walker.deinit();

        // Collect files to delete (can't delete while iterating)
        var files_to_delete = std.ArrayList([]const u8).empty;
        defer {
            for (files_to_delete.items) |f| self.allocator.free(f);
            files_to_delete.deinit(self.allocator);
        }

        while (walker.next() catch null) |entry| {
            if (entry.kind == .file and std.mem.endsWith(u8, entry.basename, extension)) {
                const full_path = try std.fs.path.join(self.allocator, &.{ dir_path, entry.path });
                files_to_delete.append(self.allocator, full_path) catch continue;
            }
        }

        // Delete collected files
        for (files_to_delete.items) |file_path| {
            std.fs.cwd().deleteFile(file_path) catch {};
            if (self.options.verbose) {
                std.debug.print("  Deleted: {s}\n", .{file_path});
            }
        }
    }

    /// Build entire workspace
    pub fn buildWorkspace(self: *Self, ws: *const workspace.Workspace) !BuildResult {
        var result = BuildResult.init(self.allocator);
        errdefer result.deinit();

        // Full rebuild: clean all caches before building
        if (self.options.rebuild) {
            try self.cleanAllCaches(ws);
        }

        // Resolve dependencies for build order
        var dep_resolver = resolver.Resolver.init(self.allocator);
        defer dep_resolver.deinit();
        try dep_resolver.loadWorkspace(ws);

        var resolved = try dep_resolver.resolve();
        defer resolved.deinit();

        // Discover all components
        var discoverer = discovery.Discoverer.init(self.allocator);

        // Build in dependency order
        for (resolved.build_order.items) |project_name| {
            // Skip if specific project requested and this isn't it
            if (self.options.project) |target| {
                if (!std.mem.eql(u8, project_name, target)) continue;
            }

            // Find the project
            const project = ws.findProject(project_name) orelse continue;

            // Build the project (pass workspace root for output directory)
            const project_result = try self.buildProject(project, &discoverer, ws.root_path);
            result.total_projects += 1;

            if (project_result.success) {
                result.successful_projects += 1;
            } else {
                result.failed_projects += 1;
            }

            try result.projects.append(self.allocator, project_result);
        }

        return result;
    }

    /// Build a single project
    pub fn buildProject(self: *Self, project: *const workspace.Project, discoverer: *discovery.Discoverer, workspace_root: []const u8) !ProjectResult {
        var result = try ProjectResult.init(self.allocator, project.name, project.path);
        errdefer result.deinit(self.allocator);

        // Load schema if configured for this project
        try self.loadProjectSchema(project);

        // Determine build mode:
        // - Release mode (--release): always bundle
        // - Development mode: individual files for fast incremental builds
        const should_bundle = self.options.release or project.config.bundle;

        // Determine project type subdirectory
        const type_subdir = if (project.config.project_type == .library) "packages" else "apps";

        // Determine output directory (all under workspace root .cot-out/):
        // - Release: .cot-out/apps/<name>/ or .cot-out/packages/<name>/
        // - Development: .cot-out/.cache/<name>/
        const output_base = if (self.options.output_dir) |dir|
            try std.fs.path.join(self.allocator, &.{ dir, project.name })
        else if (should_bundle)
            try std.fs.path.join(self.allocator, &.{ workspace_root, ".cot-out", type_subdir, project.name })
        else
            try std.fs.path.join(self.allocator, &.{ workspace_root, ".cot-out", ".cache", project.name });
        defer self.allocator.free(output_base);

        // Clean if requested
        if (self.options.clean) {
            std.fs.cwd().deleteTree(output_base) catch {};
        }

        // Create output directory
        try std.fs.cwd().makePath(output_base);

        // Discover components
        var disc_result = try discoverer.discoverProject(project);
        defer disc_result.deinit();

        if (should_bundle) {
            // Build bundled executable/package
            return self.buildBundledProject(project, &disc_result, output_base, &result);
        } else {
            // Build individual .cotc files for development
            return self.buildIndividualFiles(project, &disc_result, output_base, &result);
        }
    }

    /// Build project with bundling - all modules in single executable
    fn buildBundledProject(
        self: *Self,
        project: *const workspace.Project,
        disc_result: *discovery.DiscoveryResult,
        output_base: []const u8,
        result: *ProjectResult,
    ) !ProjectResult {
        const build_result = try bundler.buildBundledProject(
            self.allocator,
            project,
            disc_result,
            output_base,
        );

        // Set result info
        result.bundled = true;
        result.total_files = build_result.manifest.modules.items.len;

        if (build_result.success) {
            result.compiled_files = result.total_files;
            result.success = true;

            // Store bundle path with appropriate extension
            // Apps: no extension (or .exe on Windows)
            // Libraries: .cotpkg
            const extension = if (project.config.project_type == .library)
                Extensions.library_package
            else
                Extensions.executable;

            const bundle_filename = try std.fmt.allocPrint(self.allocator, "{s}{s}", .{ project.name, extension });
            defer self.allocator.free(bundle_filename);
            result.bundle_path = try std.fs.path.join(self.allocator, &.{ output_base, bundle_filename });

            // Store manifest path if generated
            if (project.config.generate_manifest) {
                const manifest_filename = try std.fmt.allocPrint(self.allocator, "{s}.manifest.json", .{project.name});
                defer self.allocator.free(manifest_filename);
                result.manifest_path = try std.fs.path.join(self.allocator, &.{ output_base, manifest_filename });
            }
        } else {
            result.failed_files = build_result.errors.len;
            result.success = false;

            // Copy errors
            for (build_result.errors) |err| {
                try result.errors.append(self.allocator, try self.allocator.dupe(u8, err));
            }
        }

        // Clean up build result
        var manifest = build_result.manifest;
        manifest.deinit();
        for (build_result.errors) |e| self.allocator.free(e);
        self.allocator.free(build_result.errors);

        return result.*;
    }

    /// Build project with individual files - original behavior
    fn buildIndividualFiles(
        self: *Self,
        project: *const workspace.Project,
        disc_result: *discovery.DiscoveryResult,
        output_base: []const u8,
        result: *ProjectResult,
    ) !ProjectResult {
        // Compile main entry point
        if (disc_result.main_entry) |main_path| {
            const main_filename = try std.fmt.allocPrint(self.allocator, "{s}.cotc", .{project.name});
            defer self.allocator.free(main_filename);
            const main_output = try std.fs.path.join(self.allocator, &.{ output_base, main_filename });
            defer self.allocator.free(main_output);

            const file_result = try self.compileFile(main_path, main_output);
            try result.files.append(self.allocator, file_result);
            result.total_files += 1;

            if (file_result.success) {
                result.compiled_files += 1;
            } else {
                result.failed_files += 1;
                result.success = false;
            }
        }

        // Compile screens
        for (disc_result.screens.items) |screen| {
            const rel_output = try std.fmt.allocPrint(self.allocator, "screens/{s}.cotc", .{screen.name});
            defer self.allocator.free(rel_output);
            const output_path = try std.fs.path.join(self.allocator, &.{ output_base, rel_output });
            defer self.allocator.free(output_path);

            // Ensure output directory exists
            if (std.fs.path.dirname(output_path)) |dir| {
                std.fs.cwd().makePath(dir) catch {};
            }

            const file_result = try self.compileFile(screen.path, output_path);
            try result.files.append(self.allocator, file_result);
            result.total_files += 1;

            if (file_result.success) {
                result.compiled_files += 1;
            } else {
                result.failed_files += 1;
                result.success = false;
            }
        }

        // Compile reports
        for (disc_result.reports.items) |report| {
            const rel_output = try std.fmt.allocPrint(self.allocator, "reports/{s}.cotc", .{report.name});
            defer self.allocator.free(rel_output);
            const output_path = try std.fs.path.join(self.allocator, &.{ output_base, rel_output });
            defer self.allocator.free(output_path);

            if (std.fs.path.dirname(output_path)) |dir| {
                std.fs.cwd().makePath(dir) catch {};
            }

            const file_result = try self.compileFile(report.path, output_path);
            try result.files.append(self.allocator, file_result);
            result.total_files += 1;

            if (file_result.success) {
                result.compiled_files += 1;
            } else {
                result.failed_files += 1;
                result.success = false;
            }
        }

        // Compile jobs
        for (disc_result.jobs.items) |job| {
            const rel_output = try std.fmt.allocPrint(self.allocator, "jobs/{s}.cotc", .{job.name});
            defer self.allocator.free(rel_output);
            const output_path = try std.fs.path.join(self.allocator, &.{ output_base, rel_output });
            defer self.allocator.free(output_path);

            if (std.fs.path.dirname(output_path)) |dir| {
                std.fs.cwd().makePath(dir) catch {};
            }

            const file_result = try self.compileFile(job.path, output_path);
            try result.files.append(self.allocator, file_result);
            result.total_files += 1;

            if (file_result.success) {
                result.compiled_files += 1;
            } else {
                result.failed_files += 1;
                result.success = false;
            }
        }

        // Compile subroutines
        for (disc_result.subroutines.items) |sub| {
            const rel_output = try std.fmt.allocPrint(self.allocator, "subroutines/{s}.cotc", .{sub.name});
            defer self.allocator.free(rel_output);
            const output_path = try std.fs.path.join(self.allocator, &.{ output_base, rel_output });
            defer self.allocator.free(output_path);

            if (std.fs.path.dirname(output_path)) |dir| {
                std.fs.cwd().makePath(dir) catch {};
            }

            const file_result = try self.compileFile(sub.path, output_path);
            try result.files.append(self.allocator, file_result);
            result.total_files += 1;

            if (file_result.success) {
                result.compiled_files += 1;
            } else {
                result.failed_files += 1;
                result.success = false;
            }
        }

        // Compile library exports (for library packages)
        if (project.config.project_type == .library) {
            var exports_it = project.config.exports.iterator();
            while (exports_it.next()) |entry| {
                const export_name = entry.key_ptr.*;
                const export_path = entry.value_ptr.*;

                // Build full source path
                const source_path = try std.fs.path.join(self.allocator, &.{ project.path, export_path });
                defer self.allocator.free(source_path);

                // Create output filename from export name (strip "./" prefix if present)
                var clean_name = export_name;
                if (std.mem.startsWith(u8, clean_name, "./")) {
                    clean_name = clean_name[2..];
                }

                const rel_output = try std.fmt.allocPrint(self.allocator, "{s}.cotc", .{clean_name});
                defer self.allocator.free(rel_output);
                const output_path = try std.fs.path.join(self.allocator, &.{ output_base, rel_output });
                defer self.allocator.free(output_path);

                if (std.fs.path.dirname(output_path)) |dir| {
                    std.fs.cwd().makePath(dir) catch {};
                }

                const file_result = try self.compileFile(source_path, output_path);
                try result.files.append(self.allocator, file_result);
                result.total_files += 1;

                if (file_result.success) {
                    result.compiled_files += 1;
                } else {
                    result.failed_files += 1;
                    result.success = false;
                }
            }
        }

        return result.*;
    }

    /// Compile a single file
    fn compileFile(self: *Self, source_path: []const u8, output_path: []const u8) !FileResult {
        var result = FileResult{
            .source = try self.allocator.dupe(u8, source_path),
            .output = try self.allocator.dupe(u8, output_path),
            .success = false,
        };

        // For DBL files, spawn cot-dbl as external process
        if (std.mem.endsWith(u8, source_path, ".dbl")) {
            return self.compileDblFile(source_path, output_path, &result);
        }

        // Read source file
        const file = std.fs.cwd().openFile(source_path, .{}) catch |err| {
            result.error_message = try std.fmt.allocPrint(self.allocator, "Could not open file: {}", .{err});
            return result;
        };
        defer file.close();

        const source = file.readToEndAlloc(self.allocator, 1024 * 1024 * 10) catch |err| {
            result.error_message = try std.fmt.allocPrint(self.allocator, "Could not read file: {}", .{err});
            return result;
        };
        defer self.allocator.free(source);

        // Tokenize
        var lex = cot.lexer.Lexer.init(source);
        const tokens = lex.tokenize(self.allocator) catch |err| {
            result.error_message = try std.fmt.allocPrint(self.allocator, "Lexer error: {}", .{err});
            return result;
        };
        defer self.allocator.free(tokens);

        // Create StringInterner and NodeStore for new parser API
        var strings = cot.base.StringInterner.init(self.allocator);
        defer strings.deinit();
        var store = cot.ast.NodeStore.init(self.allocator, &strings);
        defer store.deinit();

        // Parse
        var parse = cot.parser.Parser.init(self.allocator, tokens, &store, &strings);
        defer parse.deinit();
        const top_level = parse.parse() catch |err| {
            result.error_message = try std.fmt.allocPrint(self.allocator, "Parser error: {}", .{err});
            return result;
        };

        // Lower AST to IR using NodeStore directly
        // Schema metadata is available via self.schema_metadata for table lookups
        _ = self.schema_metadata;

        const ir_module = cot.ir_lower.lower(self.allocator, &store, &strings, top_level, source_path) catch |err| {
            result.error_message = try std.fmt.allocPrint(self.allocator, "IR lowering error: {}", .{err});
            return result;
        };
        defer ir_module.deinit();

        // Type checking
        var collector = cot.compiler.DiagnosticCollector.init(self.allocator);
        defer collector.deinit();

        // Cache source for error messages
        collector.cacheSource(source_path, source) catch {};

        var type_checker = cot.compiler.TypeChecker.init(self.allocator, &collector, ir_module, source_path);
        type_checker.check();

        if (collector.hasErrors()) {
            // Format errors and return as error message
            cot.compiler.formatter.printToStderr(&collector, .{ .use_color = true });
            result.error_message = try std.fmt.allocPrint(self.allocator, "Type checking failed with {} error(s)", .{collector.error_count});
            return result;
        }

        // Emit bytecode
        var emitter = cot.ir_emit_bytecode.BytecodeEmitter.init(self.allocator);
        defer emitter.deinit();

        var mod = emitter.emit(ir_module) catch |err| {
            result.error_message = try std.fmt.allocPrint(self.allocator, "Bytecode error: {}", .{err});
            return result;
        };
        defer mod.deinit();

        // Create output directory if needed
        if (std.fs.path.dirname(output_path)) |dir| {
            std.fs.cwd().makePath(dir) catch {};
        }

        // Write bytecode file
        const out_file = std.fs.cwd().createFile(output_path, .{}) catch |err| {
            result.error_message = try std.fmt.allocPrint(self.allocator, "Could not create output: {}", .{err});
            return result;
        };
        defer out_file.close();

        var write_buffer: [4096]u8 = undefined;
        var buffered_writer = out_file.writer(&write_buffer);

        mod.serialize(&buffered_writer.interface) catch |err| {
            result.error_message = try std.fmt.allocPrint(self.allocator, "Write error: {}", .{err});
            return result;
        };
        buffered_writer.interface.flush() catch |err| {
            result.error_message = try std.fmt.allocPrint(self.allocator, "Flush error: {}", .{err});
            return result;
        };

        result.success = true;
        return result;
    }

    /// Compile a DBL file by spawning cot-dbl as an external process
    fn compileDblFile(self: *Self, source_path: []const u8, output_path: []const u8, result: *FileResult) !FileResult {
        // Create output directory if needed
        if (std.fs.path.dirname(output_path)) |dir| {
            std.fs.cwd().makePath(dir) catch {};
        }

        // Spawn cot-dbl to compile the file
        const argv = [_][]const u8{
            "cot-dbl",
            source_path,
            "--output",
            output_path,
        };

        var child = std.process.Child.init(&argv, self.allocator);
        child.stderr_behavior = .Ignore;
        child.stdout_behavior = .Ignore;

        _ = child.spawn() catch |err| {
            if (err == error.FileNotFound) {
                result.error_message = try std.fmt.allocPrint(
                    self.allocator,
                    "cot-dbl not found. DBL files require cot-dbl to compile.",
                    .{},
                );
            } else {
                result.error_message = try std.fmt.allocPrint(
                    self.allocator,
                    "Failed to spawn cot-dbl: {}",
                    .{err},
                );
            }
            return result.*;
        };

        const wait_result = child.wait() catch |err| {
            result.error_message = try std.fmt.allocPrint(
                self.allocator,
                "cot-dbl wait failed: {}",
                .{err},
            );
            return result.*;
        };

        const success = switch (wait_result) {
            .Exited => |code| code == 0,
            else => false,
        };

        if (!success) {
            result.error_message = try std.fmt.allocPrint(
                self.allocator,
                "cot-dbl compilation failed",
                .{},
            );
            return result.*;
        }

        result.success = true;
        return result.*;
    }
};

/// Print build result summary
pub fn printBuildSummary(result: *const BuildResult, writer: anytype) !void {
    try writer.writeAll("\n");
    try writer.writeAll("Build Summary\n");
    try writer.writeAll("─────────────\n");

    for (result.projects.items) |project| {
        const status = if (project.success) "✓" else "✗";

        if (project.bundled) {
            // Bundled build output
            try writer.print("{s} {s}: bundled {d} modules\n", .{
                status,
                project.name,
                project.total_files,
            });

            if (project.bundle_path) |path| {
                try writer.print("    → {s}\n", .{path});
            }
            if (project.manifest_path) |path| {
                try writer.print("    → {s}\n", .{path});
            }

            // Show errors
            for (project.errors.items) |err| {
                try writer.print("    Error: {s}\n", .{err});
            }
        } else {
            // Individual files output
            try writer.print("{s} {s}: {d}/{d} files compiled\n", .{
                status,
                project.name,
                project.compiled_files,
                project.total_files,
            });

            // Show errors for failed files
            for (project.files.items) |file| {
                if (!file.success) {
                    if (file.error_message) |msg| {
                        try writer.print("    Error in {s}: {s}\n", .{
                            std.fs.path.basename(file.source),
                            msg,
                        });
                    }
                }
            }
        }
    }

    try writer.writeAll("\n");
    if (result.isSuccess()) {
        try writer.print("Build succeeded: {d} projects\n", .{result.successful_projects});
    } else {
        try writer.print("Build failed: {d} succeeded, {d} failed\n", .{
            result.successful_projects,
            result.failed_projects,
        });
    }
}
