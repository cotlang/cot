//! Bytecode Bundler
//!
//! Bundles multiple .cot files into a single executable or library package.
//! Links subroutines, includes dependencies, and generates manifest.
//!
//! Output formats:
//! - Apps: <name> (no extension on Unix, .exe on Windows)
//! - Libraries: <name>.cotpkg

const std = @import("std");
const builtin = @import("builtin");
const Allocator = std.mem.Allocator;
const cot = @import("cot");
const discovery = @import("../discovery.zig");
const workspace = @import("../workspace.zig");
const config = @import("../config.zig");

/// File extensions for bundled output
pub const Extensions = struct {
    /// Library package
    pub const library_package = ".cotpkg";
    /// Executable extension (platform-dependent)
    pub const executable = if (builtin.os.tag == .windows) ".exe" else "";
};

/// Bundle manifest - describes the contents of a bundled executable
pub const Manifest = struct {
    name: []const u8,
    version: []const u8,
    main_entry: []const u8,
    modules: std.ArrayList(ModuleInfo),
    dependencies: std.ArrayList([]const u8),
    build_time: i64,
    allocator: Allocator,

    pub const ModuleInfo = struct {
        name: []const u8,
        source_path: []const u8,
        module_type: ModuleType,
        byte_offset: usize,
        byte_size: usize,
    };

    pub const ModuleType = enum {
        main,
        subroutine,
        screen,
        report,
        job,
        package,
    };

    pub fn init(allocator: Allocator, name: []const u8, version: []const u8) !Manifest {
        return .{
            .name = try allocator.dupe(u8, name),
            .version = try allocator.dupe(u8, version),
            .main_entry = "",
            .modules = .{},
            .dependencies = .{},
            .build_time = std.time.timestamp(),
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Manifest) void {
        self.allocator.free(self.name);
        self.allocator.free(self.version);
        if (self.main_entry.len > 0) self.allocator.free(self.main_entry);
        for (self.modules.items) |m| {
            self.allocator.free(m.name);
            self.allocator.free(m.source_path);
        }
        self.modules.deinit(self.allocator);
        for (self.dependencies.items) |d| self.allocator.free(d);
        self.dependencies.deinit(self.allocator);
    }

    /// Write manifest to JSON file
    pub fn writeToFile(self: *const Manifest, path: []const u8) !void {
        var file = try std.fs.cwd().createFile(path, .{});
        defer file.close();

        var write_buffer: [4096]u8 = undefined;
        var buffered = file.writer(&write_buffer);
        const writer = &buffered.interface;

        try writer.writeAll("{\n");
        try writer.print("  \"name\": \"{s}\",\n", .{self.name});
        try writer.print("  \"version\": \"{s}\",\n", .{self.version});
        try writer.print("  \"mainEntry\": \"{s}\",\n", .{self.main_entry});
        try writer.print("  \"buildTime\": {d},\n", .{self.build_time});

        try writer.writeAll("  \"modules\": [\n");
        for (self.modules.items, 0..) |m, i| {
            try writer.writeAll("    {\n");
            try writer.print("      \"name\": \"{s}\",\n", .{m.name});
            try writer.print("      \"type\": \"{s}\",\n", .{@tagName(m.module_type)});
            try writer.print("      \"source\": \"{s}\",\n", .{m.source_path});
            try writer.print("      \"offset\": {d},\n", .{m.byte_offset});
            try writer.print("      \"size\": {d}\n", .{m.byte_size});
            if (i < self.modules.items.len - 1) {
                try writer.writeAll("    },\n");
            } else {
                try writer.writeAll("    }\n");
            }
        }
        try writer.writeAll("  ],\n");

        try writer.writeAll("  \"dependencies\": [");
        for (self.dependencies.items, 0..) |d, i| {
            try writer.print("\"{s}\"", .{d});
            if (i < self.dependencies.items.len - 1) {
                try writer.writeAll(", ");
            }
        }
        try writer.writeAll("]\n");
        try writer.writeAll("}\n");

        try buffered.interface.flush();
    }
};

/// Bundled module bytecode
const BundledModule = struct {
    name: []const u8,
    module_type: Manifest.ModuleType,
    source_path: []const u8,
    bytecode: []const u8,
};

/// Bundler for creating single-file executables
pub const Bundler = struct {
    allocator: Allocator,
    modules: std.ArrayList(BundledModule),
    errors: std.ArrayList([]const u8),
    cache_dir: ?[]const u8,

    const Self = @This();

    pub fn init(allocator: Allocator, cache_dir: ?[]const u8) Self {
        return .{
            .allocator = allocator,
            .modules = .{},
            .errors = .{},
            .cache_dir = cache_dir,
        };
    }

    pub fn deinit(self: *Self) void {
        for (self.modules.items) |m| {
            self.allocator.free(m.name);
            self.allocator.free(m.source_path);
            self.allocator.free(m.bytecode);
        }
        self.modules.deinit(self.allocator);
        for (self.errors.items) |e| self.allocator.free(e);
        self.errors.deinit(self.allocator);
    }

    /// Add a source file to the bundle
    pub fn addSource(self: *Self, name: []const u8, source_path: []const u8, module_type: Manifest.ModuleType) !bool {
        // Read source file
        const file = std.fs.cwd().openFile(source_path, .{}) catch |err| {
            try self.errors.append(self.allocator, try std.fmt.allocPrint(self.allocator, "{s}: Could not open file: {}", .{ source_path, err }));
            return false;
        };
        defer file.close();

        const source = file.readToEndAlloc(self.allocator, 1024 * 1024 * 10) catch |err| {
            try self.errors.append(self.allocator, try std.fmt.allocPrint(self.allocator, "{s}: Could not read file: {}", .{ source_path, err }));
            return false;
        };
        defer self.allocator.free(source);

        // Compile to bytecode
        const bytecode = self.compileSource(source, source_path) catch |err| {
            try self.errors.append(self.allocator, try std.fmt.allocPrint(self.allocator, "{s}: Compilation error: {}", .{ source_path, err }));
            return false;
        };

        try self.modules.append(self.allocator, .{
            .name = try self.allocator.dupe(u8, name),
            .module_type = module_type,
            .source_path = try self.allocator.dupe(u8, source_path),
            .bytecode = bytecode,
        });

        return true;
    }

    /// Add a pre-compiled bytecode file to the bundle
    /// This is faster and more reliable than re-compiling from source
    pub fn addBytecode(self: *Self, name: []const u8, bytecode_path: []const u8, source_path: []const u8, module_type: Manifest.ModuleType) !bool {
        // Read bytecode file
        const file = std.fs.cwd().openFile(bytecode_path, .{}) catch |err| {
            try self.errors.append(self.allocator, try std.fmt.allocPrint(self.allocator, "{s}: Could not open bytecode file: {}", .{ bytecode_path, err }));
            return false;
        };
        defer file.close();

        const bytecode = file.readToEndAlloc(self.allocator, 1024 * 1024 * 10) catch |err| {
            try self.errors.append(self.allocator, try std.fmt.allocPrint(self.allocator, "{s}: Could not read bytecode: {}", .{ bytecode_path, err }));
            return false;
        };

        // Validate bytecode header
        if (bytecode.len < 4 or !std.mem.eql(u8, bytecode[0..4], "CBO1")) {
            self.allocator.free(bytecode);
            try self.errors.append(self.allocator, try std.fmt.allocPrint(self.allocator, "{s}: Invalid bytecode format (missing CBO1 header)", .{bytecode_path}));
            return false;
        }

        try self.modules.append(self.allocator, .{
            .name = try self.allocator.dupe(u8, name),
            .module_type = module_type,
            .source_path = try self.allocator.dupe(u8, source_path),
            .bytecode = bytecode,
        });

        return true;
    }

    /// Compile source to bytecode bytes
    fn compileSource(self: *Self, source: []const u8, source_path: []const u8) ![]const u8 {
        // Check if this is a DBL file - spawn cot-dbl as external process
        if (std.mem.endsWith(u8, source_path, ".dbl")) {
            return self.compileDblFile(source_path);
        }

        // Tokenize
        var lex = cot.lexer.Lexer.init(source);
        const tokens = try lex.tokenize(self.allocator);
        defer self.allocator.free(tokens);

        // Create StringInterner and NodeStore for new parser API
        var strings = cot.base.StringInterner.init(self.allocator);
        defer strings.deinit();
        var store = cot.ast.NodeStore.init(self.allocator, &strings);
        defer store.deinit();

        // Parse
        var parse = cot.parser.Parser.init(self.allocator, tokens, &store, &strings);
        defer parse.deinit();
        const top_level = try parse.parse();

        // Lower to IR using detailed error reporting
        const lower_result = cot.ir_lower.lowerWithDetails(self.allocator, &store, &strings, top_level, source_path, .{});
        const ir_module = switch (lower_result) {
            .ok => |module| module,
            .err => |e| {
                if (e.detail) |detail| {
                    std.debug.print("{d}:{d}: error: {s}\n", .{ detail.line, detail.column, detail.message });
                    if (detail.context.len > 0) {
                        std.debug.print("  while {s}\n", .{detail.context});
                    }
                } else {
                    std.debug.print("IR lowering error: {}\n", .{e.kind});
                }
                return e.kind;
            },
        };
        defer ir_module.deinit();

        // Type checking
        var collector = cot.compiler.DiagnosticCollector.init(self.allocator);
        defer collector.deinit();
        collector.cacheSource(source_path, source) catch {};

        var type_checker = cot.compiler.TypeChecker.init(self.allocator, &collector, ir_module, source_path);
        type_checker.check();

        if (collector.hasErrors()) {
            // Print errors and return error
            cot.compiler.formatter.printToStderr(&collector, .{ .use_color = true });
            return error.TypeCheckFailed;
        }

        // Run optimization passes
        _ = cot.ir_optimize.optimize(ir_module, .{});

        // Emit bytecode
        var emitter = cot.ir_emit_bytecode.BytecodeEmitter.init(self.allocator);
        defer emitter.deinit();
        var mod = try emitter.emit(ir_module);
        defer mod.deinit();

        // Serialize to bytes
        var bytecode_buf: std.ArrayList(u8) = .{};
        errdefer bytecode_buf.deinit(self.allocator);

        // Use a custom writer that appends to ArrayList
        var array_writer = ArrayListWriter{ .list = &bytecode_buf, .allocator = self.allocator };
        try mod.serialize(array_writer.writer());

        return bytecode_buf.toOwnedSlice(self.allocator);
    }

    /// Compile a DBL file by spawning cot-dbl as an external process
    fn compileDblFile(self: *Self, source_path: []const u8) ![]const u8 {
        // Create output path for bytecode - use cache_dir if available, otherwise fallback to source location
        const output_path = if (self.cache_dir) |cache_dir| blk: {
            // Use cache directory: cache_dir/<basename>.cbo
            const basename = std.fs.path.basename(source_path);
            break :blk try std.fmt.allocPrint(self.allocator, "{s}/{s}.cbo", .{ cache_dir, basename });
        } else try std.fmt.allocPrint(self.allocator, "{s}.cbo", .{source_path});
        defer self.allocator.free(output_path);

        // Ensure cache directory exists
        if (self.cache_dir) |cache_dir| {
            std.fs.cwd().makePath(cache_dir) catch {};
        }

        // Spawn cot-dbl to compile the file
        // cot-dbl <input.dbl> compile --output <output.cbo>
        const argv = [_][]const u8{
            "cot-dbl",
            source_path,
            "compile",
            "--output",
            output_path,
        };

        var child = std.process.Child.init(&argv, self.allocator);
        // Capture stderr to get actual error messages from cot-dbl
        child.stderr_behavior = .Pipe;
        child.stdout_behavior = .Ignore;

        _ = child.spawn() catch |err| {
            // If cot-dbl is not found, provide a helpful message
            if (err == error.FileNotFound) {
                try self.errors.append(self.allocator, try std.fmt.allocPrint(
                    self.allocator,
                    "{s}: cot-dbl not found. DBL files require cot-dbl to compile.",
                    .{source_path},
                ));
            }
            return error.CompilationFailed;
        };

        // Read stderr output before waiting (to avoid pipe buffer deadlock)
        var stderr_output: []const u8 = "";
        if (child.stderr) |stderr_pipe| {
            stderr_output = stderr_pipe.readToEndAlloc(self.allocator, 64 * 1024) catch "";
        }

        const result = child.wait() catch return error.CompilationFailed;

        // Check if process exited successfully (exit code 0)
        const success = switch (result) {
            .Exited => |code| code == 0,
            else => false,
        };

        if (!success) {
            // Include actual error output from cot-dbl
            if (stderr_output.len > 0) {
                // Strip ANSI codes and trim for cleaner output
                const error_msg = try std.fmt.allocPrint(
                    self.allocator,
                    "{s}:\n{s}",
                    .{ source_path, std.mem.trim(u8, stderr_output, " \t\n\r") },
                );
                try self.errors.append(self.allocator, error_msg);
                self.allocator.free(stderr_output);
            } else {
                try self.errors.append(self.allocator, try std.fmt.allocPrint(
                    self.allocator,
                    "{s}: cot-dbl compilation failed",
                    .{source_path},
                ));
            }
            return error.CompilationFailed;
        }
        if (stderr_output.len > 0) self.allocator.free(stderr_output);

        // Read the compiled bytecode
        const file = std.fs.cwd().openFile(output_path, .{}) catch return error.CompilationFailed;
        defer file.close();
        const bytecode = file.readToEndAlloc(self.allocator, 1024 * 1024 * 10) catch return error.CompilationFailed;

        return bytecode;
    }

    /// Bundle all modules into a single output file
    pub fn bundle(self: *Self, output_path: []const u8, project_name: []const u8, version: []const u8) !Manifest {
        var manifest = try Manifest.init(self.allocator, project_name, version);
        errdefer manifest.deinit();

        // Create output file
        var out_file = try std.fs.cwd().createFile(output_path, .{});
        defer out_file.close();

        // Write bundle header
        // Magic: "CBUNDLE\0"
        try out_file.writeAll("CBUNDLE\x00");
        // Version: 1
        try out_file.writeAll(&[_]u8{0x01});
        // Module count
        var count_buf: [4]u8 = undefined;
        std.mem.writeInt(u32, &count_buf, @intCast(self.modules.items.len), .little);
        try out_file.writeAll(&count_buf);

        // Calculate offsets (header + module table)
        const header_size: usize = 8 + 1 + 4; // magic + version + count
        const module_table_entry_size: usize = 4 + 4 + 1; // offset + size + type
        const module_table_size = module_table_entry_size * self.modules.items.len;
        var current_offset = header_size + module_table_size;

        // Write module table
        for (self.modules.items) |mod| {
            // Offset (4 bytes)
            var offset_buf: [4]u8 = undefined;
            std.mem.writeInt(u32, &offset_buf, @intCast(current_offset), .little);
            try out_file.writeAll(&offset_buf);

            // Size (4 bytes)
            var size_buf: [4]u8 = undefined;
            std.mem.writeInt(u32, &size_buf, @intCast(mod.bytecode.len), .little);
            try out_file.writeAll(&size_buf);

            // Type (1 byte)
            try out_file.writeAll(&[_]u8{@intFromEnum(mod.module_type)});

            // Add to manifest
            try manifest.modules.append(self.allocator, .{
                .name = try self.allocator.dupe(u8, mod.name),
                .source_path = try self.allocator.dupe(u8, mod.source_path),
                .module_type = mod.module_type,
                .byte_offset = current_offset,
                .byte_size = mod.bytecode.len,
            });

            // Track main entry
            if (mod.module_type == .main) {
                manifest.main_entry = try self.allocator.dupe(u8, mod.name);
            }

            current_offset += mod.bytecode.len;
        }

        // Write module bytecode
        for (self.modules.items) |mod| {
            try out_file.writeAll(mod.bytecode);
        }

        return manifest;
    }

    /// Get compilation errors
    pub fn getErrors(self: *const Self) []const []const u8 {
        return self.errors.items;
    }

    pub fn hasErrors(self: *const Self) bool {
        return self.errors.items.len > 0;
    }
};

/// Simple ArrayList-based writer for serialization
const ArrayListWriter = struct {
    list: *std.ArrayList(u8),
    allocator: Allocator,

    const Writer = std.io.GenericWriter(*ArrayListWriter, error{OutOfMemory}, write);

    fn write(self: *ArrayListWriter, bytes: []const u8) error{OutOfMemory}!usize {
        self.list.appendSlice(self.allocator, bytes) catch return error.OutOfMemory;
        return bytes.len;
    }

    fn writer(self: *ArrayListWriter) Writer {
        return .{ .context = self };
    }
};

/// Build a project into a bundled executable
pub fn buildBundledProject(
    allocator: Allocator,
    project: *const workspace.Project,
    disc_result: *const discovery.DiscoveryResult,
    output_dir: []const u8,
) !struct { manifest: Manifest, success: bool, errors: []const []const u8 } {
    // Use output_dir as cache directory for intermediate compilation artifacts
    var bund = Bundler.init(allocator, output_dir);
    defer bund.deinit();

    // Helper to add a module, preferring pre-compiled bytecode from cache if it's newer than source
    const addModule = struct {
        fn add(b: *Bundler, alloc: Allocator, proj_path: []const u8, name: []const u8, source_path: []const u8, module_type: Manifest.ModuleType) !bool {
            // Get source file modification time
            const source_mtime = getFileMtime(source_path) orelse {
                // Source doesn't exist or can't be read - try to compile anyway
                return b.addSource(name, source_path, module_type);
            };

            // Check for pre-compiled bytecode in .cot-cache/
            const cache_dir = try std.fs.path.join(alloc, &.{ proj_path, ".cot-cache" });
            defer alloc.free(cache_dir);
            const cache_filename = try std.fmt.allocPrint(alloc, "{s}.cotc", .{name});
            defer alloc.free(cache_filename);
            const cache_path = try std.fs.path.join(alloc, &.{ cache_dir, cache_filename });
            defer alloc.free(cache_path);

            // Try to use cached bytecode if it exists AND is newer than source
            if (getFileMtime(cache_path)) |cache_mtime| {
                if (cache_mtime >= source_mtime) {
                    return b.addBytecode(name, cache_path, source_path, module_type);
                }
            }

            // Check for .cbo file next to source (legacy format: <source>.cbo)
            const cbo_path = try std.fmt.allocPrint(alloc, "{s}.cbo", .{source_path});
            defer alloc.free(cbo_path);
            if (getFileMtime(cbo_path)) |cbo_mtime| {
                if (cbo_mtime >= source_mtime) {
                    return b.addBytecode(name, cbo_path, source_path, module_type);
                }
            }

            // Cache is stale or doesn't exist - compile from source
            return b.addSource(name, source_path, module_type);
        }

        /// Get file modification time, returns null if file doesn't exist
        fn getFileMtime(path: []const u8) ?i128 {
            const file = std.fs.cwd().openFile(path, .{}) catch return null;
            defer file.close();
            const stat = file.stat() catch return null;
            return stat.mtime;
        }
    }.add;

    // Add main entry point
    if (disc_result.main_entry) |main_path| {
        _ = try addModule(&bund, allocator, project.path, project.name, main_path, .main);
    }

    // Add all subroutines
    for (disc_result.subroutines.items) |sub| {
        _ = try addModule(&bund, allocator, project.path, sub.name, sub.path, .subroutine);
    }

    // Add all screens
    for (disc_result.screens.items) |screen| {
        _ = try addModule(&bund, allocator, project.path, screen.name, screen.path, .screen);
    }

    // Add all reports
    for (disc_result.reports.items) |report| {
        _ = try addModule(&bund, allocator, project.path, report.name, report.path, .report);
    }

    // Add all jobs
    for (disc_result.jobs.items) |job| {
        _ = try addModule(&bund, allocator, project.path, job.name, job.path, .job);
    }

    // Create output directory
    try std.fs.cwd().makePath(output_dir);

    // Bundle to output file with appropriate extension
    // Apps: no extension (or .exe on Windows)
    // Libraries: .cotpkg
    const extension = if (project.config.project_type == .library)
        Extensions.library_package
    else
        Extensions.executable;

    const output_filename = try std.fmt.allocPrint(allocator, "{s}{s}", .{ project.name, extension });
    defer allocator.free(output_filename);
    const output_path = try std.fs.path.join(allocator, &.{ output_dir, output_filename });
    defer allocator.free(output_path);

    var manifest = try bund.bundle(output_path, project.name, project.config.version);

    // Write manifest
    const manifest_filename = try std.fmt.allocPrint(allocator, "{s}.manifest.json", .{project.name});
    defer allocator.free(manifest_filename);
    const manifest_path = try std.fs.path.join(allocator, &.{ output_dir, manifest_filename });
    defer allocator.free(manifest_path);
    try manifest.writeToFile(manifest_path);

    // Copy errors
    var errors: std.ArrayList([]const u8) = .{};
    for (bund.errors.items) |e| {
        try errors.append(allocator, try allocator.dupe(u8, e));
    }

    return .{
        .manifest = manifest,
        .success = !bund.hasErrors(),
        .errors = try errors.toOwnedSlice(allocator),
    };
}
