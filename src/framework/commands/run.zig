//! cot run command
//!
//! Runs a compiled project or specific component.

const std = @import("std");
const config = @import("../config.zig");
const workspace = @import("../workspace.zig");
const Allocator = std.mem.Allocator;

// Import runtime
const cot = @import("cot");
const debug = cot.debug;
const build_options = @import("build_options");

pub const RunCommandOptions = struct {
    target: ?[]const u8 = null,
    filter: ?[]const u8 = null, // --filter=<project> like Turborepo
    show_help: bool = false,
    dev_mode: bool = false, // --dev flag for split-pane dev tools

    /// Get effective target (filter takes precedence)
    pub fn getTarget(self: RunCommandOptions) ?[]const u8 {
        return self.filter orelse self.target;
    }
};

/// Target specifier parsed from command line
pub const RunTarget = struct {
    project: []const u8,
    component_type: ?ComponentType = null,
    component_name: ?[]const u8 = null,
};

pub const ComponentType = enum {
    main,
    screen,
    report,
    job,
    api,
    subroutine,
};

/// Run the run command
pub fn run(allocator: Allocator, options: RunCommandOptions) !void {
    debug.print(.general, ">>> run_cmd.run called", .{});
    var stdout_buffer: [4096]u8 = undefined;
    var stdout_file = std.fs.File.stdout();
    var stdout_writer = stdout_file.writer(&stdout_buffer);
    const stdout = &stdout_writer.interface;

    if (options.show_help) {
        try printHelp();
        return;
    }

    // Find workspace - use target path if specified to find workspace more accurately
    const cwd = try std.fs.cwd().realpathAlloc(allocator, ".");
    defer allocator.free(cwd);

    var config_loader = config.ConfigLoader.init(allocator);

    // If target is specified, try to find workspace based on target path
    var workspace_root: []const u8 = undefined;
    const target_spec = options.getTarget();

    if (target_spec) |target| {
        // Parse target to get project path
        debug.print(.general, "Target specified: {s}", .{target});
        const parsed = try parseTarget(target);

        // Try to resolve target path - handle both absolute and relative paths
        const target_path = if (std.fs.path.isAbsolute(parsed.project))
            try allocator.dupe(u8, parsed.project)
        else
            std.fs.path.join(allocator, &.{ cwd, parsed.project }) catch |err| {
                try stdout.print("Error: Could not resolve path: {}\n", .{err});
                try stdout.flush();
                return error.InvalidPath;
            };
        defer allocator.free(target_path);
        debug.print(.general, "Resolved target path: {s}", .{target_path});

        // Check if this path exists
        if (std.fs.cwd().access(target_path, .{})) |_| {
            // Path exists - find workspace from this location
            debug.print(.general, "Path exists, finding workspace...", .{});
            const abs_target = std.fs.cwd().realpathAlloc(allocator, target_path) catch target_path;
            defer if (abs_target.ptr != target_path.ptr) allocator.free(abs_target);

            workspace_root = try config_loader.findWorkspaceRoot(abs_target) orelse {
                try stdout.print("Error: Not inside a Cot workspace.\n", .{});
                try stdout.print("Run 'cot init' first to create a workspace.\n", .{});
                try stdout.flush();
                return error.NoWorkspace;
            };
            debug.print(.general, "Workspace root found: {s}", .{workspace_root});
        } else |_| {
            // Path doesn't exist, try direct workspace search from cwd
            debug.print(.general, "Path does not exist, searching from cwd", .{});
            workspace_root = try config_loader.findWorkspaceRoot(cwd) orelse {
                try stdout.print("Error: Not inside a Cot workspace.\n", .{});
                try stdout.print("Run 'cot init' first to create a workspace.\n", .{});
                try stdout.flush();
                return error.NoWorkspace;
            };
        }
    } else {
        // No target specified, use cwd
        workspace_root = try config_loader.findWorkspaceRoot(cwd) orelse {
            try stdout.print("Error: Not inside a Cot workspace.\n", .{});
            try stdout.print("Run 'cot init' first to create a workspace.\n", .{});
            try stdout.flush();
            return error.NoWorkspace;
        };
    }
    defer allocator.free(workspace_root);

    // Load workspace
    debug.print(.general, "Loading workspace...", .{});
    var ws_loader = workspace.WorkspaceLoader.init(allocator);
    var ws = try ws_loader.load(workspace_root);
    defer ws.deinit();
    debug.print(.general, "Workspace loaded: {d} apps, {d} packages", .{ ws.apps.items.len, ws.packages.items.len });

    // Determine what to run (filter takes precedence)
    // target_spec was already obtained above
    debug.print(.general, "Determining target to run...", .{});
    if (target_spec == null) {
        // No target specified, try to run first/default app
        debug.print(.general, "No target specified, using first app", .{});
        if (ws.apps.items.len == 0) {
            try stdout.print("Error: No apps found in workspace.\n", .{});
            try stdout.print("Create an app with 'cot new <name>'\n", .{});
            try stdout.flush();
            return error.NoApps;
        }

        // Run the first app's main
        const first_app = &ws.apps.items[0];
        try runProjectMain(allocator, first_app, &ws, stdout, options.dev_mode);
        return;
    }

    // Parse the target specifier
    debug.print(.general, "Parsing target: {s}", .{target_spec.?});
    const target = try parseTarget(target_spec.?);

    // Find the project
    debug.print(.general, "Looking for project: {s}", .{target.project});
    const project = ws.findProject(target.project) orelse {
        debug.print(.general, "Project not found!", .{});
        try stdout.print("Error: Project '{s}' not found.\n", .{target.project});
        try stdout.flush();
        return error.ProjectNotFound;
    };

    // Run based on component type
    debug.print(.general, "Found project: {s}", .{project.name});
    if (target.component_type) |comp_type| {
        debug.print(.general, "Running component: {s}", .{@tagName(comp_type)});
        try runComponent(allocator, project, comp_type, target.component_name, stdout);
    } else {
        debug.print(.general, "Running project main...", .{});
        try runProjectMain(allocator, project, &ws, stdout, options.dev_mode);
    }
    debug.print(.general, "Run completed", .{});
}

/// Parse a target specifier like "inventory" or "inventory:job:nightly_sync"
fn parseTarget(spec: []const u8) !RunTarget {
    var target = RunTarget{ .project = spec };

    // Check for component specifier (project:type:name)
    var parts = std.mem.splitScalar(u8, spec, ':');
    const project_part = parts.next() orelse return target;
    target.project = project_part;

    if (parts.next()) |type_part| {
        if (std.mem.eql(u8, type_part, "screen")) {
            target.component_type = .screen;
        } else if (std.mem.eql(u8, type_part, "report")) {
            target.component_type = .report;
        } else if (std.mem.eql(u8, type_part, "job")) {
            target.component_type = .job;
        } else if (std.mem.eql(u8, type_part, "api")) {
            target.component_type = .api;
        } else if (std.mem.eql(u8, type_part, "subroutine") or std.mem.eql(u8, type_part, "sub")) {
            target.component_type = .subroutine;
        } else if (std.mem.eql(u8, type_part, "main")) {
            target.component_type = .main;
        }

        if (parts.next()) |name_part| {
            target.component_name = name_part;
        }
    }

    return target;
}

/// Run the main entry point of a project with its dependencies
fn runProjectMain(allocator: Allocator, project: *const workspace.Project, ws: *const workspace.Workspace, writer: anytype, dev_mode: bool) !void {
    debug.print(.general, "runProjectMain for: {s}", .{project.name});
    // Find compiled main bytecode - try workspace .cot-out/ directory
    // Dev mode: .cot-out/.cache/<name>/<name>.cotc
    // Release mode: .cot-out/apps/<name>/<name> (no extension)
    var bytecode_path: []const u8 = undefined;
    var found = false;

    // Determine project type subdirectory
    const type_subdir = if (project.config.project_type == .library) "packages" else "apps";

    // Try dev cache first (.cot-out/.cache/<name>/<name>.cotc)
    const dev_bytecode = try std.fs.path.join(allocator, &.{
        ws.root_path,
        ".cot-out",
        ".cache",
        project.name,
        project.name,
    });
    const dev_bytecode_ext = try std.fmt.allocPrint(allocator, "{s}.cotc", .{dev_bytecode});
    allocator.free(dev_bytecode);

    if (std.fs.cwd().access(dev_bytecode_ext, .{})) |_| {
        bytecode_path = dev_bytecode_ext;
        found = true;
    } else |_| {
        allocator.free(dev_bytecode_ext);
    }

    // Try release output (.cot-out/apps/<name>/<name> - no extension)
    if (!found) {
        const release_path = try std.fs.path.join(allocator, &.{
            ws.root_path,
            ".cot-out",
            type_subdir,
            project.name,
            project.name,
        });
        if (std.fs.cwd().access(release_path, .{})) |_| {
            bytecode_path = release_path;
            found = true;
        } else |_| {
            allocator.free(release_path);
        }
    }

    // Try legacy locations for backwards compatibility
    if (!found) {
        // Legacy: {project.path}/bin/{name}
        const legacy_bundle = try std.fs.path.join(allocator, &.{
            project.path,
            "bin",
            project.name,
        });
        if (std.fs.cwd().access(legacy_bundle, .{})) |_| {
            bytecode_path = legacy_bundle;
            found = true;
        } else |_| {
            allocator.free(legacy_bundle);
        }
    }

    if (!found) {
        // Legacy: {project.path}/.cot-cache/{name}.cotc
        const legacy_cache = try std.fs.path.join(allocator, &.{
            project.path,
            ".cot-cache",
            project.name,
        });
        const legacy_cache_ext = try std.fmt.allocPrint(allocator, "{s}.cotc", .{legacy_cache});
        allocator.free(legacy_cache);

        if (std.fs.cwd().access(legacy_cache_ext, .{})) |_| {
            bytecode_path = legacy_cache_ext;
            found = true;
        } else |_| {
            allocator.free(legacy_cache_ext);
        }
    }

    if (!found) {
        try writer.print("Error: Compiled bytecode not found.\n", .{});
        try writer.print("Run 'cot build' first to compile the project.\n", .{});
        try writer.flush();
        return error.NotCompiled;
    }
    defer allocator.free(bytecode_path);

    debug.print(.general, "runProjectMain called for: {s}", .{project.name});

    // Load dependency packages
    debug.print(.general, "Project config has {d} dependencies", .{project.config.dependencies.count()});
    var dep_count: usize = 0;
    var dep_it = project.config.dependencies.iterator();
    while (dep_it.next()) |entry| {
        const dep_name = entry.key_ptr.*;
        const dep_version = entry.value_ptr.*;

        // Check if it's a workspace dependency
        if (std.mem.startsWith(u8, dep_version, "workspace:")) {
            // Find the package in workspace
            if (ws.findProject(dep_name)) |dep_project| {
                try writer.print("  Loading: {s}\n", .{dep_name});
                dep_count += 1;
                _ = dep_project; // Will be used when loading modules
            }
        }
    }

    if (dep_count > 0) {
        try writer.print("  Loaded {d} dependencies\n", .{dep_count});
    }
    try writer.print("─────────────────────\n", .{});
    try writer.flush();

    try runBytecodeFileWithDeps(allocator, bytecode_path, ws, project, writer, dev_mode);
}

/// Run a specific component
fn runComponent(
    allocator: Allocator,
    project: *const workspace.Project,
    comp_type: ComponentType,
    comp_name: ?[]const u8,
    writer: anytype,
) !void {
    const subdir = switch (comp_type) {
        .main => "",
        .screen => "screens",
        .report => "reports",
        .job => "jobs",
        .api => "api",
        .subroutine => "subroutines",
    };

    const name = comp_name orelse {
        try writer.print("Error: Component name required for {s}\n", .{@tagName(comp_type)});
        try writer.flush();
        return error.MissingComponentName;
    };

    // Build bytecode path
    const bytecode_path = if (subdir.len > 0)
        try std.fmt.allocPrint(allocator, "{s}/{s}/{s}/{s}.cbo", .{
            project.path,
            project.config.output_dir,
            subdir,
            name,
        })
    else
        try std.fmt.allocPrint(allocator, "{s}/{s}/{s}.cbo", .{
            project.path,
            project.config.output_dir,
            name,
        });
    defer allocator.free(bytecode_path);

    // Check if bytecode exists
    std.fs.cwd().access(bytecode_path, .{}) catch {
        try writer.print("Error: Compiled bytecode not found: {s}\n", .{bytecode_path});
        try writer.print("Run 'cot build' first to compile the project.\n", .{});
        try writer.flush();
        return error.NotCompiled;
    };

    try writer.print("Running: {s}:{s}:{s}\n", .{ project.name, @tagName(comp_type), name });
    try writer.print("─────────────────────\n", .{});

    try runBytecodeFile(allocator, bytecode_path, writer);
}

/// Run a bytecode file (handles both regular and bundled formats)
fn runBytecodeFile(allocator: Allocator, path: []const u8, writer: anytype) !void {
    _ = writer;

    // Read bytecode file
    const file = std.fs.cwd().openFile(path, .{}) catch |err| {
        std.debug.print("Error: Could not open bytecode file: {}\n", .{err});
        return error.FileNotFound;
    };
    defer file.close();

    const bytes = try file.readToEndAlloc(allocator, 1024 * 1024 * 100);
    defer allocator.free(bytes);

    // Check for bundle format (CBUNDLE magic header)
    const is_bundle = bytes.len >= 8 and std.mem.eql(u8, bytes[0..8], "CBUNDLE\x00");

    if (is_bundle) {
        try runBundledFile(allocator, bytes);
    } else {
        try runRegularBytecode(allocator, bytes);
    }
}

/// Run a regular bytecode module
fn runRegularBytecode(allocator: Allocator, bytes: []const u8) !void {
    const extension = cot.extension;

    // Write to file for debugging
    if (std.fs.cwd().createFile("/tmp/cot_run_debug.log", .{})) |f| {
        _ = f.write("runRegularBytecode entered\n") catch {};
        f.close();
    } else |_| {}
    std.debug.print("[RUN] Running regular bytecode ({d} bytes)\n", .{bytes.len});

    // Initialize extension registry
    extension.initRegistry(allocator);
    defer extension.deinitRegistry();

    // Register TUI extension when available
    if (comptime build_options.enable_tui) {
        extension.registerExtension(cot.cot_tui.extension) catch {};
    }

    // Deserialize module
    var fbs = std.io.fixedBufferStream(bytes);
    var mod = cot.bytecode.Module.deserialize(allocator, fbs.reader()) catch |err| {
        std.debug.print("Error: Invalid bytecode format: {}\n", .{err});
        return error.InvalidBytecode;
    };
    defer mod.deinit();

    // Run VM
    var vm = cot.bytecode.VM.init(allocator);
    defer vm.deinit();

    vm.execute(&mod) catch |err| {
        std.debug.print("Runtime error: {}\n", .{err});
        cot.bytecode.VM.printLastError();
        return err;
    };
}

/// Run bytecode with dependencies loaded
/// In dev mode, supports hot-reloading when the bytecode file changes
fn runBytecodeFileWithDeps(
    allocator: Allocator,
    main_path: []const u8,
    ws: *const workspace.Workspace,
    project: *const workspace.Project,
    writer: anytype,
    dev_mode: bool,
) !void {
    _ = writer;
    debug.print(.general, ">>> runBytecodeFileWithDeps: main_path={s} dev_mode={}", .{ main_path, dev_mode });

    // Convert paths to absolute BEFORE changing directory
    const abs_main_path = try std.fs.cwd().realpathAlloc(allocator, main_path);
    defer allocator.free(abs_main_path);

    const abs_project_path = try std.fs.cwd().realpathAlloc(allocator, project.path);
    defer allocator.free(abs_project_path);

    const tui_runtime = cot.native.tui_runtime;
    const extension = cot.extension;

    // Initialize extension registry for this run
    extension.initRegistry(allocator);
    defer extension.deinitRegistry();

    // Register TUI extension when available
    if (comptime build_options.enable_tui) {
        extension.registerExtension(cot.cot_tui.extension) catch {};
    }

    // Load dependency packages (we do this once, before the reload loop)
    var dep_modules = std.ArrayListUnmanaged([]const u8){};
    defer {
        for (dep_modules.items) |path| allocator.free(path);
        dep_modules.deinit(allocator);
    }

    debug.print(.general, "Loading dependencies for project...", .{});
    var dep_it = project.config.dependencies.iterator();
    while (dep_it.next()) |entry| {
        const dep_name = entry.key_ptr.*;
        const dep_version = entry.value_ptr.*;

        debug.print(.general, "  Checking dependency: {s} = {s}", .{ dep_name, dep_version });

        // Check if it's a workspace dependency
        if (std.mem.startsWith(u8, dep_version, "workspace:")) {
            // Find the package in workspace
            if (ws.findProject(dep_name)) |dep_project| {
                debug.print(.general, "    Found project: {s} at {s}", .{ dep_project.name, dep_project.path });

                const dep_type_subdir = if (dep_project.config.project_type == .library) "packages" else "apps";

                // Try .cot-out/ locations first, then legacy locations
                // 1. .cot-out/.cache/<name>/ (dev cache)
                // 2. .cot-out/packages/<name>/ or .cot-out/apps/<name>/ (release)
                // 3. Legacy: {project.path}/.cot-cache/
                // 4. Legacy: {project.path}/{output_dir}/
                const cache_path = try std.fs.path.join(allocator, &.{ ws.root_path, ".cot-out", ".cache", dep_name });
                defer allocator.free(cache_path);
                const release_path = try std.fs.path.join(allocator, &.{ ws.root_path, ".cot-out", dep_type_subdir, dep_name });
                defer allocator.free(release_path);
                const legacy_cache = try std.fs.path.join(allocator, &.{ dep_project.path, ".cot-cache" });
                defer allocator.free(legacy_cache);
                const legacy_output = try std.fs.path.join(allocator, &.{ dep_project.path, dep_project.config.output_dir });
                defer allocator.free(legacy_output);

                const dirs_to_try = [_][]const u8{ cache_path, release_path, legacy_cache, legacy_output };

                for (dirs_to_try) |dep_output| {
                    debug.print(.general, "    Looking in: {s}", .{dep_output});

                    var dir = std.fs.cwd().openDir(dep_output, .{ .iterate = true }) catch |err| {
                        debug.print(.general, "    Could not open dir: {}", .{err});
                        continue;
                    };
                    defer dir.close();

                    var loaded_any = false;
                    var iter = dir.iterate();
                    while (iter.next() catch null) |file_entry| {
                        // Look for .cotc or .cbo files, or extensionless bundles
                        if (file_entry.kind == .file) {
                            const is_module = std.mem.endsWith(u8, file_entry.name, ".cotc") or
                                std.mem.endsWith(u8, file_entry.name, ".cbo") or
                                std.mem.eql(u8, file_entry.name, dep_name);
                            if (is_module) {
                                const module_path = try std.fs.path.join(allocator, &.{ dep_output, file_entry.name });
                                try dep_modules.append(allocator, module_path);
                                loaded_any = true;
                            }
                        }
                    }

                    // If we found files from this directory, don't check the next
                    if (loaded_any) break;
                }
            } else {
                debug.print(.general, "    Project not found in workspace", .{});
            }
        }
    }

    // Change working directory to project path for relative file access (e.g., data/customers)
    debug.print(.general, "Changing to project directory: {s}", .{abs_project_path});
    try std.posix.chdir(abs_project_path);

    // Hot-reload loop - in dev mode, we restart the app when bytecode changes
    var reload_count: u32 = 0;
    while (true) {
        // Create fresh VM for each run
        var vm = cot.bytecode.VM.init(allocator);
        defer vm.deinit();

        // Load dependency modules into VM
        for (dep_modules.items) |module_path| {
            debug.print(.general, "Loading module: {s}", .{module_path});
            vm.native_registry.loadModule(module_path) catch |err| {
                debug.print(.general, "Warning: Could not load module {s}: {}", .{ module_path, err });
            };
        }

        // Read and deserialize main module (use absolute path since we changed directory)
        const file = std.fs.cwd().openFile(abs_main_path, .{}) catch |err| {
            std.debug.print("Error: Could not open bytecode file: {}\n", .{err});
            return error.FileNotFound;
        };

        const bytes = file.readToEndAlloc(allocator, 1024 * 1024 * 100) catch |err| {
            file.close();
            std.debug.print("Error reading bytecode: {}\n", .{err});
            return error.FileNotFound;
        };
        file.close();
        defer allocator.free(bytes);

        // Check for bundle format (CBUNDLE magic header)
        const is_bundle = bytes.len >= 8 and std.mem.eql(u8, bytes[0..8], "CBUNDLE\x00");
        debug.print(.general, "is_bundle: {}", .{is_bundle});

        // In dev mode, set up file watching and dev mode for hot-reload
        if (dev_mode) {
            // Set pending dev mode (applied when TUI init is called)
            // Must be set each iteration since t_init clears it
            tui_runtime.setPendingDevMode(true);
            // Set pending watch file (applied when TUI init is called)
            tui_runtime.setPendingWatchFile(abs_main_path);
            // Also set directly if TUI is already initialized (for reloads)
            if (tui_runtime.isInitialized()) {
                tui_runtime.setWatchFile(abs_main_path);
                tui_runtime.setDevMode(true);
            }
            if (reload_count > 0) {
                tui_runtime.devLog(">>> App reloaded!");
            }
        }

        var execution_error: ?anyerror = null;
        var error_ip: u32 = 0;
        var error_routine_buf: [128]u8 = undefined;
        var error_routine: ?[]const u8 = null;
        var error_callstack: [][]const u8 = &[_][]const u8{};
        // Rich error info captured from VM.getLastError() before module freed
        var error_source_line: u32 = 0;
        var error_message_buf: [256]u8 = undefined;
        var error_message: ?[]const u8 = null;
        var error_opcode: ?cot.bytecode.Opcode = null;

        if (is_bundle) {
            // Extract main module from bundle and run it
            // Bundle format: header(8) + version(1) + module_count(4) + entries(9 each) + modules
            const module_count = std.mem.readInt(u32, bytes[9..13], .little);
            debug.print(.general, "Bundle has {d} modules", .{module_count});

            // Find main module (type 0) in entries
            var main_offset: ?u32 = null;
            var main_size: ?u32 = null;
            for (0..module_count) |i| {
                const entry_start = 13 + (i * 9);
                const offset = std.mem.readInt(u32, bytes[entry_start..][0..4], .little);
                const size = std.mem.readInt(u32, bytes[entry_start + 4 ..][0..4], .little);
                const mod_type = bytes[entry_start + 8];
                if (mod_type == 0) { // Main module
                    main_offset = offset;
                    main_size = size;
                    break;
                }
            }

            if (main_offset == null) {
                std.debug.print("Error: No main module in bundle\n", .{});
                return error.InvalidBytecode;
            }

            const main_bytes = bytes[main_offset.? .. main_offset.? + main_size.?];
            var fbs = std.io.fixedBufferStream(main_bytes);
            var mod = cot.bytecode.Module.deserialize(allocator, fbs.reader()) catch |err| {
                std.debug.print("Error: Invalid bytecode format in bundle: {}\n", .{err});
                return error.InvalidBytecode;
            };
            defer mod.deinit();

            // Execute
            vm.execute(&mod) catch |err| {
                execution_error = err;
                // Capture location BEFORE mod is freed by defer
                // Must copy routine name since it points into module memory
                const loc = vm.getLocation();
                error_ip = loc.ip;
                if (loc.routine) |r| {
                    const len = @min(r.len, error_routine_buf.len);
                    @memcpy(error_routine_buf[0..len], r[0..len]);
                    error_routine = error_routine_buf[0..len];
                }
                error_callstack = vm.getCallstack(allocator) catch &[_][]const u8{};
                // Capture rich error info before module is freed
                if (cot.bytecode.VM.getLastError()) |rich_err| {
                    error_source_line = rich_err.source_line;
                    error_opcode = rich_err.opcode;
                    const msg_len = @min(rich_err.message.len, error_message_buf.len);
                    @memcpy(error_message_buf[0..msg_len], rich_err.message[0..msg_len]);
                    error_message = error_message_buf[0..msg_len];
                }
            };
        } else {
            var fbs = std.io.fixedBufferStream(bytes);
            var mod = cot.bytecode.Module.deserialize(allocator, fbs.reader()) catch |err| {
                debug.print(.general, "Invalid bytecode format: {}", .{err});
                return error.InvalidBytecode;
            };
            defer mod.deinit();

            // Execute
            vm.execute(&mod) catch |err| {
                execution_error = err;
                // Capture location BEFORE mod is freed by defer
                // Must copy routine name since it points into module memory
                const loc = vm.getLocation();
                error_ip = loc.ip;
                if (loc.routine) |r| {
                    const len = @min(r.len, error_routine_buf.len);
                    @memcpy(error_routine_buf[0..len], r[0..len]);
                    error_routine = error_routine_buf[0..len];
                }
                error_callstack = vm.getCallstack(allocator) catch &[_][]const u8{};
                // Capture rich error info before module is freed
                if (cot.bytecode.VM.getLastError()) |rich_err| {
                    error_source_line = rich_err.source_line;
                    error_opcode = rich_err.opcode;
                    const msg_len = @min(rich_err.message.len, error_message_buf.len);
                    @memcpy(error_message_buf[0..msg_len], rich_err.message[0..msg_len]);
                    error_message = error_message_buf[0..msg_len];
                }
            };
        }

        // Handle execution error
        if (execution_error) |err| {
            // Check for hot-reload request (Ctrl+R in dev mode)
            // This is not an error - it's a signal to reload the app
            if (err == cot.bytecode.vm.VMError.ReloadRequested) {
                if (dev_mode) {
                    // Note: devLog was already called in pollKey when Ctrl+R was detected
                    // Clean up TUI state before reloading (since we aborted mid-execution)
                    tui_runtime.clearReloadPending();
                    tui_runtime.clearWatchFile();
                    tui_runtime.deinit();
                    reload_count += 1;
                    continue;
                }
            }

            // Use pre-captured callstack (captured before mod was freed)
            const callstack = error_callstack;
            defer {
                for (callstack) |entry_item| allocator.free(entry_item);
                allocator.free(callstack);
            }

            // In dev mode, show error in dev console and wait for reload
            if (dev_mode and tui_runtime.isInitialized()) {
                // Use captured error info (captured before module freed)
                var msg_buf: [256]u8 = undefined;
                const routine_name = error_routine orelse "unknown";
                if (error_source_line > 0) {
                    const error_msg = std.fmt.bufPrint(&msg_buf, "Error: {} at {s} line {d}", .{ err, routine_name, error_source_line }) catch "Runtime error";
                    tui_runtime.devLog(error_msg);
                } else {
                    const error_msg = std.fmt.bufPrint(&msg_buf, "Error: {} at {s}", .{ err, routine_name }) catch "Runtime error";
                    tui_runtime.devLog(error_msg);
                }
                if (error_message) |msg| {
                    var msg_buf2: [256]u8 = undefined;
                    const detail_msg = std.fmt.bufPrint(&msg_buf2, "  {s}", .{msg}) catch "";
                    tui_runtime.devLog(detail_msg);
                }
                tui_runtime.devLog("Fix the error and rebuild to reload...");
                tui_runtime.devLog("Press Q or Ctrl+C to quit");

                // Wait for reload or quit
                while (!tui_runtime.isReloadPending()) {
                    std.Thread.sleep(100 * std.time.ns_per_ms);
                    // Check file periodically even without key presses
                    tui_runtime.pollWatchedFile();
                    // Also check for quit keys
                    if (tui_runtime.pollKey()) |key| {
                        // Q, q, Escape, or Ctrl+C to quit
                        const should_quit = switch (key) {
                            .char => |c| c == 'q' or c == 'Q' or c == 3, // q, Q, Ctrl+C
                            .escape => true,
                            else => false,
                        };
                        if (should_quit) {
                            tui_runtime.deinit();
                            return err;
                        }
                    }
                }
            } else if (tui_runtime.isInitialized()) {
                // Display error with TUI - exit TUI first for clean output
                tui_runtime.deinit();

                // Now print to stderr cleanly
                std.debug.print("\n\x1b[1;31m=== Runtime Error ===\x1b[0m\n", .{});
                std.debug.print("Error: {}\n", .{err});
                if (error_message) |msg| {
                    std.debug.print("Message: {s}\n", .{msg});
                }
                const routine_name = error_routine orelse "unknown";
                if (error_source_line > 0) {
                    std.debug.print("At: {s} line {d} (IP: 0x{x:0>4})\n", .{ routine_name, error_source_line, error_ip });
                } else {
                    std.debug.print("At: {s} (IP: 0x{x:0>4})\n", .{ routine_name, error_ip });
                }
                if (error_opcode) |op| {
                    std.debug.print("Opcode: {s}\n", .{@tagName(op)});
                }
                if (callstack.len > 0) {
                    std.debug.print("\nCall Stack:\n", .{});
                    for (callstack) |entry_item| {
                        std.debug.print("  {s}\n", .{entry_item});
                    }
                }
                return err;
            } else {
                // Fallback to stderr - use captured error info
                std.debug.print("\n=== Runtime Error ===\n", .{});
                std.debug.print("Error: {}\n", .{err});
                if (error_message) |msg| {
                    std.debug.print("Message: {s}\n", .{msg});
                }
                const routine_name = error_routine orelse "unknown";
                if (error_source_line > 0) {
                    std.debug.print("At: {s} line {d} (IP: 0x{x:0>4})\n", .{ routine_name, error_source_line, error_ip });
                } else {
                    std.debug.print("At: {s} (IP: 0x{x:0>4})\n", .{ routine_name, error_ip });
                }
                if (error_opcode) |op| {
                    std.debug.print("Opcode: {s}\n", .{@tagName(op)});
                }
                if (callstack.len > 0) {
                    std.debug.print("\nCall Stack:\n", .{});
                    for (callstack) |entry_item| {
                        std.debug.print("  {s}\n", .{entry_item});
                    }
                }
                return err;
            }
        }

        // Check if we should reload (dev mode only)
        if (dev_mode and tui_runtime.isReloadPending()) {
            tui_runtime.clearReloadPending();
            tui_runtime.clearWatchFile();
            reload_count += 1;
            tui_runtime.devLog(">>> Reloading app...");
            // Continue to next iteration of the reload loop
            continue;
        }

        // Normal exit - break out of reload loop
        break;
    }
}

/// Run a bundled bytecode file
fn runBundledFile(allocator: Allocator, bytes: []const u8) !void {
    std.debug.print("[RUN] Running bundled file\n", .{});
    // Parse bundle header
    // Format: "CBUNDLE\0" (8 bytes) + version (1 byte) + module_count (4 bytes)
    if (bytes.len < 13) {
        std.debug.print("Error: Bundle file too small\n", .{});
        return error.InvalidBytecode;
    }

    const version = bytes[8];
    if (version != 0x01) {
        std.debug.print("Error: Unsupported bundle version: {d}\n", .{version});
        return error.InvalidBytecode;
    }

    const module_count = std.mem.readInt(u32, bytes[9..13], .little);
    if (module_count == 0) {
        std.debug.print("Error: Bundle contains no modules\n", .{});
        return error.InvalidBytecode;
    }

    // Parse module table to find main entry
    // Each entry: offset (4 bytes) + size (4 bytes) + type (1 byte)
    const table_entry_size: usize = 9;
    const table_start: usize = 13;

    var main_offset: ?u32 = null;
    var main_size: ?u32 = null;

    for (0..module_count) |i| {
        const entry_start = table_start + (i * table_entry_size);
        if (entry_start + table_entry_size > bytes.len) {
            std.debug.print("Error: Module table extends beyond file\n", .{});
            return error.InvalidBytecode;
        }

        const offset = std.mem.readInt(u32, bytes[entry_start..][0..4], .little);
        const size = std.mem.readInt(u32, bytes[entry_start + 4 ..][0..4], .little);
        const mod_type = bytes[entry_start + 8];

        // Type 0 = main
        if (mod_type == 0) {
            main_offset = offset;
            main_size = size;
            break;
        }
    }

    if (main_offset == null or main_size == null) {
        std.debug.print("Error: No main entry point in bundle\n", .{});
        return error.InvalidBytecode;
    }

    // Extract and run the main module
    const offset = main_offset.?;
    const size = main_size.?;

    if (offset + size > bytes.len) {
        std.debug.print("Error: Main module extends beyond file\n", .{});
        return error.InvalidBytecode;
    }

    const main_bytes = bytes[offset .. offset + size];
    try runRegularBytecode(allocator, main_bytes);
}

/// Parse run command arguments
pub fn parseArgs(args: []const []const u8) RunCommandOptions {
    var options = RunCommandOptions{};
    var i: usize = 0;

    while (i < args.len) : (i += 1) {
        const arg = args[i];
        if (std.mem.eql(u8, arg, "--help") or std.mem.eql(u8, arg, "-h")) {
            options.show_help = true;
        } else if (std.mem.eql(u8, arg, "--dev") or std.mem.eql(u8, arg, "-d")) {
            options.dev_mode = true;
        } else if (std.mem.startsWith(u8, arg, "--filter=")) {
            // Turborepo-style --filter=<project>
            options.filter = arg[9..];
        } else if (std.mem.eql(u8, arg, "--filter") or std.mem.eql(u8, arg, "-f")) {
            // --filter <project> (separate arg)
            if (i + 1 < args.len) {
                i += 1;
                options.filter = args[i];
            }
        } else if (!std.mem.startsWith(u8, arg, "-")) {
            if (options.target == null) {
                options.target = arg;
            }
        }
    }

    return options;
}

pub fn printHelp() !void {
    var stdout_buffer: [4096]u8 = undefined;
    var stdout_file = std.fs.File.stdout();
    var stdout_writer = stdout_file.writer(&stdout_buffer);
    const stdout = &stdout_writer.interface;

    try stdout.writeAll(
        \\Usage: cot run [target] [options]
        \\
        \\Run a compiled project or component.
        \\
        \\Arguments:
        \\  target            What to run (optional)
        \\                    - project name: runs main entry point
        \\                    - project:type:name: runs specific component
        \\
        \\Target Specifier:
        \\  inventory                     Run main entry point
        \\  inventory:screen:products     Run a screen
        \\  inventory:job:nightly_sync    Run a batch job
        \\  inventory:report:sales        Run a report
        \\
        \\Options:
        \\  -d, --dev         Enable dev mode (split-pane with dev console)
        \\  -f, --filter=X    Run only project X (Turborepo-style)
        \\  -h, --help        Show this help
        \\
        \\Examples:
        \\  cot run                       Run default app
        \\  cot run inventory             Run inventory app main
        \\  cot run --dev                 Run with dev tools pane
        \\  cot run --filter=inventory    Run with Turborepo-style filter
        \\
        \\Note: Projects must be compiled first with 'cot build'
        \\
    );
}
