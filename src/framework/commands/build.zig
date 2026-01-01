//! cot build command
//!
//! Builds all or specific projects in the workspace.

const std = @import("std");
const cot = @import("cot");
const config = @import("../config.zig");
const workspace = @import("../workspace.zig");
const pipeline = @import("../build/pipeline.zig");
const log = cot.log;
const Allocator = std.mem.Allocator;

pub const BuildCommandOptions = struct {
    project: ?[]const u8 = null,
    filter: ?[]const u8 = null, // --filter=<project> like Turborepo
    output_dir: ?[]const u8 = null,
    verbose_level: u8 = 0, // 0=normal, 1=verbose, 2=debug, 3=trace
    quiet: bool = false,
    clean: bool = false,
    rebuild: bool = false, // Full rebuild: clean all caches before building
    release: bool = false, // Bundle into single executable
    show_help: bool = false,

    /// Get the effective project filter (supports both positional and --filter)
    pub fn getProjectFilter(self: BuildCommandOptions) ?[]const u8 {
        return self.filter orelse self.project;
    }

    /// Get the log level for this build
    pub fn getLogLevel(self: BuildCommandOptions) log.Level {
        if (self.quiet) return .silent;
        if (self.verbose_level > 0) return log.Level.fromVerboseCount(self.verbose_level);
        // No explicit flags - check environment variable
        return log.Level.fromEnv();
    }

    /// Legacy compatibility: is verbose enabled?
    pub fn isVerbose(self: BuildCommandOptions) bool {
        return self.verbose_level >= 1;
    }
};

/// Run the build command
pub fn run(allocator: Allocator, options: BuildCommandOptions) !void {
    // Zig 0.15 stdout pattern: buffered writer with .interface and flush()
    var stdout_buffer: [4096]u8 = undefined;
    var stdout_file = std.fs.File.stdout();
    var stdout_writer = stdout_file.writer(&stdout_buffer);
    const stdout = &stdout_writer.interface;

    if (options.show_help) {
        try printHelp();
        return;
    }

    // Set up logging based on verbosity flags
    log.setLevel(options.getLogLevel());
    const logger = log.getLogger();

    // Find workspace
    const cwd = try std.fs.cwd().realpathAlloc(allocator, ".");
    defer allocator.free(cwd);

    var config_loader = config.ConfigLoader.init(allocator);
    const workspace_root = try config_loader.findWorkspaceRoot(cwd) orelse {
        logger.err("Not inside a Cot workspace. Run 'cot init' first.", .{});
        return error.NoWorkspace;
    };
    defer allocator.free(workspace_root);

    // Load workspace
    var ws_loader = workspace.WorkspaceLoader.init(allocator);
    var ws = try ws_loader.load(workspace_root);
    defer ws.deinit();

    // Print workspace info (unless quiet mode)
    if (logger.isEnabled(.normal)) {
        try stdout.print("Building workspace: {s}\n", .{ws.name});
        if (logger.isEnabled(.verbose)) {
            try stdout.print("Root: {s}\n", .{ws.root_path});
        }
        try stdout.print("\n", .{});
        try stdout.flush();
    }

    // Build options (use filter if provided, otherwise project)
    const build_options = pipeline.BuildOptions{
        .project = options.getProjectFilter(),
        .output_dir = options.output_dir,
        .verbose = options.isVerbose(),
        .clean = options.clean,
        .rebuild = options.rebuild,
        .release = options.release,
    };

    if (options.release) {
        logger.verbose("Release build: bundling all modules", .{});
    }

    // Run build pipeline with timing
    const build_timer = log.Timer.init("build");
    var build_pipeline = pipeline.Pipeline.init(allocator, build_options);
    var result = try build_pipeline.buildWorkspace(&ws);
    defer result.deinit();

    // Print results (unless quiet mode, unless there are errors)
    if (logger.isEnabled(.normal) or !result.isSuccess()) {
        try pipeline.printBuildSummary(&result, stdout);
    }

    // Show total time at verbose level
    if (logger.isEnabled(.verbose)) {
        try stdout.print("\nTotal time: {d}ms\n", .{build_timer.elapsedMs()});
    }

    if (logger.isEnabled(.normal) or !result.isSuccess()) {
        try stdout.flush();
    }

    if (!result.isSuccess()) {
        return error.BuildFailed;
    }
}

/// Parse build command arguments
pub fn parseArgs(args: []const []const u8) BuildCommandOptions {
    var options = BuildCommandOptions{};
    var i: usize = 0;

    while (i < args.len) : (i += 1) {
        const arg = args[i];
        if (std.mem.eql(u8, arg, "--help") or std.mem.eql(u8, arg, "-h")) {
            options.show_help = true;
        } else if (std.mem.eql(u8, arg, "--quiet") or std.mem.eql(u8, arg, "-q")) {
            options.quiet = true;
        } else if (std.mem.eql(u8, arg, "--verbose") or std.mem.eql(u8, arg, "-v")) {
            options.verbose_level = @min(options.verbose_level + 1, 3);
        } else if (std.mem.eql(u8, arg, "-vv")) {
            options.verbose_level = 2;
        } else if (std.mem.eql(u8, arg, "-vvv")) {
            options.verbose_level = 3;
        } else if (std.mem.eql(u8, arg, "--clean") or std.mem.eql(u8, arg, "-c")) {
            options.clean = true;
        } else if (std.mem.eql(u8, arg, "--rebuild") or std.mem.eql(u8, arg, "-R")) {
            options.rebuild = true;
        } else if (std.mem.eql(u8, arg, "--release") or std.mem.eql(u8, arg, "-r")) {
            options.release = true;
        } else if (std.mem.eql(u8, arg, "--output") or std.mem.eql(u8, arg, "-o")) {
            if (i + 1 < args.len) {
                i += 1;
                options.output_dir = args[i];
            }
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
            // Project path/name (positional)
            if (options.project == null) {
                options.project = arg;
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
        \\Usage: cot build [project] [options]
        \\
        \\Build all or specific projects in the workspace.
        \\
        \\Arguments:
        \\  project           Project name or path to build (optional, builds all if omitted)
        \\
        \\Options:
        \\  -r, --release     Release build (bundle into single executable)
        \\  -f, --filter=X    Build only project X (Turborepo-style)
        \\  -o, --output      Output directory override
        \\  -c, --clean       Clean build (remove cache first)
        \\  -R, --rebuild     Full rebuild (remove all caches and recompile everything)
        \\  -h, --help        Show this help
        \\
        \\Verbosity:
        \\  -q, --quiet       Errors only (silent)
        \\  (default)         Progress + warnings + errors
        \\  -v, --verbose     + timing, file counts, stages
        \\  -vv               + IR/bytecode details
        \\  -vvv              + instruction-level tracing
        \\
        \\Output formats:
        \\  Development:      .cotc files (individual compiled modules)
        \\  Release apps:     bin/<name> (single executable, runs on Cot VM)
        \\  Release libs:     bin/<name>.cotpkg (library package)
        \\
        \\Examples:
        \\  cot build                      Build all (development)
        \\  cot build -v                   Build with timing info
        \\  cot build --release            Build all (production bundle)
        \\  cot build --clean --release    Clean release build
        \\
        \\Environment:
        \\  COT_LOG_LEVEL=N   Set log level (0=silent, 1=normal, 2=verbose, 3=debug, 4=trace)
        \\  COT_DEBUG_VM=1    Enable VM runtime tracing (separate from build logging)
        \\
    );
    try stdout.flush();
}
