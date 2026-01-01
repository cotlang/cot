//! Workspace Management
//!
//! Discovers and manages monorepo workspaces with apps/ and packages/.
//! Handles dependency resolution and build ordering.

const std = @import("std");
const config = @import("config.zig");
const Allocator = std.mem.Allocator;

/// A discovered project (app or package) in the workspace
pub const Project = struct {
    name: []const u8,
    path: []const u8,
    config: config.ProjectConfig,
    project_type: config.ProjectType,

    pub fn deinit(self: *Project, allocator: Allocator) void {
        allocator.free(self.name);
        allocator.free(self.path);
        var cfg = self.config;
        cfg.deinit();
    }
};

/// Workspace containing multiple projects
pub const Workspace = struct {
    root_path: []const u8,
    name: []const u8,
    apps: std.ArrayList(Project),
    packages: std.ArrayList(Project),
    allocator: Allocator,

    const Self = @This();

    pub fn init(allocator: Allocator, root_path: []const u8, name: []const u8) !Self {
        return .{
            .root_path = try allocator.dupe(u8, root_path),
            .name = try allocator.dupe(u8, name),
            .apps = .{},
            .packages = .{},
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Self) void {
        for (self.apps.items) |*app| {
            app.deinit(self.allocator);
        }
        self.apps.deinit(self.allocator);

        for (self.packages.items) |*pkg| {
            pkg.deinit(self.allocator);
        }
        self.packages.deinit(self.allocator);

        self.allocator.free(self.root_path);
        self.allocator.free(self.name);
    }

    /// Get all projects (apps + packages)
    pub fn allProjects(self: *const Self) ![]const Project {
        var all: std.ArrayList(Project) = .{};
        try all.appendSlice(self.allocator, self.packages.items);
        try all.appendSlice(self.allocator, self.apps.items);
        return try all.toOwnedSlice(self.allocator);
    }

    /// Find a project by name or path
    pub fn findProject(self: *const Self, name_or_path: []const u8) ?*const Project {
        // First try exact name match
        for (self.packages.items) |*pkg| {
            if (std.mem.eql(u8, pkg.name, name_or_path)) return pkg;
        }
        for (self.apps.items) |*app| {
            if (std.mem.eql(u8, app.name, name_or_path)) return app;
        }

        // Try matching by relative path (e.g., "apps/customers")
        for (self.packages.items) |*pkg| {
            if (std.mem.endsWith(u8, pkg.path, name_or_path)) return pkg;
        }
        for (self.apps.items) |*app| {
            if (std.mem.endsWith(u8, app.path, name_or_path)) return app;
        }

        return null;
    }

    /// Get build order (packages first, then apps, respecting dependencies)
    pub fn getBuildOrder(self: *const Self) ![]const *const Project {
        var order: std.ArrayList(*const Project) = .{};
        var visited = std.StringHashMap(bool).init(self.allocator);
        defer visited.deinit();

        // Add all packages first (they have no app dependencies)
        for (self.packages.items) |*pkg| {
            try order.append(self.allocator, pkg);
            try visited.put(pkg.name, true);
        }

        // Add apps
        for (self.apps.items) |*app| {
            if (!visited.contains(app.name)) {
                try order.append(self.allocator, app);
            }
        }

        return try order.toOwnedSlice(self.allocator);
    }
};

/// Workspace discovery and loading
pub const WorkspaceLoader = struct {
    allocator: Allocator,
    config_loader: config.ConfigLoader,

    const Self = @This();

    pub fn init(allocator: Allocator) Self {
        return .{
            .allocator = allocator,
            .config_loader = config.ConfigLoader.init(allocator),
        };
    }

    /// Load workspace from a root directory
    pub fn load(self: *Self, root_path: []const u8) !Workspace {
        // Load root config
        var root_config = try self.config_loader.load(root_path);
        defer root_config.deinit();

        var workspace = try Workspace.init(self.allocator, root_path, root_config.name);
        errdefer workspace.deinit();

        // Discover apps
        const apps_path = try std.fs.path.join(self.allocator, &.{ root_path, "apps" });
        defer self.allocator.free(apps_path);

        if (std.fs.cwd().openDir(apps_path, .{ .iterate = true })) |dir| {
            var apps_dir = dir;
            defer apps_dir.close();

            var it = apps_dir.iterate();
            while (try it.next()) |entry| {
                if (entry.kind == .directory) {
                    const app_path = try std.fs.path.join(self.allocator, &.{ apps_path, entry.name });
                    defer self.allocator.free(app_path);

                    if (config.ConfigLoader.exists(app_path)) {
                        const app_config = try self.config_loader.load(app_path);

                        const project = Project{
                            .name = try self.allocator.dupe(u8, app_config.name),
                            .path = try self.allocator.dupe(u8, app_path),
                            .config = app_config,
                            .project_type = .app,
                        };
                        try workspace.apps.append(self.allocator, project);
                    }
                }
            }
        } else |_| {
            // apps/ directory doesn't exist yet, that's fine
        }

        // Discover packages
        const packages_path = try std.fs.path.join(self.allocator, &.{ root_path, "packages" });
        defer self.allocator.free(packages_path);

        if (std.fs.cwd().openDir(packages_path, .{ .iterate = true })) |dir| {
            var packages_dir = dir;
            defer packages_dir.close();

            var it = packages_dir.iterate();
            while (try it.next()) |entry| {
                if (entry.kind == .directory) {
                    const pkg_path = try std.fs.path.join(self.allocator, &.{ packages_path, entry.name });
                    defer self.allocator.free(pkg_path);

                    if (config.ConfigLoader.exists(pkg_path)) {
                        const pkg_config = try self.config_loader.load(pkg_path);

                        const project = Project{
                            .name = try self.allocator.dupe(u8, pkg_config.name),
                            .path = try self.allocator.dupe(u8, pkg_path),
                            .config = pkg_config,
                            .project_type = .library,
                        };
                        try workspace.packages.append(self.allocator, project);
                    }
                }
            }
        } else |_| {
            // packages/ directory doesn't exist yet, that's fine
        }

        return workspace;
    }

    /// Find and load workspace from current directory or parents
    pub fn discover(self: *Self) !?Workspace {
        const cwd = try std.fs.cwd().realpathAlloc(self.allocator, ".");
        defer self.allocator.free(cwd);

        if (try self.config_loader.findWorkspaceRoot(cwd)) |root| {
            defer self.allocator.free(root);
            return try self.load(root);
        }

        return null;
    }

    /// Create a new workspace
    pub fn create(self: *Self, path: []const u8, name: []const u8) !void {
        // Create directories
        const apps_path = try std.fs.path.join(self.allocator, &.{ path, "apps" });
        defer self.allocator.free(apps_path);
        try std.fs.cwd().makePath(apps_path);

        const packages_path = try std.fs.path.join(self.allocator, &.{ path, "packages" });
        defer self.allocator.free(packages_path);
        try std.fs.cwd().makePath(packages_path);

        // Create cot.json
        const config_path = try std.fs.path.join(self.allocator, &.{ path, "cot.json" });
        defer self.allocator.free(config_path);

        const file = try std.fs.cwd().createFile(config_path, .{});
        defer file.close();
        try config.writeWorkspaceConfig(file, name);
    }

    /// Create a new app in the workspace
    pub fn createApp(self: *Self, workspace_root: []const u8, name: []const u8) !void {
        const app_path = try std.fs.path.join(self.allocator, &.{ workspace_root, "apps", name });
        defer self.allocator.free(app_path);

        // Create app directory
        try std.fs.cwd().makePath(app_path);

        // Create cot.json
        const config_path = try std.fs.path.join(self.allocator, &.{ app_path, "cot.json" });
        defer self.allocator.free(config_path);

        const file = try std.fs.cwd().createFile(config_path, .{});
        defer file.close();
        try config.writeAppConfig(file, name);

        // Create main.cot
        const main_path = try std.fs.path.join(self.allocator, &.{ app_path, "main.cot" });
        defer self.allocator.free(main_path);

        const main_file = try std.fs.cwd().createFile(main_path, .{});
        defer main_file.close();
        try main_file.writeAll(
            \\; Main entry point for the application
            \\main
            \\record
            \\    message, a50
            \\endrecord
            \\proc
            \\    message = "Hello from Cot!"
            \\    display(0, message)
            \\    stop
            \\endmain
            \\
        );
    }

    /// Create a new package in the workspace
    pub fn createPackage(self: *Self, workspace_root: []const u8, name: []const u8) !void {
        const pkg_path = try std.fs.path.join(self.allocator, &.{ workspace_root, "packages", name });
        defer self.allocator.free(pkg_path);

        // Create package directory structure
        try std.fs.cwd().makePath(pkg_path);

        const src_path = try std.fs.path.join(self.allocator, &.{ pkg_path, "src" });
        defer self.allocator.free(src_path);
        try std.fs.cwd().makePath(src_path);

        // Create cot.json
        const config_path = try std.fs.path.join(self.allocator, &.{ pkg_path, "cot.json" });
        defer self.allocator.free(config_path);

        const file = try std.fs.cwd().createFile(config_path, .{});
        defer file.close();
        try config.writeLibraryConfig(file, name);

        // Create index.cot
        const index_path = try std.fs.path.join(self.allocator, &.{ src_path, "index.cot" });
        defer self.allocator.free(index_path);

        const index_file = try std.fs.cwd().createFile(index_path, .{});
        defer index_file.close();
        try index_file.writeAll(
            \\; Package entry point
            \\; Export shared records and subroutines here
            \\
        );
    }
};

/// Print workspace summary
pub fn printWorkspaceSummary(workspace: *const Workspace, writer: anytype) !void {
    try writer.print("Workspace: {s}\n", .{workspace.name});
    try writer.print("Root: {s}\n\n", .{workspace.root_path});

    if (workspace.apps.items.len > 0) {
        try writer.print("Apps ({d}):\n", .{workspace.apps.items.len});
        for (workspace.apps.items) |app| {
            try writer.print("  - {s} ({s})\n", .{ app.name, app.path });
        }
        try writer.writeAll("\n");
    }

    if (workspace.packages.items.len > 0) {
        try writer.print("Packages ({d}):\n", .{workspace.packages.items.len});
        for (workspace.packages.items) |pkg| {
            try writer.print("  - {s} ({s})\n", .{ pkg.name, pkg.path });
        }
    }
}

// Tests
test "create workspace structure" {
    // This would need a temp directory for proper testing
}
