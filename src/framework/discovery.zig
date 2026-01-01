//! Convention-Based Discovery
//!
//! Automatically discovers screens, reports, jobs, API endpoints,
//! and subroutines based on directory conventions.

const std = @import("std");
const config = @import("config.zig");
const workspace = @import("workspace.zig");
const Allocator = std.mem.Allocator;

/// Type of discovered component
pub const ComponentType = enum {
    screen,
    report,
    job,
    api,
    subroutine,
    main,
};

/// A discovered component
pub const Component = struct {
    name: []const u8,
    path: []const u8,
    component_type: ComponentType,
    /// Route for API endpoints (e.g., "/api/products")
    route: ?[]const u8 = null,

    pub fn deinit(self: *Component, allocator: Allocator) void {
        allocator.free(self.name);
        allocator.free(self.path);
        if (self.route) |r| allocator.free(r);
    }
};

/// Discovery result for a project
pub const DiscoveryResult = struct {
    project_name: []const u8,
    project_path: []const u8,
    main_entry: ?[]const u8 = null,
    screens: std.ArrayList(Component),
    reports: std.ArrayList(Component),
    jobs: std.ArrayList(Component),
    api_endpoints: std.ArrayList(Component),
    subroutines: std.ArrayList(Component),
    allocator: Allocator,

    pub fn init(allocator: Allocator, name: []const u8, path: []const u8) !DiscoveryResult {
        return .{
            .project_name = try allocator.dupe(u8, name),
            .project_path = try allocator.dupe(u8, path),
            .screens = .{},
            .reports = .{},
            .jobs = .{},
            .api_endpoints = .{},
            .subroutines = .{},
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *DiscoveryResult) void {
        self.allocator.free(self.project_name);
        self.allocator.free(self.project_path);
        if (self.main_entry) |m| self.allocator.free(m);

        for (self.screens.items) |*s| s.deinit(self.allocator);
        self.screens.deinit(self.allocator);

        for (self.reports.items) |*r| r.deinit(self.allocator);
        self.reports.deinit(self.allocator);

        for (self.jobs.items) |*j| j.deinit(self.allocator);
        self.jobs.deinit(self.allocator);

        for (self.api_endpoints.items) |*a| a.deinit(self.allocator);
        self.api_endpoints.deinit(self.allocator);

        for (self.subroutines.items) |*s| s.deinit(self.allocator);
        self.subroutines.deinit(self.allocator);
    }

    /// Get total component count
    pub fn totalComponents(self: *const DiscoveryResult) usize {
        var count: usize = 0;
        if (self.main_entry != null) count += 1;
        count += self.screens.items.len;
        count += self.reports.items.len;
        count += self.jobs.items.len;
        count += self.api_endpoints.items.len;
        count += self.subroutines.items.len;
        return count;
    }
};

/// Component discoverer
pub const Discoverer = struct {
    allocator: Allocator,

    const Self = @This();

    pub fn init(allocator: Allocator) Self {
        return .{ .allocator = allocator };
    }

    /// Discover all components in a project
    pub fn discoverProject(self: *Self, project: *const workspace.Project) !DiscoveryResult {
        var result = try DiscoveryResult.init(self.allocator, project.name, project.path);
        errdefer result.deinit();

        // Check for main entry point
        if (project.config.main) |main_file| {
            const main_path = try std.fs.path.join(self.allocator, &.{ project.path, main_file });
            defer self.allocator.free(main_path);

            if (fileExists(main_path)) {
                result.main_entry = try self.allocator.dupe(u8, main_path);
            }
        }

        // Discover screens
        try self.discoverDirectory(
            project.path,
            project.config.screens_dir,
            .screen,
            &result.screens,
            null,
        );

        // Discover reports
        try self.discoverDirectory(
            project.path,
            project.config.reports_dir,
            .report,
            &result.reports,
            null,
        );

        // Discover jobs
        try self.discoverDirectory(
            project.path,
            "jobs",
            .job,
            &result.jobs,
            null,
        );

        // Discover API endpoints
        try self.discoverDirectory(
            project.path,
            "api",
            .api,
            &result.api_endpoints,
            "/api",
        );

        // Discover subroutines
        try self.discoverDirectory(
            project.path,
            "subroutines",
            .subroutine,
            &result.subroutines,
            null,
        );

        return result;
    }

    /// Discover .cot files in a directory
    fn discoverDirectory(
        self: *Self,
        project_path: []const u8,
        dir_name: []const u8,
        component_type: ComponentType,
        components: *std.ArrayList(Component),
        route_prefix: ?[]const u8,
    ) !void {
        const dir_path = try std.fs.path.join(self.allocator, &.{ project_path, dir_name });
        defer self.allocator.free(dir_path);

        var dir = std.fs.cwd().openDir(dir_path, .{ .iterate = true }) catch |err| {
            if (err == error.FileNotFound) return;
            return err;
        };
        defer dir.close();

        try self.scanDirectory(dir, dir_path, "", component_type, components, route_prefix);
    }

    /// Recursively scan directory for .cot files
    fn scanDirectory(
        self: *Self,
        dir: std.fs.Dir,
        base_path: []const u8,
        relative_path: []const u8,
        component_type: ComponentType,
        components: *std.ArrayList(Component),
        route_prefix: ?[]const u8,
    ) !void {
        var it = dir.iterate();
        while (try it.next()) |entry| {
            if (entry.kind == .directory) {
                // Skip hidden directories
                if (entry.name[0] == '.') continue;

                const sub_path = if (relative_path.len > 0)
                    try std.fmt.allocPrint(self.allocator, "{s}/{s}", .{ relative_path, entry.name })
                else
                    try self.allocator.dupe(u8, entry.name);
                defer self.allocator.free(sub_path);

                const full_path = try std.fs.path.join(self.allocator, &.{ base_path, entry.name });
                defer self.allocator.free(full_path);

                var sub_dir = dir.openDir(entry.name, .{ .iterate = true }) catch continue;
                defer sub_dir.close();

                try self.scanDirectory(sub_dir, full_path, sub_path, component_type, components, route_prefix);
            } else if (entry.kind == .file) {
                // Check for .cot extension
                if (!std.mem.endsWith(u8, entry.name, ".cot")) continue;

                // Skip layout files (Next.js convention)
                if (std.mem.eql(u8, entry.name, "_layout.cot")) continue;

                // Get component name (filename without extension)
                const name_end = entry.name.len - 4; // Remove ".cot"
                const base_name = entry.name[0..name_end];

                // Build full component name including path
                const component_name = if (relative_path.len > 0)
                    try std.fmt.allocPrint(self.allocator, "{s}/{s}", .{ relative_path, base_name })
                else
                    try self.allocator.dupe(u8, base_name);

                // Build full file path
                const file_path = try std.fs.path.join(self.allocator, &.{ base_path, entry.name });

                // Build route for API endpoints
                var route: ?[]const u8 = null;
                if (route_prefix) |prefix| {
                    // Handle dynamic routes [id] -> :id
                    const route_name = try self.allocator.dupe(u8, component_name);
                    defer self.allocator.free(route_name);

                    // Replace [param] with :param
                    var route_builder: std.ArrayList(u8) = .{};
                    defer route_builder.deinit(self.allocator);

                    var i: usize = 0;
                    while (i < route_name.len) {
                        if (route_name[i] == '[') {
                            try route_builder.append(self.allocator, ':');
                            i += 1;
                            while (i < route_name.len and route_name[i] != ']') {
                                try route_builder.append(self.allocator, route_name[i]);
                                i += 1;
                            }
                            if (i < route_name.len) i += 1; // Skip ']'
                        } else {
                            try route_builder.append(self.allocator, route_name[i]);
                            i += 1;
                        }
                    }

                    route = try std.fmt.allocPrint(self.allocator, "{s}/{s}", .{ prefix, route_builder.items });
                }

                const component = Component{
                    .name = component_name,
                    .path = file_path,
                    .component_type = component_type,
                    .route = route,
                };

                try components.append(self.allocator, component);
            }
        }
    }

    /// Discover all projects in a workspace
    pub fn discoverWorkspace(self: *Self, ws: *const workspace.Workspace) !std.ArrayList(DiscoveryResult) {
        var results: std.ArrayList(DiscoveryResult) = .{};
        errdefer {
            for (results.items) |*r| r.deinit();
            results.deinit(self.allocator);
        }

        // Discover packages first
        for (ws.packages.items) |*pkg| {
            const result = try self.discoverProject(pkg);
            try results.append(self.allocator, result);
        }

        // Then apps
        for (ws.apps.items) |*app| {
            const result = try self.discoverProject(app);
            try results.append(self.allocator, result);
        }

        return results;
    }
};

/// Check if a file exists
fn fileExists(path: []const u8) bool {
    std.fs.cwd().access(path, .{}) catch return false;
    return true;
}

/// Print discovery results
pub fn printDiscoveryResult(result: *const DiscoveryResult, writer: anytype) !void {
    try writer.print("Project: {s}\n", .{result.project_name});
    try writer.print("Path: {s}\n", .{result.project_path});

    if (result.main_entry) |main| {
        try writer.print("Main: {s}\n", .{main});
    }

    if (result.screens.items.len > 0) {
        try writer.print("\nScreens ({d}):\n", .{result.screens.items.len});
        for (result.screens.items) |screen| {
            try writer.print("  - {s}\n", .{screen.name});
        }
    }

    if (result.reports.items.len > 0) {
        try writer.print("\nReports ({d}):\n", .{result.reports.items.len});
        for (result.reports.items) |report| {
            try writer.print("  - {s}\n", .{report.name});
        }
    }

    if (result.jobs.items.len > 0) {
        try writer.print("\nJobs ({d}):\n", .{result.jobs.items.len});
        for (result.jobs.items) |job| {
            try writer.print("  - {s}\n", .{job.name});
        }
    }

    if (result.api_endpoints.items.len > 0) {
        try writer.print("\nAPI Endpoints ({d}):\n", .{result.api_endpoints.items.len});
        for (result.api_endpoints.items) |api| {
            if (api.route) |route| {
                try writer.print("  - {s} -> {s}\n", .{ api.name, route });
            } else {
                try writer.print("  - {s}\n", .{api.name});
            }
        }
    }

    if (result.subroutines.items.len > 0) {
        try writer.print("\nSubroutines ({d}):\n", .{result.subroutines.items.len});
        for (result.subroutines.items) |sub| {
            try writer.print("  - {s}\n", .{sub.name});
        }
    }
}
