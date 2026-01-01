//! Dependency Resolver
//!
//! Resolves dependencies between packages in a workspace.
//! Performs topological sorting to determine build order.

const std = @import("std");
const config = @import("config.zig");
const workspace = @import("workspace.zig");
const Allocator = std.mem.Allocator;

/// Dependency graph node
pub const DependencyNode = struct {
    name: []const u8,
    path: []const u8,
    project_type: config.ProjectType,
    dependencies: std.ArrayList([]const u8),
    allocator: Allocator,
    resolved: bool = false,

    pub fn init(allocator: Allocator, name: []const u8, path: []const u8, project_type: config.ProjectType) DependencyNode {
        return .{
            .name = name,
            .path = path,
            .project_type = project_type,
            .dependencies = .{},
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *DependencyNode) void {
        self.dependencies.deinit(self.allocator);
    }

    pub fn addDependency(self: *DependencyNode, dep: []const u8) !void {
        try self.dependencies.append(self.allocator, dep);
    }
};

/// Dependency resolution result
pub const ResolvedDependencies = struct {
    /// Build order (packages first, then apps)
    build_order: std.ArrayList([]const u8),
    /// Map of project name to its resolved dependencies
    dependency_map: std.StringHashMap(std.ArrayList([]const u8)),
    allocator: Allocator,

    pub fn init(allocator: Allocator) ResolvedDependencies {
        return .{
            .build_order = .{},
            .dependency_map = std.StringHashMap(std.ArrayList([]const u8)).init(allocator),
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *ResolvedDependencies) void {
        self.build_order.deinit(self.allocator);
        var it = self.dependency_map.iterator();
        while (it.next()) |entry| {
            entry.value_ptr.deinit(self.allocator);
        }
        self.dependency_map.deinit();
    }
};

/// Dependency resolver
pub const Resolver = struct {
    allocator: Allocator,
    nodes: std.StringHashMap(DependencyNode),

    const Self = @This();

    pub fn init(allocator: Allocator) Self {
        return .{
            .allocator = allocator,
            .nodes = std.StringHashMap(DependencyNode).init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        var it = self.nodes.iterator();
        while (it.next()) |entry| {
            entry.value_ptr.deinit();
        }
        self.nodes.deinit();
    }

    /// Add a project to the dependency graph
    pub fn addProject(self: *Self, project: *const workspace.Project) !void {
        var node = DependencyNode.init(
            self.allocator,
            project.name,
            project.path,
            project.project_type,
        );

        // Add dependencies from config
        var dep_it = project.config.dependencies.iterator();
        while (dep_it.next()) |entry| {
            const dep_name = entry.key_ptr.*;
            // Handle workspace: prefix
            if (std.mem.startsWith(u8, entry.value_ptr.*, "workspace:")) {
                try node.addDependency(dep_name);
            }
        }

        try self.nodes.put(project.name, node);
    }

    /// Load all projects from a workspace
    pub fn loadWorkspace(self: *Self, ws: *const workspace.Workspace) !void {
        // Add all packages
        for (ws.packages.items) |*pkg| {
            try self.addProject(pkg);
        }

        // Add all apps
        for (ws.apps.items) |*app| {
            try self.addProject(app);
        }
    }

    /// Resolve dependencies and return build order
    pub fn resolve(self: *Self) !ResolvedDependencies {
        var result = ResolvedDependencies.init(self.allocator);
        errdefer result.deinit();

        // Track visited nodes for cycle detection
        var visiting = std.StringHashMap(bool).init(self.allocator);
        defer visiting.deinit();

        var visited = std.StringHashMap(bool).init(self.allocator);
        defer visited.deinit();

        // Topological sort using DFS
        var it = self.nodes.iterator();
        while (it.next()) |entry| {
            const name = entry.key_ptr.*;
            if (!visited.contains(name)) {
                try self.visit(name, &visiting, &visited, &result.build_order);
            }
        }

        // Build dependency map
        var node_it = self.nodes.iterator();
        while (node_it.next()) |entry| {
            var deps: std.ArrayList([]const u8) = .{};
            for (entry.value_ptr.dependencies.items) |dep| {
                try deps.append(self.allocator, dep);
            }
            try result.dependency_map.put(entry.key_ptr.*, deps);
        }

        return result;
    }

    fn visit(
        self: *Self,
        name: []const u8,
        visiting: *std.StringHashMap(bool),
        visited: *std.StringHashMap(bool),
        order: *std.ArrayList([]const u8),
    ) !void {
        if (visiting.contains(name)) {
            return error.CyclicDependency;
        }

        if (visited.contains(name)) {
            return;
        }

        try visiting.put(name, true);

        // Visit dependencies first
        if (self.nodes.get(name)) |node| {
            for (node.dependencies.items) |dep| {
                try self.visit(dep, visiting, visited, order);
            }
        }

        _ = visiting.remove(name);
        try visited.put(name, true);
        try order.append(self.allocator, name);
    }

    /// Check if a dependency exists
    pub fn hasDependency(self: *Self, project: []const u8, dependency: []const u8) bool {
        if (self.nodes.get(project)) |node| {
            for (node.dependencies.items) |dep| {
                if (std.mem.eql(u8, dep, dependency)) {
                    return true;
                }
            }
        }
        return false;
    }

    /// Get all dependencies for a project (transitive)
    pub fn getTransitiveDependencies(self: *Self, project: []const u8) !std.ArrayList([]const u8) {
        var deps: std.ArrayList([]const u8) = .{};
        var visited = std.StringHashMap(bool).init(self.allocator);
        defer visited.deinit();

        try self.collectDependencies(project, &deps, &visited);
        return deps;
    }

    fn collectDependencies(
        self: *Self,
        project: []const u8,
        deps: *std.ArrayList([]const u8),
        visited: *std.StringHashMap(bool),
    ) !void {
        if (visited.contains(project)) return;
        try visited.put(project, true);

        if (self.nodes.get(project)) |node| {
            for (node.dependencies.items) |dep| {
                try self.collectDependencies(dep, deps, visited);
                try deps.append(self.allocator, dep);
            }
        }
    }
};

/// Print dependency graph for debugging
pub fn printDependencyGraph(resolver: *const Resolver, writer: anytype) !void {
    try writer.writeAll("Dependency Graph:\n");

    var it = resolver.nodes.iterator();
    while (it.next()) |entry| {
        const node = entry.value_ptr;
        const type_str = switch (node.project_type) {
            .app => "app",
            .library => "lib",
            .workspace => "workspace",
        };
        try writer.print("  {s} ({s})\n", .{ node.name, type_str });

        if (node.dependencies.items.len > 0) {
            for (node.dependencies.items) |dep| {
                try writer.print("    -> {s}\n", .{dep});
            }
        }
    }
}

// Tests
test "resolve empty workspace" {
    const allocator = std.testing.allocator;
    var resolver = Resolver.init(allocator);
    defer resolver.deinit();

    var result = try resolver.resolve();
    defer result.deinit();

    try std.testing.expect(result.build_order.items.len == 0);
}
