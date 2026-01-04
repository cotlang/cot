//! Framework Module
//!
//! Exports project configuration, workspace management, and CLI commands.
//! This module is part of the cot package and can be imported by other
//! packages (like cot-dbl) via `cot.framework`.

const std = @import("std");

// Core configuration
pub const config = @import("config.zig");
pub const workspace = @import("workspace.zig");
pub const resolver = @import("resolver.zig");
pub const discovery = @import("discovery.zig");

// CLI commands
pub const commands = struct {
    pub const init = @import("commands/init.zig");
    pub const new = @import("commands/new.zig");
    pub const build = @import("commands/build.zig");
    pub const run = @import("commands/run.zig");
    pub const schema = @import("commands/schema.zig");
    pub const data = @import("commands/data.zig");
    pub const convert = @import("commands/convert.zig");
    pub const dev = @import("commands/dev.zig");
};

// Build system
pub const build_utils = struct {
    pub const bundler = @import("build/bundler.zig");
    pub const pipeline = @import("build/pipeline.zig");
};

// HTTP framework
pub const http = struct {
    pub const server = @import("http/server.zig");
    pub const router = @import("http/router.zig");
};

// Re-export commonly used types for convenience
pub const ConfigLoader = config.ConfigLoader;
pub const ProjectConfig = config.ProjectConfig;
pub const ProjectType = config.ProjectType;
