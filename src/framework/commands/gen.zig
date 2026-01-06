//! cot gen command
//!
//! Code generators for various project components.

const std = @import("std");
const Allocator = std.mem.Allocator;

pub const Generator = enum {
    dex,
};

pub const GenOptions = struct {
    generator: ?Generator = null,
    name: ?[]const u8 = null,
};

/// Run the generator
pub fn run(allocator: Allocator, options: GenOptions) !void {
    var stdout_buffer: [4096]u8 = undefined;
    var stdout_file = std.fs.File.stdout();
    var stdout_writer = stdout_file.writer(&stdout_buffer);
    const stdout = &stdout_writer.interface;

    const generator = options.generator orelse {
        try stdout.print("Error: Please specify a generator.\n", .{});
        try stdout.print("Usage: cot gen <generator> <name>\n", .{});
        try stdout.print("\nAvailable generators:\n", .{});
        try stdout.print("  dex     Generate a Dex component\n", .{});
        try stdout.flush();
        return error.MissingGenerator;
    };

    switch (generator) {
        .dex => try generateDexComponent(allocator, options.name, stdout),
    }
}

/// Generate a Dex component
fn generateDexComponent(allocator: Allocator, name_opt: ?[]const u8, stdout: anytype) !void {
    _ = allocator;

    const name = name_opt orelse {
        try stdout.print("Error: Please provide a component name.\n", .{});
        try stdout.print("Usage: cot gen dex <ComponentName>\n", .{});
        try stdout.flush();
        return error.MissingName;
    };

    // Validate component name starts with uppercase
    if (name.len == 0 or !std.ascii.isUpper(name[0])) {
        try stdout.print("Error: Component name must start with an uppercase letter.\n", .{});
        try stdout.print("Example: cot gen dex Counter\n", .{});
        try stdout.flush();
        return error.InvalidName;
    }

    // Create file in components directory
    const cwd = std.fs.cwd();

    // Try to create components directory
    cwd.makeDir("components") catch |err| {
        if (err != error.PathAlreadyExists) return err;
    };

    // Build filename: Counter.dex
    var filename_buf: [256]u8 = undefined;
    const filename = std.fmt.bufPrint(&filename_buf, "components/{s}.dex", .{name}) catch {
        try stdout.print("Error: Component name too long.\n", .{});
        try stdout.flush();
        return error.NameTooLong;
    };

    // Check if file already exists
    if (cwd.openFile(filename, .{})) |file| {
        file.close();
        try stdout.print("Error: Component already exists: {s}\n", .{filename});
        try stdout.flush();
        return error.FileExists;
    } else |_| {}

    // Generate component content
    var content_buf: [2048]u8 = undefined;
    const content = std.fmt.bufPrint(&content_buf,
        \\component {s} {{
        \\    // Component state
        \\    state {{
        \\        count: int = 0
        \\    }}
        \\
        \\    // Event handlers
        \\    handlers {{
        \\        fn increment(self) {{
        \\            self.count += 1
        \\        }}
        \\    }}
        \\
        \\    // Template
        \\    template {{
        \\        <div class="component">
        \\            <h2>{s}</h2>
        \\            <p>Count: {{self.count}}</p>
        \\            <button @click="increment">Increment</button>
        \\        </div>
        \\    }}
        \\}}
        \\
    , .{ name, name }) catch {
        try stdout.print("Error: Component name too long.\n", .{});
        try stdout.flush();
        return error.NameTooLong;
    };

    // Write the file
    const file = cwd.createFile(filename, .{}) catch |err| {
        try stdout.print("Error: Could not create file: {}\n", .{err});
        try stdout.flush();
        return err;
    };
    defer file.close();

    _ = file.writeAll(content) catch |err| {
        try stdout.print("Error: Could not write file: {}\n", .{err});
        try stdout.flush();
        return err;
    };

    try stdout.print("\n", .{});
    try stdout.print("  Created Dex component: {s}\n", .{name});
    try stdout.print("  Location: {s}\n", .{filename});
    try stdout.print("\n", .{});
    try stdout.print("  Next steps:\n", .{});
    try stdout.print("    1. Edit {s} to customize your component\n", .{filename});
    try stdout.print("    2. Import it in your page: <{s} />\n", .{name});
    try stdout.print("    3. Run: cot dev\n", .{});
    try stdout.print("\n", .{});
    try stdout.flush();
}

/// Parse gen command arguments
pub fn parseArgs(args: []const []const u8) GenOptions {
    var options = GenOptions{};
    var i: usize = 0;

    while (i < args.len) : (i += 1) {
        const arg = args[i];
        if (std.mem.startsWith(u8, arg, "-")) {
            // Skip flags for now
            continue;
        }

        // First positional arg is generator type
        if (options.generator == null) {
            if (std.mem.eql(u8, arg, "dex")) {
                options.generator = .dex;
            }
        } else if (options.name == null) {
            // Second positional arg is the name
            options.name = arg;
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
        \\Usage: cot gen <generator> <name> [options]
        \\
        \\Generate code for various project components.
        \\
        \\Generators:
        \\  dex               Generate a Dex component
        \\
        \\Arguments:
        \\  name              Name of the component to generate
        \\
        \\Options:
        \\  --help            Show this help
        \\
        \\Examples:
        \\  cot gen dex Counter          Create a Counter.dex component
        \\  cot gen dex UserProfile      Create a UserProfile.dex component
        \\
    );
    try stdout.flush();
}
