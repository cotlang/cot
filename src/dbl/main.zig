//! Cot-DBL Compiler
//!
//! A DBL syntax frontend for the Cot language.
//! Compiles .dbl files using DBL syntax through the standard Cot pipeline.

const std = @import("std");
const cot = @import("cot");
const cot_tui = @import("cot_tui");
const dbl = @import("root.zig");

pub fn main() !void {
    var gpa: std.heap.GeneralPurposeAllocator(.{}) = .init;
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len < 2) {
        printUsage();
        return;
    }

    var filename: ?[]const u8 = null;
    var output_path: ?[]const u8 = null;

    // Parse arguments
    var i: usize = 1;
    while (i < args.len) : (i += 1) {
        const arg = args[i];
        if (std.mem.eql(u8, arg, "--output") or std.mem.eql(u8, arg, "-o")) {
            if (i + 1 >= args.len) {
                std.debug.print("Error: --output requires a path argument\n", .{});
                return;
            }
            i += 1;
            output_path = args[i];
        } else if (std.mem.eql(u8, arg, "--help") or std.mem.eql(u8, arg, "-h")) {
            printUsage();
            return;
        } else if (arg[0] == '-') {
            std.debug.print("Error: Unknown option '{s}'\n", .{arg});
            return;
        } else {
            filename = arg;
        }
    }

    if (filename == null) {
        std.debug.print("Error: No input file specified\n", .{});
        printUsage();
        return;
    }

    // Read source file
    const file = std.fs.cwd().openFile(filename.?, .{}) catch |err| {
        std.debug.print("Error: Could not open file '{s}': {}\n", .{ filename.?, err });
        return;
    };
    defer file.close();

    const source = try file.readToEndAlloc(allocator, 1024 * 1024 * 10);
    defer allocator.free(source);

    // If output path specified, compile to bytecode file
    if (output_path) |out_path| {
        compileToFile(allocator, source, filename.?, out_path) catch |err| {
            std.debug.print("Error: Compilation failed: {}\n", .{err});
            std.process.exit(1);
        };
    } else {
        // Initialize extension registry and register TUI extension
        cot.extension.initRegistry(allocator);
        defer cot.extension.deinitRegistry();

        // Register TUI extension for t_* native functions
        cot.extension.registerExtension(cot_tui.extension) catch |err| {
            std.debug.print("Warning: Could not register TUI extension: {}\n", .{err});
        };

        // Compile and run with source file directory for import resolution
        const source_dir = std.fs.path.dirname(filename.?);
        dbl.runWithRegistryAndPath(allocator, source, source_dir) catch |err| {
            std.debug.print("Error: {}\n", .{err});
        };
    }
}

fn printUsage() void {
    std.debug.print(
        \\Cot-DBL: DBL syntax frontend for Cot
        \\
        \\Usage: cot-dbl <file.dbl> [options]
        \\
        \\Options:
        \\  -o, --output <path>  Compile to bytecode file instead of running
        \\  -h, --help           Show this help message
        \\
        \\Examples:
        \\  cot-dbl program.dbl              Run program.dbl
        \\  cot-dbl program.dbl -o out.cbo   Compile to out.cbo
        \\
    , .{});
}

fn compileToFile(allocator: std.mem.Allocator, source: []const u8, source_name: []const u8, output_path: []const u8) !void {
    // Extract module name from source filename
    const basename = std.fs.path.basename(source_name);
    const module_name = if (std.mem.lastIndexOf(u8, basename, ".")) |dot|
        basename[0..dot]
    else
        basename;

    // Compile to bytecode module
    var module = try dbl.compileToModule(allocator, source, module_name);
    defer module.deinit();

    // Create output file
    var out_file = std.fs.cwd().createFile(output_path, .{}) catch |err| {
        std.debug.print("Error: Could not create output file '{s}': {}\n", .{ output_path, err });
        return error.OutputError;
    };
    defer out_file.close();

    // Serialize bytecode to file
    var write_buffer: [8192]u8 = undefined;
    var buffered = out_file.writer(&write_buffer);

    try module.serialize(&buffered.interface);
    try buffered.interface.flush();

    std.debug.print("Compiled {s} -> {s}\n", .{ source_name, output_path });
}
