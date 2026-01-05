//! Generate native function return type mappings from natives.toml
//!
//! This generates Zig source code for:
//! - FunctionReturnTypes: Maps qualified function names to IR types
//! - PreludeFunctions: List of functions available without import in .cot files
//!
//! Usage:
//!     zig build gen-natives
//!     # or directly:
//!     zig run tools/codegen/gen_natives.zig -- spec/natives.toml src/ir/generated/native_types.zig

const std = @import("std");

const Function = struct {
    name: []const u8,
    namespace: []const u8,
    returns: []const u8,
    is_prelude: bool,
    native_name: ?[]const u8,
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    const input_path = if (args.len > 1) args[1] else "spec/natives.toml";
    const output_path = if (args.len > 2) args[2] else "src/ir/generated/native_types.zig";

    // Read input file
    const input = std.fs.cwd().readFileAlloc(allocator, input_path, 1024 * 1024) catch |err| {
        std.debug.print("Error reading {s}: {}\n", .{ input_path, err });
        return err;
    };
    defer allocator.free(input);

    // Parse functions from TOML
    var functions: std.ArrayListUnmanaged(Function) = .empty;
    defer {
        for (functions.items) |f| {
            allocator.free(f.name);
            allocator.free(f.namespace);
            allocator.free(f.returns);
            if (f.native_name) |n| allocator.free(n);
        }
        functions.deinit(allocator);
    }

    try parseToml(allocator, input, &functions);

    // Generate output
    var output: std.ArrayListUnmanaged(u8) = .empty;
    defer output.deinit(allocator);

    try generateZig(allocator, functions.items, output.writer(allocator));

    // Ensure output directory exists
    const dir_path = std.fs.path.dirname(output_path) orelse ".";
    std.fs.cwd().makePath(dir_path) catch {};

    // Write output file
    const file = std.fs.cwd().createFile(output_path, .{}) catch |err| {
        std.debug.print("Error creating {s}: {}\n", .{ output_path, err });
        return err;
    };
    defer file.close();

    try file.writeAll(output.items);

    std.debug.print("Generated {s}\n", .{output_path});
    std.debug.print("  - {d} functions processed\n", .{functions.items.len});
}

fn parseToml(allocator: std.mem.Allocator, input: []const u8, functions: *std.ArrayListUnmanaged(Function)) !void {
    var current_func: ?[]const u8 = null;
    var current_namespace: []const u8 = "";
    var current_returns: []const u8 = "void";
    var current_prelude: bool = false;
    var current_native_name: ?[]const u8 = null;
    var in_signature: bool = false;

    var lines = std.mem.splitScalar(u8, input, '\n');
    while (lines.next()) |line| {
        const trimmed = std.mem.trim(u8, line, " \t\r");

        // Skip empty lines and comments
        if (trimmed.len == 0 or trimmed[0] == '#') continue;

        // Section header: [functions.name] or [functions.name.signature]
        if (trimmed[0] == '[' and trimmed[trimmed.len - 1] == ']') {
            const header = trimmed[1 .. trimmed.len - 1];

            if (std.mem.startsWith(u8, header, "functions.")) {
                const rest = header["functions.".len..];

                if (std.mem.endsWith(u8, rest, ".signature")) {
                    in_signature = true;
                } else if (std.mem.endsWith(u8, rest, ".behavior") or
                    std.mem.endsWith(u8, rest, ".tests"))
                {
                    in_signature = false;
                } else if (std.mem.indexOf(u8, rest, ".") == null) {
                    // New function - save previous if exists
                    if (current_func) |func| {
                        try functions.append(allocator, .{
                            .name = try allocator.dupe(u8, func),
                            .namespace = try allocator.dupe(u8, current_namespace),
                            .returns = try allocator.dupe(u8, current_returns),
                            .is_prelude = current_prelude,
                            .native_name = if (current_native_name) |n| try allocator.dupe(u8, n) else null,
                        });
                    }

                    current_func = rest;
                    current_namespace = "";
                    current_returns = "void";
                    current_prelude = false;
                    current_native_name = null;
                    in_signature = false;
                }
            }
            continue;
        }

        // Key-value pairs
        if (std.mem.indexOf(u8, trimmed, "=")) |eq_pos| {
            const key = std.mem.trim(u8, trimmed[0..eq_pos], " \t");
            const value = std.mem.trim(u8, trimmed[eq_pos + 1 ..], " \t");

            if (std.mem.eql(u8, key, "namespace")) {
                current_namespace = parseStringValue(value);
            } else if (std.mem.eql(u8, key, "prelude")) {
                current_prelude = std.mem.eql(u8, value, "true");
            } else if (std.mem.eql(u8, key, "native_name")) {
                current_native_name = parseStringValue(value);
            } else if (std.mem.eql(u8, key, "returns") and in_signature) {
                current_returns = parseReturnType(value);
            }
        }
    }

    // Save last function
    if (current_func) |func| {
        try functions.append(allocator, .{
            .name = try allocator.dupe(u8, func),
            .namespace = try allocator.dupe(u8, current_namespace),
            .returns = try allocator.dupe(u8, current_returns),
            .is_prelude = current_prelude,
            .native_name = if (current_native_name) |n| try allocator.dupe(u8, n) else null,
        });
    }
}

fn parseStringValue(value: []const u8) []const u8 {
    if (value.len >= 2 and value[0] == '"' and value[value.len - 1] == '"') {
        return value[1 .. value.len - 1];
    }
    return value;
}

fn parseReturnType(value: []const u8) []const u8 {
    const unquoted = parseStringValue(value);

    // Handle simple types
    if (std.mem.eql(u8, unquoted, "void")) return "void";
    if (std.mem.eql(u8, unquoted, "i64")) return "i64";
    if (std.mem.eql(u8, unquoted, "f64")) return "f64";
    if (std.mem.eql(u8, unquoted, "bool")) return "bool";
    if (std.mem.eql(u8, unquoted, "string")) return "string";
    if (std.mem.eql(u8, unquoted, "alpha")) return "string";
    if (std.mem.eql(u8, unquoted, "handle")) return "i64";

    // Handle complex types like { type = "decimal", ... }
    if (std.mem.startsWith(u8, value, "{")) {
        if (std.mem.indexOf(u8, value, "\"decimal\"") != null) return "f64";
        if (std.mem.indexOf(u8, value, "\"optional\"") != null) {
            if (std.mem.indexOf(u8, value, "\"string\"") != null) return "string";
            return "any";
        }
    }

    return "any";
}

fn toIrType(returns: []const u8) []const u8 {
    if (std.mem.eql(u8, returns, "void")) return ".void";
    if (std.mem.eql(u8, returns, "i64")) return ".i64";
    if (std.mem.eql(u8, returns, "f64")) return ".f64";
    if (std.mem.eql(u8, returns, "bool")) return ".bool";
    if (std.mem.eql(u8, returns, "string")) return ".string";
    return ".any";
}

fn generateZig(allocator: std.mem.Allocator, functions: []const Function, writer: anytype) !void {
    // Collect entries for sorting
    const Entry = struct {
        qualified_name: []const u8,
        ir_type: []const u8,
    };

    var entries: std.ArrayListUnmanaged(Entry) = .empty;
    defer {
        for (entries.items) |e| allocator.free(e.qualified_name);
        entries.deinit(allocator);
    }

    var prelude_funcs: std.ArrayListUnmanaged([]const u8) = .empty;
    defer {
        for (prelude_funcs.items) |p| allocator.free(p);
        prelude_funcs.deinit(allocator);
    }

    for (functions) |func| {
        // Skip DBL-only functions
        if (std.mem.eql(u8, func.namespace, "dbl")) continue;

        const ir_type = toIrType(func.returns);

        // Build qualified name
        var qualified_buf: [256]u8 = undefined;
        const qualified_name = if (func.namespace.len > 0)
            std.fmt.bufPrint(&qualified_buf, "{s}.{s}", .{ func.namespace, func.name }) catch continue
        else
            func.name;

        try entries.append(allocator, .{
            .qualified_name = try allocator.dupe(u8, qualified_name),
            .ir_type = ir_type,
        });

        // Add short name for prelude functions
        if (func.is_prelude) {
            try entries.append(allocator, .{
                .qualified_name = try allocator.dupe(u8, func.name),
                .ir_type = ir_type,
            });
            try prelude_funcs.append(allocator, try allocator.dupe(u8, func.name));
        }
    }

    // Sort entries
    std.mem.sort(Entry, entries.items, {}, struct {
        fn lessThan(_: void, a: Entry, b: Entry) bool {
            return std.mem.order(u8, a.qualified_name, b.qualified_name) == .lt;
        }
    }.lessThan);

    std.mem.sort([]const u8, prelude_funcs.items, {}, struct {
        fn lessThan(_: void, a: []const u8, b: []const u8) bool {
            return std.mem.order(u8, a, b) == .lt;
        }
    }.lessThan);

    // Write header
    try writer.writeAll(
        \\//! Auto-generated from spec/natives.toml
        \\//! DO NOT EDIT MANUALLY - regenerate with: zig build gen-natives
        \\//!
        \\//! This file contains compile-time mappings for native function return types.
        \\
        \\const std = @import("std");
        \\const ir = @import("../ir.zig");
        \\
        \\/// Maps qualified function names to their IR return types.
        \\/// Used by the lowerer to determine return types for native function calls.
        \\pub const FunctionReturnTypes = std.StaticStringMap(ir.Type).initComptime(.{
        \\
    );

    // Write entries
    for (entries.items) |entry| {
        try writer.print("    .{{ \"{s}\", {s} }},\n", .{ entry.qualified_name, entry.ir_type });
    }

    try writer.writeAll(
        \\});
        \\
        \\/// Functions available in prelude (no import required in .cot files).
        \\pub const PreludeFunctions = [_][]const u8{
        \\
    );

    for (prelude_funcs.items) |name| {
        try writer.print("    \"{s}\",\n", .{name});
    }

    try writer.writeAll(
        \\};
        \\
        \\/// Get the return type for a function name.
        \\/// Returns null if the function is not a known native.
        \\pub fn getReturnType(name: []const u8) ?ir.Type {
        \\    return FunctionReturnTypes.get(name);
        \\}
        \\
        \\/// Check if a function is in the prelude.
        \\pub fn isPreludeFunction(name: []const u8) bool {
        \\    for (PreludeFunctions) |f| {
        \\        if (std.mem.eql(u8, f, name)) return true;
        \\    }
        \\    return false;
        \\}
        \\
        \\test "known functions" {
        \\    try std.testing.expectEqual(ir.Type.void, getReturnType("std.io.print").?);
        \\    try std.testing.expectEqual(ir.Type.void, getReturnType("print").?);
        \\    try std.testing.expectEqual(ir.Type.i64, getReturnType("std.math.abs").?);
        \\    try std.testing.expect(getReturnType("unknown_function") == null);
        \\}
        \\
        \\test "prelude functions" {
        \\    try std.testing.expect(isPreludeFunction("print"));
        \\    try std.testing.expect(isPreludeFunction("println"));
        \\    try std.testing.expect(isPreludeFunction("assert"));
        \\    try std.testing.expect(!isPreludeFunction("abs"));
        \\}
        \\
    );
}
