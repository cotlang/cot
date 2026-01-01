//! cot convert command
//!
//! Converts DBL source files (.dbl, .dbo) to modern Cot syntax (.cot).
//! Uses the shared AST infrastructure to parse DBL and emit Cot.
//!
//! Usage:
//!   cot convert <file.dbl>                 Convert to <file.cot>
//!   cot convert <file.dbl> -o <output>     Convert to specific output file
//!   cot convert <directory> --recursive    Convert all DBL files in directory
//!   cot convert <file.dbl> --dry-run       Preview output without writing
//!   cot convert <file.dbl> --style=dbl     Keep DBL-style operators

const std = @import("std");
const cot = @import("cot");
const ast = cot.ast;
const base = cot.base;
const Allocator = std.mem.Allocator;

pub const ConvertCommandOptions = struct {
    input_path: ?[]const u8 = null,
    output_path: ?[]const u8 = null,
    recursive: bool = false,
    dry_run: bool = false,
    style: Style = .modern,
    verbose: bool = false,
    show_help: bool = false,

    pub const Style = enum {
        modern, // Use modern operators (==, &&, etc.)
        dbl, // Keep DBL-style operators (.EQ., .AND., etc.)
    };
};

pub const ConvertError = error{
    NoInputFile,
    InputNotFound,
    NotDblFile,
    ParseFailed,
    OutputFailed,
    OutOfMemory,
};

/// Run the convert command
pub fn run(allocator: Allocator, options: ConvertCommandOptions) !void {
    var stdout_buffer: [4096]u8 = undefined;
    var stdout_file = std.fs.File.stdout();
    var stdout_writer = stdout_file.writer(&stdout_buffer);
    const stdout = &stdout_writer.interface;

    var stderr_buffer: [4096]u8 = undefined;
    var stderr_file = std.fs.File.stderr();
    var stderr_writer = stderr_file.writer(&stderr_buffer);
    const stderr = &stderr_writer.interface;

    if (options.show_help) {
        try printHelp(stdout);
        try stdout.flush();
        return;
    }

    const input_path = options.input_path orelse {
        try stderr.print("Error: No input file specified\n", .{});
        try stderr.print("Usage: cot convert <file.dbl> [-o output.cot]\n", .{});
        try stderr.flush();
        return ConvertError.NoInputFile;
    };

    // Check if input is a directory
    const stat = std.fs.cwd().statFile(input_path) catch |err| {
        try stderr.print("Error: Cannot access '{s}': {}\n", .{ input_path, err });
        try stderr.flush();
        return ConvertError.InputNotFound;
    };

    if (stat.kind == .directory) {
        if (options.recursive) {
            try convertDirectory(allocator, input_path, options, stdout, stderr);
        } else {
            try stderr.print("Error: '{s}' is a directory. Use --recursive to convert all files.\n", .{input_path});
            try stderr.flush();
            return ConvertError.NotDblFile;
        }
    } else {
        try convertSingleFile(allocator, input_path, options, stdout, stderr);
    }

    try stdout.flush();
    try stderr.flush();
}

fn convertSingleFile(
    allocator: Allocator,
    input_path: []const u8,
    options: ConvertCommandOptions,
    stdout: anytype,
    stderr: anytype,
) !void {
    // Check file extension
    const is_dbl = std.mem.endsWith(u8, input_path, ".dbl") or
        std.mem.endsWith(u8, input_path, ".dbo");

    if (!is_dbl) {
        try stderr.print("Error: '{s}' is not a DBL file (.dbl or .dbo)\n", .{input_path});
        try stderr.flush();
        return ConvertError.NotDblFile;
    }

    // Determine output path
    const output_path = options.output_path orelse blk: {
        // Replace .dbl/.dbo with .cot
        const ext_start = std.mem.lastIndexOf(u8, input_path, ".") orelse input_path.len;
        const base_name = input_path[0..ext_start];
        break :blk try std.fmt.allocPrint(allocator, "{s}.cot", .{base_name});
    };
    defer if (options.output_path == null) allocator.free(output_path);

    if (options.verbose) {
        try stdout.print("Converting: {s}\n", .{input_path});
        try stdout.print("Output: {s}\n", .{output_path});
        try stdout.flush();
    }

    // Read source file
    const source = std.fs.cwd().readFileAlloc(allocator, input_path, 10 * 1024 * 1024) catch |err| {
        try stderr.print("Error: Cannot read '{s}': {}\n", .{ input_path, err });
        try stderr.flush();
        return ConvertError.InputNotFound;
    };
    defer allocator.free(source);

    // Convert
    const output = convertSource(allocator, source, options) catch |err| {
        try stderr.print("Error: Conversion failed: {}\n", .{err});
        try stderr.flush();
        return ConvertError.ParseFailed;
    };
    defer allocator.free(output);

    if (options.dry_run) {
        // Print to stdout
        try stdout.print("{s}", .{output});
        try stdout.flush();
    } else {
        // Write to file
        std.fs.cwd().writeFile(.{
            .sub_path = output_path,
            .data = output,
        }) catch |err| {
            try stderr.print("Error: Cannot write '{s}': {}\n", .{ output_path, err });
            try stderr.flush();
            return ConvertError.OutputFailed;
        };

        try stdout.print("Converted: {s} → {s}\n", .{ input_path, output_path });
        try stdout.flush();
    }
}

fn convertDirectory(
    allocator: Allocator,
    dir_path: []const u8,
    options: ConvertCommandOptions,
    stdout: anytype,
    stderr: anytype,
) !void {
    var dir = std.fs.cwd().openDir(dir_path, .{ .iterate = true }) catch |err| {
        try stderr.print("Error: Cannot open directory '{s}': {}\n", .{ dir_path, err });
        try stderr.flush();
        return ConvertError.InputNotFound;
    };
    defer dir.close();

    var converted_count: u32 = 0;
    var error_count: u32 = 0;

    var iter = dir.iterate();
    while (iter.next() catch null) |entry| {
        if (entry.kind == .file) {
            const is_dbl = std.mem.endsWith(u8, entry.name, ".dbl") or
                std.mem.endsWith(u8, entry.name, ".dbo");

            if (is_dbl) {
                const full_path = try std.fmt.allocPrint(allocator, "{s}/{s}", .{ dir_path, entry.name });
                defer allocator.free(full_path);

                convertSingleFile(allocator, full_path, options, stdout, stderr) catch {
                    error_count += 1;
                    continue;
                };
                converted_count += 1;
            }
        } else if (entry.kind == .directory and options.recursive) {
            // Skip hidden directories
            if (entry.name[0] != '.') {
                const subdir_path = try std.fmt.allocPrint(allocator, "{s}/{s}", .{ dir_path, entry.name });
                defer allocator.free(subdir_path);

                convertDirectory(allocator, subdir_path, options, stdout, stderr) catch {
                    error_count += 1;
                };
            }
        }
    }

    if (options.verbose or converted_count > 0 or error_count > 0) {
        try stdout.print("\nConverted {d} file(s)", .{converted_count});
        if (error_count > 0) {
            try stdout.print(", {d} error(s)", .{error_count});
        }
        try stdout.print("\n", .{});
        try stdout.flush();
    }
}

/// Convert DBL source code to Cot source code
pub fn convertSource(allocator: Allocator, source: []const u8, options: ConvertCommandOptions) ![]u8 {
    // Initialize string interner
    var strings = base.StringInterner.init(allocator);
    defer strings.deinit();

    // Initialize node store
    var store = ast.NodeStore.init(allocator, &strings);
    defer store.deinit();

    // Try to use cot-dbl for parsing if available
    // For now, we'll use a simple pattern-based conversion as fallback
    // since cot-dbl is a separate module

    // Attempt to parse as DBL
    const stmts = parseDbl(allocator, source, &store, &strings) catch {
        // Fallback: do pattern-based conversion
        return patternBasedConvert(allocator, source, options);
    };
    defer allocator.free(stmts);

    // Emit as Cot
    const emitter_options = ast.Emitter.Options{
        .modern_operators = (options.style == .modern),
        .indent_size = 4,
        .blank_lines = true,
    };

    return ast.emitToString(allocator, &store, &strings, stmts, emitter_options);
}

/// Parse DBL source - this will be connected to cot-dbl
fn parseDbl(
    allocator: Allocator,
    source: []const u8,
    store: *ast.NodeStore,
    strings: *base.StringInterner,
) ![]const ast.StmtIdx {
    // This is a placeholder - in a full implementation, we would:
    // 1. Import cot-dbl as a dependency
    // 2. Use dbl.Lexer to tokenize
    // 3. Use dbl.Parser to parse into the shared AST
    //
    // For now, return error to trigger pattern-based fallback
    _ = allocator;
    _ = source;
    _ = store;
    _ = strings;
    return error.DblParserNotAvailable;
}

/// Simple pattern-based conversion for basic DBL → Cot
/// This handles common operator replacements without full parsing
fn patternBasedConvert(allocator: Allocator, source: []const u8, options: ConvertCommandOptions) ![]u8 {
    var result: std.ArrayListUnmanaged(u8) = .{};
    errdefer result.deinit(allocator);

    var i: usize = 0;
    while (i < source.len) {
        // Check for DBL operators and convert if using modern style
        if (options.style == .modern) {
            if (matchCaseInsensitive(source[i..], ".EQ.")) {
                try result.appendSlice(allocator, "==");
                i += 4;
                continue;
            } else if (matchCaseInsensitive(source[i..], ".NE.")) {
                try result.appendSlice(allocator, "!=");
                i += 4;
                continue;
            } else if (matchCaseInsensitive(source[i..], ".LT.")) {
                try result.appendSlice(allocator, "<");
                i += 4;
                continue;
            } else if (matchCaseInsensitive(source[i..], ".LE.")) {
                try result.appendSlice(allocator, "<=");
                i += 4;
                continue;
            } else if (matchCaseInsensitive(source[i..], ".GT.")) {
                try result.appendSlice(allocator, ">");
                i += 4;
                continue;
            } else if (matchCaseInsensitive(source[i..], ".GE.")) {
                try result.appendSlice(allocator, ">=");
                i += 4;
                continue;
            } else if (matchCaseInsensitive(source[i..], ".AND.")) {
                try result.appendSlice(allocator, "&&");
                i += 5;
                continue;
            } else if (matchCaseInsensitive(source[i..], ".OR.")) {
                try result.appendSlice(allocator, "||");
                i += 4;
                continue;
            } else if (matchCaseInsensitive(source[i..], ".NOT.")) {
                try result.appendSlice(allocator, "!");
                i += 5;
                continue;
            } else if (matchCaseInsensitive(source[i..], ".XOR.")) {
                try result.appendSlice(allocator, "^");
                i += 5;
                continue;
            } else if (matchCaseInsensitive(source[i..], "XCALL ")) {
                // Remove XCALL prefix (just call the function directly)
                i += 6;
                continue;
            } else if (matchCaseInsensitive(source[i..], "XRETURN")) {
                try result.appendSlice(allocator, "return");
                i += 7;
                continue;
            } else if (matchCaseInsensitive(source[i..], "FRETURN")) {
                try result.appendSlice(allocator, "return");
                i += 7;
                continue;
            } else if (matchCaseInsensitive(source[i..], "EXITLOOP")) {
                try result.appendSlice(allocator, "break");
                i += 8;
                continue;
            } else if (matchCaseInsensitive(source[i..], "NEXTLOOP")) {
                try result.appendSlice(allocator, "continue");
                i += 8;
                continue;
            }
        }

        // Copy character as-is
        try result.append(allocator, source[i]);
        i += 1;
    }

    return result.toOwnedSlice(allocator);
}

fn matchCaseInsensitive(haystack: []const u8, needle: []const u8) bool {
    if (haystack.len < needle.len) return false;

    for (needle, 0..) |c, idx| {
        const h = haystack[idx];
        const n = c;

        // Case-insensitive comparison for letters
        const h_lower = if (h >= 'A' and h <= 'Z') h + 32 else h;
        const n_lower = if (n >= 'A' and n <= 'Z') n + 32 else n;

        if (h_lower != n_lower) return false;
    }

    return true;
}

/// Parse command arguments
pub fn parseArgs(args: []const []const u8) ConvertCommandOptions {
    var options = ConvertCommandOptions{};
    var i: usize = 0;

    while (i < args.len) : (i += 1) {
        const arg = args[i];

        if (std.mem.eql(u8, arg, "--help") or std.mem.eql(u8, arg, "-h")) {
            options.show_help = true;
        } else if (std.mem.eql(u8, arg, "--verbose") or std.mem.eql(u8, arg, "-v")) {
            options.verbose = true;
        } else if (std.mem.eql(u8, arg, "--recursive") or std.mem.eql(u8, arg, "-r")) {
            options.recursive = true;
        } else if (std.mem.eql(u8, arg, "--dry-run") or std.mem.eql(u8, arg, "-n")) {
            options.dry_run = true;
        } else if (std.mem.eql(u8, arg, "--style=modern")) {
            options.style = .modern;
        } else if (std.mem.eql(u8, arg, "--style=dbl")) {
            options.style = .dbl;
        } else if (std.mem.eql(u8, arg, "-o") or std.mem.eql(u8, arg, "--output")) {
            if (i + 1 < args.len) {
                i += 1;
                options.output_path = args[i];
            }
        } else if (!std.mem.startsWith(u8, arg, "-")) {
            // Positional argument - input file
            if (options.input_path == null) {
                options.input_path = arg;
            }
        }
    }

    return options;
}

fn printHelp(stdout: anytype) !void {
    try stdout.print(
        \\Cot Convert - DBL to Cot Transpiler
        \\
        \\USAGE:
        \\    cot convert <file.dbl> [options]
        \\    cot convert <directory> --recursive [options]
        \\
        \\DESCRIPTION:
        \\    Converts DBL source files (.dbl, .dbo) to modern Cot syntax (.cot).
        \\    The conversion preserves semantics while modernizing the syntax.
        \\
        \\OPTIONS:
        \\    -o, --output <file>    Output file path (default: same name with .cot)
        \\    -r, --recursive        Convert all DBL files in directory
        \\    -n, --dry-run          Preview output without writing files
        \\    -v, --verbose          Show detailed progress
        \\    --style=modern         Use modern operators (==, &&, etc.) [default]
        \\    --style=dbl            Keep DBL-style operators (.EQ., .AND., etc.)
        \\    -h, --help             Show this help
        \\
        \\EXAMPLES:
        \\    cot convert program.dbl                  Convert single file
        \\    cot convert program.dbl -o output.cot   Convert with custom output
        \\    cot convert apps/ --recursive           Convert all DBL files
        \\    cot convert program.dbl --dry-run       Preview conversion
        \\    cot convert program.dbl --style=dbl     Keep DBL operators
        \\
        \\TRANSFORMATIONS:
        \\    .EQ.  →  ==        XCALL name()  →  name()
        \\    .NE.  →  !=        XRETURN       →  return
        \\    .LT.  →  <         FRETURN       →  return
        \\    .LE.  →  <=        EXITLOOP      →  break
        \\    .GT.  →  >         NEXTLOOP      →  continue
        \\    .GE.  →  >=
        \\    .AND. →  &&
        \\    .OR.  →  ||
        \\    .NOT. →  !
        \\
    , .{});
}

// ============================================================================
// Tests
// ============================================================================

test "pattern convert operators" {
    const allocator = std.testing.allocator;

    const input = "if (x .EQ. 0 .AND. y .GT. 10)";
    const output = try patternBasedConvert(allocator, input, .{ .style = .modern });
    defer allocator.free(output);

    try std.testing.expectEqualStrings("if (x == 0 && y > 10)", output);
}

test "pattern convert xcall" {
    const allocator = std.testing.allocator;

    const input = "XCALL my_function(arg1, arg2)";
    const output = try patternBasedConvert(allocator, input, .{ .style = .modern });
    defer allocator.free(output);

    try std.testing.expectEqualStrings("my_function(arg1, arg2)", output);
}

test "pattern convert preserves dbl style" {
    const allocator = std.testing.allocator;

    const input = "if (x .EQ. 0)";
    const output = try patternBasedConvert(allocator, input, .{ .style = .dbl });
    defer allocator.free(output);

    try std.testing.expectEqualStrings("if (x .EQ. 0)", output);
}

test "parse args" {
    const args = [_][]const u8{ "input.dbl", "-o", "output.cot", "--verbose", "--style=modern" };
    const options = parseArgs(&args);

    try std.testing.expectEqualStrings("input.dbl", options.input_path.?);
    try std.testing.expectEqualStrings("output.cot", options.output_path.?);
    try std.testing.expect(options.verbose);
    try std.testing.expectEqual(ConvertCommandOptions.Style.modern, options.style);
}
