//! cot data command
//!
//! Load and unload fixed-width data files to/from database tables.
//! This enables migration of legacy ISAM data to SQL databases.
//!
//! Subcommands:
//!   cot data load <table> <file>     Load fixed-width file into table
//!   cot data unload <table> <file>   Export table to fixed-width file
//!   cot data info <table>            Show table layout and byte offsets

const std = @import("std");
const cot = @import("cot");
const config = @import("../config.zig");
const Allocator = std.mem.Allocator;

const schema = cot.schema;

/// Data command options
pub const DataOptions = struct {
    /// Subcommand to run
    subcommand: Subcommand = .help,
    /// Table name
    table: ?[]const u8 = null,
    /// Data file path
    file: ?[]const u8 = null,
    /// Database file path (for SQLite)
    database: ?[]const u8 = null,
    /// Skip header line in input file
    skip_header: bool = false,
    /// Verbose output
    verbose: bool = false,
    /// Dry run (don't actually write)
    dry_run: bool = false,
};

pub const Subcommand = enum {
    load,
    unload,
    info,
    help,
};

/// Parse command line arguments
pub fn parseArgs(args: []const []const u8) DataOptions {
    var options = DataOptions{};
    var positional_index: usize = 0;

    var i: usize = 0;
    while (i < args.len) : (i += 1) {
        const arg = args[i];

        if (std.mem.eql(u8, arg, "--skip-header")) {
            options.skip_header = true;
        } else if (std.mem.eql(u8, arg, "--dry-run")) {
            options.dry_run = true;
        } else if (std.mem.eql(u8, arg, "--verbose") or std.mem.eql(u8, arg, "-v")) {
            options.verbose = true;
        } else if (std.mem.eql(u8, arg, "--database") or std.mem.eql(u8, arg, "-d")) {
            if (i + 1 < args.len) {
                i += 1;
                options.database = args[i];
            }
        } else if (std.mem.eql(u8, arg, "load")) {
            options.subcommand = .load;
        } else if (std.mem.eql(u8, arg, "unload")) {
            options.subcommand = .unload;
        } else if (std.mem.eql(u8, arg, "info")) {
            options.subcommand = .info;
        } else if (std.mem.eql(u8, arg, "help") or std.mem.eql(u8, arg, "--help") or std.mem.eql(u8, arg, "-h")) {
            options.subcommand = .help;
        } else if (!std.mem.startsWith(u8, arg, "-")) {
            // Positional argument
            if (positional_index == 0) {
                options.table = arg;
            } else if (positional_index == 1) {
                options.file = arg;
            }
            positional_index += 1;
        }
    }

    return options;
}

/// Run the data command
pub fn run(allocator: Allocator, options: DataOptions) !void {
    switch (options.subcommand) {
        .load => try runLoad(allocator, options),
        .unload => try runUnload(allocator, options),
        .info => try runInfo(allocator, options),
        .help => try printHelp(),
    }
}

/// Load fixed-width file into database table
fn runLoad(allocator: Allocator, options: DataOptions) !void {
    var stdout_buffer: [4096]u8 = undefined;
    var stdout_file = std.fs.File.stdout();
    var stdout_writer = stdout_file.writer(&stdout_buffer);
    const stdout = &stdout_writer.interface;

    const table_name = options.table orelse {
        try stdout.print("Error: Table name required\n", .{});
        try stdout.print("Usage: cot data load <table> <file>\n", .{});
        try stdout_writer.interface.flush();
        return;
    };

    const file_path = options.file orelse {
        try stdout.print("Error: File path required\n", .{});
        try stdout.print("Usage: cot data load <table> <file>\n", .{});
        try stdout_writer.interface.flush();
        return;
    };

    // Load project config and schema
    const project_info = findProject(allocator, null) catch {
        try stdout.print("Error: No cot.json found in current directory\n", .{});
        try stdout_writer.interface.flush();
        return;
    };
    defer if (project_info.project_path) |p| allocator.free(p);

    const schema_config = project_info.config.schema orelse {
        try stdout.print("Error: No schema configured in cot.json\n", .{});
        try stdout_writer.interface.flush();
        return;
    };

    const schema_path = try std.fs.path.join(allocator, &.{
        project_info.project_path orelse ".",
        schema_config.file,
    });
    defer allocator.free(schema_path);

    var schema_file = schema.parseSchemaFile(allocator, schema_path) catch |err| {
        try stdout.print("Error loading schema: {}\n", .{err});
        try stdout_writer.interface.flush();
        return;
    };
    defer schema_file.deinit();

    // Find the table
    const table = findTable(&schema_file, table_name) orelse {
        try stdout.print("Error: Table '{s}' not found in schema\n", .{table_name});
        try stdout_writer.interface.flush();
        return;
    };

    // Calculate field offsets
    const layout = try calculateLayout(allocator, table);
    defer allocator.free(layout.offsets);

    try stdout.print("\n", .{});
    try stdout.print("  Loading Data\n", .{});
    try stdout.print("  ============\n", .{});
    try stdout.print("\n", .{});
    try stdout.print("  Table:       {s}\n", .{table_name});
    try stdout.print("  File:        {s}\n", .{file_path});
    try stdout.print("  Record size: {d} bytes\n", .{layout.record_size});
    try stdout.print("\n", .{});

    // Read entire input file
    const file_content = std.fs.cwd().readFileAlloc(allocator, file_path, 10 * 1024 * 1024) catch |err| {
        try stdout.print("Error reading file: {}\n", .{err});
        try stdout_writer.interface.flush();
        return;
    };
    defer allocator.free(file_content);

    // Parse lines
    var line_num: usize = 0;
    var loaded: usize = 0;
    var errors: usize = 0;

    // Collect INSERT statements
    var inserts: std.ArrayListAligned([]const u8, null) = .empty;
    defer {
        for (inserts.items) |item| allocator.free(item);
        inserts.deinit(allocator);
    }

    // Split by lines
    var lines = std.mem.splitScalar(u8, file_content, '\n');
    while (lines.next()) |line| {
        line_num += 1;

        // Skip header if requested
        if (options.skip_header and line_num == 1) continue;

        // Skip empty lines
        if (line.len == 0) continue;

        // Strip carriage return if present (Windows line endings)
        const clean_line = if (line.len > 0 and line[line.len - 1] == '\r')
            line[0 .. line.len - 1]
        else
            line;

        // Skip if line is now empty
        if (clean_line.len == 0) continue;

        // Validate line length
        if (clean_line.len < layout.record_size) {
            if (options.verbose) {
                try stdout.print("  Warning: Line {d} too short ({d} < {d}), padding\n", .{ line_num, clean_line.len, layout.record_size });
            }
        }

        // Parse fields from fixed-width line
        const insert_sql = generateInsertSql(allocator, table, layout, clean_line) catch |err| {
            try stdout.print("  Error on line {d}: {}\n", .{ line_num, err });
            errors += 1;
            continue;
        };

        if (options.verbose) {
            try stdout.print("  Line {d}: {s}\n", .{ line_num, insert_sql });
        }

        try inserts.append(allocator, insert_sql);
        loaded += 1;
    }

    try stdout.print("  Parsed {d} records ({d} errors)\n", .{ loaded, errors });

    if (options.dry_run) {
        try stdout.print("\n  Dry run - no data written\n", .{});
        try stdout.print("\n  SQL statements:\n", .{});
        for (inserts.items) |sql| {
            try stdout.print("    {s}\n", .{sql});
        }
    } else {
        // Write SQL to output file
        const output_path = try std.fmt.allocPrint(allocator, "{s}_load.sql", .{table_name});
        defer allocator.free(output_path);

        const output_file = try std.fs.cwd().createFile(output_path, .{});
        defer output_file.close();

        var write_buffer: [8192]u8 = undefined;
        var buffered = output_file.writer(&write_buffer);
        const writer = &buffered.interface;

        try writer.print("-- Load data for table: {s}\n", .{table_name});
        try writer.print("-- Source file: {s}\n", .{file_path});
        try writer.print("-- Records: {d}\n\n", .{loaded});

        try writer.print("BEGIN TRANSACTION;\n\n", .{});

        for (inserts.items) |sql| {
            try writer.print("{s}\n", .{sql});
        }

        try writer.print("\nCOMMIT;\n", .{});
        try buffered.interface.flush();

        try stdout.print("\n  Created: {s}\n", .{output_path});
        try stdout.print("\n  To apply, run:\n", .{});
        try stdout.print("    sqlite3 your_database.db < {s}\n", .{output_path});
    }

    try stdout.print("\n", .{});
    try stdout_writer.interface.flush();
}

/// Unload database table to fixed-width file
fn runUnload(allocator: Allocator, options: DataOptions) !void {
    var stdout_buffer: [4096]u8 = undefined;
    var stdout_file = std.fs.File.stdout();
    var stdout_writer = stdout_file.writer(&stdout_buffer);
    const stdout = &stdout_writer.interface;

    const table_name = options.table orelse {
        try stdout.print("Error: Table name required\n", .{});
        try stdout.print("Usage: cot data unload <table> <file>\n", .{});
        try stdout_writer.interface.flush();
        return;
    };

    const file_path = options.file orelse {
        try stdout.print("Error: File path required\n", .{});
        try stdout.print("Usage: cot data unload <table> <file>\n", .{});
        try stdout_writer.interface.flush();
        return;
    };

    // Load project config and schema
    const project_info = findProject(allocator, null) catch {
        try stdout.print("Error: No cot.json found in current directory\n", .{});
        try stdout_writer.interface.flush();
        return;
    };
    defer if (project_info.project_path) |p| allocator.free(p);

    const schema_config = project_info.config.schema orelse {
        try stdout.print("Error: No schema configured in cot.json\n", .{});
        try stdout_writer.interface.flush();
        return;
    };

    const schema_path = try std.fs.path.join(allocator, &.{
        project_info.project_path orelse ".",
        schema_config.file,
    });
    defer allocator.free(schema_path);

    var schema_file = schema.parseSchemaFile(allocator, schema_path) catch |err| {
        try stdout.print("Error loading schema: {}\n", .{err});
        try stdout_writer.interface.flush();
        return;
    };
    defer schema_file.deinit();

    // Find the table
    const table = findTable(&schema_file, table_name) orelse {
        try stdout.print("Error: Table '{s}' not found in schema\n", .{table_name});
        try stdout_writer.interface.flush();
        return;
    };

    // Calculate field offsets
    const layout = try calculateLayout(allocator, table);
    defer allocator.free(layout.offsets);

    try stdout.print("\n", .{});
    try stdout.print("  Unloading Data\n", .{});
    try stdout.print("  ==============\n", .{});
    try stdout.print("\n", .{});
    try stdout.print("  Table:       {s}\n", .{table_name});
    try stdout.print("  Output:      {s}\n", .{file_path});
    try stdout.print("  Record size: {d} bytes\n", .{layout.record_size});
    try stdout.print("\n", .{});

    // Generate SELECT SQL
    var select_sql: std.ArrayListAligned(u8, null) = .empty;
    defer select_sql.deinit(allocator);

    try select_sql.appendSlice(allocator, "SELECT ");
    for (table.fields, 0..) |field, i| {
        if (i > 0) try select_sql.appendSlice(allocator, ", ");
        try select_sql.appendSlice(allocator, field.name);
    }
    try select_sql.appendSlice(allocator, " FROM ");
    try select_sql.appendSlice(allocator, table_name);
    try select_sql.appendSlice(allocator, " ORDER BY ");

    // Order by primary key or first field
    var pk_field: []const u8 = table.fields[0].name;
    for (table.fields) |field| {
        if (field.decorators.primary) {
            pk_field = field.name;
            break;
        }
    }
    try select_sql.appendSlice(allocator, pk_field);
    try select_sql.appendSlice(allocator, ";");

    try stdout.print("  Query: {s}\n", .{select_sql.items});
    try stdout.print("\n", .{});
    try stdout.print("  To unload data:\n", .{});
    try stdout.print("  1. Run the query against your database\n", .{});
    try stdout.print("  2. Format each row as fixed-width using the layout below\n", .{});
    try stdout.print("  3. Write to {s}\n", .{file_path});
    try stdout.print("\n", .{});

    // Show layout
    try stdout.print("  Field Layout:\n", .{});
    try stdout.print("  {s: <20} {s: >8} {s: >8} {s: >8}\n", .{ "Field", "Offset", "Size", "Type" });
    try stdout.print("  {s:-<20} {s:->8} {s:->8} {s:->8}\n", .{ "", "", "", "" });

    for (table.fields, 0..) |field, i| {
        const type_str = fieldTypeString(field.data_type);
        try stdout.print("  {s: <20} {d: >8} {d: >8} {s: >8}\n", .{
            field.name,
            layout.offsets[i],
            field.byteSize(),
            type_str,
        });
    }

    // Generate a shell script helper
    const script_path = try std.fmt.allocPrint(allocator, "{s}_unload.sh", .{table_name});
    defer allocator.free(script_path);

    if (!options.dry_run) {
        const script_file = try std.fs.cwd().createFile(script_path, .{});
        defer script_file.close();

        var write_buffer: [4096]u8 = undefined;
        var buffered = script_file.writer(&write_buffer);
        const writer = &buffered.interface;

        try writer.print("#!/bin/bash\n", .{});
        try writer.print("# Unload {s} table to fixed-width file\n", .{table_name});
        try writer.print("# Generated by: cot data unload\n\n", .{});
        try writer.print("DB=\"$1\"\n", .{});
        try writer.print("if [ -z \"$DB\" ]; then\n", .{});
        try writer.print("    echo \"Usage: {s} <database.db>\"\n", .{script_path});
        try writer.print("    exit 1\n", .{});
        try writer.print("fi\n\n", .{});

        // Build printf format string
        try writer.print("sqlite3 -separator '' \"$DB\" \\\n", .{});
        try writer.print("    \"SELECT ", .{});

        for (table.fields, 0..) |field, i| {
            if (i > 0) try writer.print(" || ", .{});
            const size = field.byteSize();
            switch (field.data_type) {
                .text, .ulid, .reference => {
                    try writer.print("printf('%{d}s', COALESCE({s}, ''))", .{ size, field.name });
                },
                .integer => {
                    try writer.print("printf('%0{d}d', COALESCE({s}, 0))", .{ size, field.name });
                },
                .decimal => |d| {
                    // Format with fixed precision
                    try writer.print("printf('%0{d}d', CAST(COALESCE({s}, 0) * {d} AS INTEGER))", .{
                        size,
                        field.name,
                        std.math.pow(u64, 10, d.scale),
                    });
                },
                else => {
                    try writer.print("COALESCE({s}, '')", .{field.name});
                },
            }
        }

        try writer.print(" FROM {s}\" > {s}\n\n", .{ table_name, file_path });
        try writer.print("echo \"Exported to {s}\"\n", .{file_path});
        try buffered.interface.flush();

        try stdout.print("\n  Created: {s}\n", .{script_path});
        try stdout.print("  Run: chmod +x {s} && ./{s} your_database.db\n", .{ script_path, script_path });
    }

    try stdout.print("\n", .{});
    try stdout_writer.interface.flush();
}

/// Show table info and byte layout
fn runInfo(allocator: Allocator, options: DataOptions) !void {
    var stdout_buffer: [4096]u8 = undefined;
    var stdout_file = std.fs.File.stdout();
    var stdout_writer = stdout_file.writer(&stdout_buffer);
    const stdout = &stdout_writer.interface;

    const table_name = options.table orelse {
        try stdout.print("Error: Table name required\n", .{});
        try stdout.print("Usage: cot data info <table>\n", .{});
        try stdout_writer.interface.flush();
        return;
    };

    // Load project config and schema
    const project_info = findProject(allocator, null) catch {
        try stdout.print("Error: No cot.json found in current directory\n", .{});
        try stdout_writer.interface.flush();
        return;
    };
    defer if (project_info.project_path) |p| allocator.free(p);

    const schema_config = project_info.config.schema orelse {
        try stdout.print("Error: No schema configured in cot.json\n", .{});
        try stdout_writer.interface.flush();
        return;
    };

    const schema_path = try std.fs.path.join(allocator, &.{
        project_info.project_path orelse ".",
        schema_config.file,
    });
    defer allocator.free(schema_path);

    var schema_file = schema.parseSchemaFile(allocator, schema_path) catch |err| {
        try stdout.print("Error loading schema: {}\n", .{err});
        try stdout_writer.interface.flush();
        return;
    };
    defer schema_file.deinit();

    // Find the table
    const table = findTable(&schema_file, table_name) orelse {
        try stdout.print("Error: Table '{s}' not found in schema\n", .{table_name});
        try stdout.print("\nAvailable tables:\n", .{});
        for (schema_file.tables) |t| {
            try stdout.print("  - {s}\n", .{t.name});
        }
        try stdout_writer.interface.flush();
        return;
    };

    // Calculate field offsets
    const layout = try calculateLayout(allocator, table);
    defer allocator.free(layout.offsets);

    try stdout.print("\n", .{});
    try stdout.print("  Table Layout: {s}\n", .{table_name});
    try stdout.writeAll("  ==============");
    for (table_name) |_| {
        try stdout.writeAll("=");
    }
    try stdout.writeAll("\n\n");
    try stdout.print("  Total record size: {d} bytes\n", .{layout.record_size});
    try stdout.print("  Number of fields:  {d}\n", .{table.fields.len});
    try stdout.print("\n", .{});

    // Header
    try stdout.print("  {s: <20} {s: >8} {s: >8} {s: >12} {s}\n", .{ "Field", "Offset", "Size", "Type", "Decorators" });
    try stdout.print("  {s:-<20} {s:->8} {s:->8} {s:->12} {s:-<20}\n", .{ "", "", "", "", "" });

    // Fields
    for (table.fields, 0..) |field, i| {
        const type_str = fieldTypeString(field.data_type);
        var dec_buf: [128]u8 = undefined;
        const decorators = formatDecorators(field.decorators, &dec_buf);

        try stdout.print("  {s: <20} {d: >8} {d: >8} {s: >12} {s}\n", .{
            field.name,
            layout.offsets[i],
            field.byteSize(),
            type_str,
            decorators,
        });
    }

    // Visual layout (only if record size > 0)
    if (layout.record_size > 0) {
        try stdout.print("\n", .{});
        try stdout.print("  Fixed-Width Record Layout:\n", .{});
        try stdout.print("  ", .{});
        var pos: usize = 0;
        for (table.fields) |field| {
            const size = field.byteSize();
            if (size == 0) continue;
            try stdout.writeAll("|");
            // Print field name truncated/padded to size
            const name_len = @min(field.name.len, size);
            try stdout.print("{s}", .{field.name[0..name_len]});
            // Pad remaining
            if (size > name_len) {
                var pad: usize = 0;
                while (pad < size - name_len) : (pad += 1) {
                    try stdout.writeAll(" ");
                }
            }
            pos += size;
        }
        try stdout.writeAll("|\n");

        // Offset ruler
        try stdout.writeAll("  ");
        pos = 0;
        for (table.fields) |field| {
            const size = field.byteSize();
            if (size == 0) continue;
            try stdout.print("+{d}", .{pos});
            // Pad with dashes
            var num_buf: [16]u8 = undefined;
            const num_str = std.fmt.bufPrint(&num_buf, "{d}", .{pos}) catch "";
            if (size > num_str.len) {
                var pad: usize = num_str.len;
                while (pad < size) : (pad += 1) {
                    try stdout.writeAll("-");
                }
            }
            pos += size;
        }
        try stdout.print("+{d}\n", .{pos});
    }

    try stdout.print("\n", .{});
    try stdout_writer.interface.flush();
}

/// Print help
pub fn printHelp() !void {
    var stdout_buffer: [4096]u8 = undefined;
    var stdout_file = std.fs.File.stdout();
    var stdout_writer = stdout_file.writer(&stdout_buffer);
    const stdout = &stdout_writer.interface;

    try stdout.print(
        \\
        \\  cot data - Load and unload fixed-width data files
        \\
        \\  USAGE:
        \\    cot data <command> [options]
        \\
        \\  COMMANDS:
        \\    load <table> <file>     Load fixed-width file into table
        \\    unload <table> <file>   Export table to fixed-width file
        \\    info <table>            Show table layout and byte offsets
        \\
        \\  OPTIONS:
        \\    --skip-header           Skip first line of input file
        \\    --dry-run               Parse but don't write output
        \\    --verbose               Show detailed progress
        \\
        \\  EXAMPLES:
        \\    cot data info customer
        \\    cot data load customer customers.txt
        \\    cot data unload customer customers_export.txt
        \\
        \\  The fixed-width format uses the byte sizes from your schema.cot
        \\  file. Each line in the file represents one record, with fields
        \\  at fixed byte positions.
        \\
        \\
    , .{});
    try stdout_writer.interface.flush();
}

// ============================================================================
// Helper Types and Functions
// ============================================================================

const TableLayout = struct {
    record_size: usize,
    offsets: []usize,
};

fn calculateLayout(allocator: Allocator, table: *const schema.TableSchema) !TableLayout {
    var offsets = try allocator.alloc(usize, table.fields.len);
    var offset: usize = 0;

    for (table.fields, 0..) |field, i| {
        offsets[i] = offset;
        offset += field.byteSize();
    }

    return .{
        .record_size = offset,
        .offsets = offsets,
    };
}

fn findTable(schema_file: *const schema.SchemaFile, name: []const u8) ?*const schema.TableSchema {
    for (schema_file.tables) |*table| {
        if (std.mem.eql(u8, table.name, name)) {
            return table;
        }
    }
    return null;
}

fn generateInsertSql(
    allocator: Allocator,
    table: *const schema.TableSchema,
    layout: TableLayout,
    line: []const u8,
) ![]const u8 {
    var sql: std.ArrayListAligned(u8, null) = .empty;
    errdefer sql.deinit(allocator);

    try sql.appendSlice(allocator, "INSERT INTO ");
    try sql.appendSlice(allocator, table.name);
    try sql.appendSlice(allocator, " (");

    // Column names
    for (table.fields, 0..) |field, i| {
        if (i > 0) try sql.appendSlice(allocator, ", ");
        try sql.appendSlice(allocator, field.name);
    }

    try sql.appendSlice(allocator, ") VALUES (");

    // Values
    for (table.fields, 0..) |field, i| {
        if (i > 0) try sql.appendSlice(allocator, ", ");

        const offset = layout.offsets[i];
        const size = field.byteSize();
        const end = @min(offset + size, line.len);

        if (offset >= line.len) {
            // Field beyond line length, use default
            try sql.appendSlice(allocator, "NULL");
            continue;
        }

        const raw_value = line[offset..end];

        switch (field.data_type) {
            .text, .ulid, .reference => {
                // Trim trailing spaces and quote
                const trimmed = std.mem.trimRight(u8, raw_value, " ");
                try sql.append(allocator, '\'');
                // Escape single quotes
                for (trimmed) |c| {
                    if (c == '\'') {
                        try sql.appendSlice(allocator, "''");
                    } else {
                        try sql.append(allocator, c);
                    }
                }
                try sql.append(allocator, '\'');
            },
            .integer => {
                // Parse as integer, trim leading zeros
                const trimmed = std.mem.trimLeft(u8, raw_value, " 0");
                if (trimmed.len == 0) {
                    try sql.append(allocator, '0');
                } else {
                    try sql.appendSlice(allocator, trimmed);
                }
            },
            .decimal => |d| {
                // Parse as integer, then divide to get decimal
                const trimmed = std.mem.trimLeft(u8, raw_value, " 0");
                if (trimmed.len == 0) {
                    try sql.append(allocator, '0');
                } else {
                    // Insert decimal point
                    const int_val = std.fmt.parseInt(i64, trimmed, 10) catch 0;
                    const divisor = std.math.pow(f64, 10.0, @floatFromInt(d.scale));
                    const float_val = @as(f64, @floatFromInt(int_val)) / divisor;
                    // Format with fixed precision (max 6 decimal places)
                    var fmt_buf: [32]u8 = undefined;
                    const formatted = std.fmt.bufPrint(&fmt_buf, "{d:.6}", .{float_val}) catch "0";
                    try sql.appendSlice(allocator, formatted);
                }
            },
            .boolean => {
                const trimmed = std.mem.trimLeft(u8, raw_value, " 0");
                if (trimmed.len == 0 or std.mem.eql(u8, trimmed, "0")) {
                    try sql.append(allocator, '0');
                } else {
                    try sql.append(allocator, '1');
                }
            },
            else => {
                // Default: treat as string
                try sql.append(allocator, '\'');
                try sql.appendSlice(allocator, raw_value);
                try sql.append(allocator, '\'');
            },
        }
    }

    try sql.appendSlice(allocator, ");");

    return try allocator.dupe(u8, sql.items);
}

fn fieldTypeString(dt: schema.SchemaDataType) []const u8 {
    return switch (dt) {
        .text => "text",
        .integer => "integer",
        .decimal => "decimal",
        .boolean => "boolean",
        .blob => "blob",
        .datetime => "datetime",
        .ulid => "ulid",
        .reference => "reference",
    };
}

fn formatDecorators(d: schema.Decorators, buf: []u8) []const u8 {
    var pos: usize = 0;

    if (d.primary) {
        const s = "@pk ";
        @memcpy(buf[pos..][0..s.len], s);
        pos += s.len;
    }
    if (d.auto) {
        const s = "@auto ";
        @memcpy(buf[pos..][0..s.len], s);
        pos += s.len;
    }
    if (d.not_null) {
        const s = "@nn ";
        @memcpy(buf[pos..][0..s.len], s);
        pos += s.len;
    }
    if (d.unique) {
        const s = "@uniq ";
        @memcpy(buf[pos..][0..s.len], s);
        pos += s.len;
    }
    if (d.index) {
        const s = "@idx ";
        @memcpy(buf[pos..][0..s.len], s);
        pos += s.len;
    }

    return buf[0..pos];
}

const ProjectInfo = struct {
    config: config.ProjectConfig,
    project_path: ?[]const u8,
};

fn findProject(allocator: Allocator, project_name: ?[]const u8) !ProjectInfo {
    _ = project_name;

    var loader = config.ConfigLoader.init(allocator);
    const cwd = try std.fs.cwd().realpathAlloc(allocator, ".");

    const cfg = loader.load(cwd) catch {
        allocator.free(cwd);
        return error.NoWorkspace;
    };

    return ProjectInfo{
        .config = cfg,
        .project_path = cwd,
    };
}
