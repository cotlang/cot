//! Output formatting for trace output
//!
//! Provides multiple output formats and destinations for execution traces.
//! Designed to work with the trace module to produce human-readable,
//! machine-parseable, or compact output.

const std = @import("std");
const Opcode = @import("../bytecode/opcodes.zig").Opcode;
const Value = @import("../bytecode/value.zig").Value;
const trace_mod = @import("trace.zig");
const RegisterSnapshot = trace_mod.RegisterSnapshot;

/// Output format
pub const Format = enum {
    /// Human-readable, optionally colored terminal output
    /// Example: [main:10] 0x0004 load_const r0, #3
    human,

    /// Machine-readable JSON (one object per line, NDJSON)
    /// Example: {"ip":4,"op":"load_const","line":10}
    json,

    /// Compact format for log files
    /// Example: 0004:load_const:r0:#3
    compact,
};

/// Output configuration
pub const Config = struct {
    format: Format = .human,

    /// Output destination
    target: Target = .stderr,

    /// File path (only used if target is .file)
    file_path: ?[]const u8 = null,

    /// Use ANSI colors (human format only)
    color: bool = true,

    /// Include timestamps
    timestamps: bool = false,

    /// Indent based on call depth
    indent_calls: bool = true,

    /// Maximum line width for human format (0 = unlimited)
    max_width: u16 = 0,

    /// Show raw value bits (useful for debugging NaN-boxing issues)
    /// Enable this at --level=full to see raw 64-bit representations
    show_raw_bits: bool = false,

    pub const Target = enum {
        stderr,
        stdout,
        file,
        buffer,
    };
};

/// ANSI color codes for terminal output
pub const Color = struct {
    pub const reset = "\x1b[0m";
    pub const bold = "\x1b[1m";
    pub const dim = "\x1b[2m";

    // Foreground colors
    pub const red = "\x1b[31m";
    pub const green = "\x1b[32m";
    pub const yellow = "\x1b[33m";
    pub const blue = "\x1b[34m";
    pub const magenta = "\x1b[35m";
    pub const cyan = "\x1b[36m";
    pub const white = "\x1b[37m";
    pub const gray = "\x1b[90m";

    // Semantic colors for trace output
    pub const ip = cyan;
    pub const opcode = bold;
    pub const line_num = gray;
    pub const routine = green;
    pub const register = yellow;
    pub const value = magenta;
    pub const error_ = red;
    pub const call = blue;
};

/// Output writer - handles formatting and writing trace entries
pub const Output = struct {
    const Self = @This();

    config: Config,
    writer: Writer,
    allocator: std.mem.Allocator,

    // State
    current_call_depth: u32 = 0,

    const Writer = union(Config.Target) {
        stderr: void,
        stdout: void,
        file: std.fs.File,
        buffer: *std.ArrayList(u8),
    };

    pub fn init(allocator: std.mem.Allocator, config: Config) Self {
        const self = Self{
            .config = config,
            .allocator = allocator,
            .writer = switch (config.target) {
                .stderr => .{ .stderr = {} },
                .stdout => .{ .stdout = {} },
                .file => blk: {
                    if (config.file_path) |path| {
                        const f = std.fs.cwd().createFile(path, .{}) catch {
                            break :blk .{ .stderr = {} };
                        };
                        break :blk .{ .file = f };
                    }
                    break :blk .{ .stderr = {} };
                },
                .buffer => .{ .buffer = undefined }, // Must be set externally
            },
        };
        return self;
    }

    pub fn deinit(self: *Self) void {
        if (self.writer == .file) {
            self.writer.file.close();
        }
    }

    /// Set buffer target (for testing or programmatic use)
    pub fn setBuffer(self: *Self, buf: *std.ArrayList(u8)) void {
        self.writer = .{ .buffer = buf };
    }

    /// Write a formatted trace line
    pub fn write(self: *Self, comptime fmt: []const u8, args: anytype) void {
        switch (self.writer) {
            .stderr => std.debug.print(fmt, args),
            .stdout => {
                const stdout_file: std.fs.File = .stdout();
                var buffer: [4096]u8 = undefined;
                var stdout_writer = stdout_file.writer(&buffer);
                stdout_writer.interface.print(fmt, args) catch {};
            },
            .file => |f| {
                var file_buffer: [4096]u8 = undefined;
                var file_writer = f.writer(&file_buffer);
                file_writer.interface.print(fmt, args) catch {};
            },
            .buffer => |buf| {
                buf.writer(self.allocator).print(fmt, args) catch {};
            },
        }
    }

    /// Write a newline
    pub fn newline(self: *Self) void {
        self.write("\n", .{});
    }

    /// Color helper - only outputs if color is enabled
    pub fn color(self: *Self, c: []const u8) void {
        if (self.config.color and self.config.format == .human) {
            self.write("{s}", .{c});
        }
    }

    /// Write colored text
    pub fn colored(self: *Self, c: []const u8, text: []const u8) void {
        self.color(c);
        self.write("{s}", .{text});
        self.color(Color.reset);
    }

    /// Write indentation based on call depth
    pub fn indent(self: *Self) void {
        if (self.config.indent_calls and self.config.format == .human) {
            for (0..self.current_call_depth) |_| {
                self.write("  ", .{});
            }
        }
    }

    // =========================================================================
    // Trace Entry Formatting
    // =========================================================================

    /// Format an opcode execution event
    pub fn writeOpcode(
        self: *Self,
        ip: u32,
        opcode: Opcode,
        line: u32,
        routine: ?[]const u8,
        operands: ?[]const u8,
    ) void {
        switch (self.config.format) {
            .human => self.writeOpcodeHuman(ip, opcode, line, routine, operands),
            .json => self.writeOpcodeJson(ip, opcode, line, routine),
            .compact => self.writeOpcodeCompact(ip, opcode, line),
        }
    }

    fn writeOpcodeHuman(
        self: *Self,
        ip: u32,
        opcode: Opcode,
        line: u32,
        routine: ?[]const u8,
        operands: ?[]const u8,
    ) void {
        self.indent();

        // [routine:line]
        self.write("[", .{});
        self.color(Color.routine);
        self.write("{s}", .{routine orelse "?"});
        self.color(Color.reset);
        self.write(":", .{});
        self.color(Color.line_num);
        self.write("{d}", .{line});
        self.color(Color.reset);
        self.write("] ", .{});

        // IP
        self.color(Color.ip);
        self.write("0x{x:0>4}", .{ip});
        self.color(Color.reset);
        self.write(" ", .{});

        // Opcode
        self.color(Color.opcode);
        self.write("{s}", .{@tagName(opcode)});
        self.color(Color.reset);

        // Operands (if any)
        if (operands) |ops| {
            if (ops.len > 0) {
                self.write(" {s}", .{ops});
            }
        }

        self.newline();
    }

    fn writeOpcodeJson(
        self: *Self,
        ip: u32,
        opcode: Opcode,
        line: u32,
        routine: ?[]const u8,
    ) void {
        self.write(
            \\{{"ip":{d},"op":"{s}","line":{d},"routine":"{s}"}}
        , .{
            ip,
            @tagName(opcode),
            line,
            routine orelse "",
        });
        self.newline();
    }

    fn writeOpcodeCompact(
        self: *Self,
        ip: u32,
        opcode: Opcode,
        line: u32,
    ) void {
        self.write("{x:0>4}:{s}:{d}\n", .{ ip, @tagName(opcode), line });
    }

    /// Format a routine call event
    pub fn writeCall(self: *Self, target: []const u8, arg_count: u8, ip: u32) void {
        switch (self.config.format) {
            .human => {
                self.indent();
                self.color(Color.call);
                self.write(">>> ", .{});
                self.color(Color.reset);
                self.write("call ", .{});
                self.colored(Color.routine, target);
                self.write("({d} args) @ 0x{x:0>4}\n", .{ arg_count, ip });
                self.current_call_depth += 1;
            },
            .json => {
                self.write(
                    \\{{"event":"call","target":"{s}","args":{d},"ip":{d}}}
                , .{ target, arg_count, ip });
                self.newline();
            },
            .compact => {
                self.write("CALL:{s}:{d}\n", .{ target, arg_count });
            },
        }
    }

    /// Format a native function call with argument values
    pub fn writeNativeCall(self: *Self, name: []const u8, args: []const Value, ip: u32) void {
        switch (self.config.format) {
            .human => {
                self.indent();
                self.color(Color.magenta);
                self.write(">>> ", .{});
                self.color(Color.reset);
                self.write("native ", .{});
                self.colored(Color.routine, name);
                self.write("(", .{});
                // Show each argument with its value
                for (args, 0..) |arg, i| {
                    if (i > 0) self.write(", ", .{});
                    self.color(Color.value);
                    var buf: [128]u8 = undefined;
                    const repr = arg.debugRepr(&buf);
                    self.write("{s}", .{repr});
                    self.color(Color.reset);
                }
                self.write(") @ 0x{x:0>4}\n", .{ip});
                self.current_call_depth += 1;
            },
            .json => {
                self.write(
                    \\{{"event":"native_call","name":"{s}","ip":{d},"args":[
                , .{ name, ip });
                for (args, 0..) |arg, i| {
                    if (i > 0) self.write(",", .{});
                    var buf: [128]u8 = undefined;
                    const repr = arg.debugRepr(&buf);
                    self.write("\"{s}\"", .{repr});
                }
                self.write("]}}\n", .{});
            },
            .compact => {
                self.write("NATIVE:{s}:{d}\n", .{ name, args.len });
            },
        }
    }

    /// Format a native function return with result value
    pub fn writeNativeReturn(self: *Self, name: []const u8, result: ?Value, ip: u32) void {
        switch (self.config.format) {
            .human => {
                if (self.current_call_depth > 0) {
                    self.current_call_depth -= 1;
                }
                self.indent();
                self.color(Color.magenta);
                self.write("<<< ", .{});
                self.color(Color.reset);
                self.write("native ", .{});
                self.colored(Color.routine, name);
                if (result) |res| {
                    self.write(" -> ", .{});
                    self.color(Color.value);
                    var buf: [128]u8 = undefined;
                    const repr = res.debugRepr(&buf);
                    self.write("{s}", .{repr});
                    self.color(Color.reset);
                }
                self.write(" @ 0x{x:0>4}\n", .{ip});
            },
            .json => {
                self.write(
                    \\{{"event":"native_return","name":"{s}","ip":{d}
                , .{ name, ip });
                if (result) |res| {
                    var buf: [128]u8 = undefined;
                    const repr = res.debugRepr(&buf);
                    self.write(",\"result\":\"{s}\"", .{repr});
                }
                self.write("}}\n", .{});
            },
            .compact => {
                self.write("NRET:{s}\n", .{name});
            },
        }
    }

    /// Format a routine return event
    pub fn writeReturn(self: *Self, routine: []const u8, has_value: bool, ip: u32) void {
        switch (self.config.format) {
            .human => {
                if (self.current_call_depth > 0) {
                    self.current_call_depth -= 1;
                }
                self.indent();
                self.color(Color.call);
                self.write("<<< ", .{});
                self.color(Color.reset);
                self.write("return from ", .{});
                self.colored(Color.routine, routine);
                if (has_value) {
                    self.write(" (with value)", .{});
                }
                self.write(" @ 0x{x:0>4}\n", .{ip});
            },
            .json => {
                self.write(
                    \\{{"event":"return","routine":"{s}","has_value":{s},"ip":{d}}}
                , .{ routine, if (has_value) "true" else "false", ip });
                self.newline();
            },
            .compact => {
                self.write("RET:{s}\n", .{routine});
            },
        }
    }

    /// Format a return event with the actual return value (enhanced tracing)
    pub fn writeReturnWithValue(self: *Self, routine: []const u8, has_value: bool, ip: u32, return_value: ?Value) void {
        switch (self.config.format) {
            .human => {
                if (self.current_call_depth > 0) {
                    self.current_call_depth -= 1;
                }
                self.indent();
                self.color(Color.call);
                self.write("<<< ", .{});
                self.color(Color.reset);
                self.write("return from ", .{});
                self.colored(Color.routine, routine);
                if (has_value) {
                    if (return_value) |val| {
                        self.write(" -> ", .{});
                        self.color(Color.value);
                        // Format return value using RegisterSnapshot
                        const snapshot = RegisterSnapshot.fromValue(val);
                        var buf: [64]u8 = undefined;
                        const repr = snapshot.format(&buf);
                        self.write("{s}", .{repr});
                        self.color(Color.reset);
                    } else {
                        self.write(" (with value)", .{});
                    }
                }
                self.write(" @ 0x{x:0>4}\n", .{ip});
            },
            .json => {
                self.write(
                    \\{{"event":"return","routine":"{s}","has_value":{s},"ip":{d}}}
                , .{ routine, if (has_value) "true" else "false", ip });
                self.newline();
            },
            .compact => {
                self.write("RET:{s}\n", .{routine});
            },
        }
    }

    /// Format an error event
    pub fn writeError(self: *Self, ip: u32, line: u32, err: anyerror, message: []const u8) void {
        switch (self.config.format) {
            .human => {
                self.color(Color.error_);
                self.write("!!! ERROR ", .{});
                self.color(Color.reset);
                self.write("at 0x{x:0>4} line {d}: ", .{ ip, line });
                self.color(Color.error_);
                self.write("{s}", .{@errorName(err)});
                self.color(Color.reset);
                if (message.len > 0) {
                    self.write(" - {s}", .{message});
                }
                self.newline();
            },
            .json => {
                self.write(
                    \\{{"event":"error","ip":{d},"line":{d},"error":"{s}","message":"{s}"}}
                , .{ ip, line, @errorName(err), message });
                self.newline();
            },
            .compact => {
                self.write("ERR:{x:0>4}:{s}\n", .{ ip, @errorName(err) });
            },
        }
    }

    /// Write section header (for crash dumps)
    pub fn writeHeader(self: *Self, title: []const u8) void {
        switch (self.config.format) {
            .human => {
                self.newline();
                self.color(Color.bold);
                self.write("=== {s} ===", .{title});
                self.color(Color.reset);
                self.newline();
                self.newline();
            },
            .json => {
                self.write(
                    \\{{"section":"{s}"}}
                , .{title});
                self.newline();
            },
            .compact => {
                self.write("### {s}\n", .{title});
            },
        }
    }

    /// Write a separator line
    pub fn writeSeparator(self: *Self) void {
        if (self.config.format == .human) {
            self.color(Color.dim);
            self.write("────────────────────────────────────────\n", .{});
            self.color(Color.reset);
        }
    }

    /// Write register state (legacy - raw i64 values)
    pub fn writeRegisters(self: *Self, regs: []const i64) void {
        switch (self.config.format) {
            .human => {
                self.write("  | ", .{});
                for (regs, 0..) |val, i| {
                    self.color(Color.register);
                    self.write("r{d}", .{i});
                    self.color(Color.reset);
                    self.write("=", .{});
                    self.color(Color.value);
                    self.write("{d}", .{val});
                    self.color(Color.reset);
                    if (i < regs.len - 1) self.write(" ", .{});
                }
            },
            .json => {
                // Included in opcode event
            },
            .compact => {
                // Not included
            },
        }
    }

    /// Write register state with type information (verbose mode)
    /// Uses debugRepr to show values with their types
    /// When show_raw_bits is enabled, also displays raw 64-bit representation
    /// (useful for debugging NaN-boxing issues)
    pub fn writeRegisterSnapshots(self: *Self, regs: []const RegisterSnapshot) void {
        switch (self.config.format) {
            .human => {
                self.write("  | ", .{});
                // Only show non-null registers to reduce noise
                var first = true;
                for (regs, 0..) |reg, i| {
                    // Skip null values to reduce clutter
                    if (reg.tag == .null_val) continue;

                    if (!first) self.write(" ", .{});
                    first = false;

                    self.color(Color.register);
                    self.write("r{d}", .{i});
                    self.color(Color.reset);
                    self.write("=", .{});
                    self.color(Color.value);
                    // Format with type info
                    var buf: [64]u8 = undefined;
                    const repr = reg.format(&buf);
                    self.write("{s}", .{repr});
                    // Show raw bits for debugging NaN-boxing issues
                    if (self.config.show_raw_bits) {
                        self.color(Color.dim);
                        self.write("[0x{x:0>16}]", .{reg.bits});
                    }
                    self.color(Color.reset);
                }
                // If all registers were null, show that
                if (first) {
                    self.color(Color.dim);
                    self.write("(all nil)", .{});
                    self.color(Color.reset);
                }
            },
            .json => {
                // Include register types and values in JSON
                self.write(",\"regs\":[", .{});
                for (regs, 0..) |reg, i| {
                    if (i > 0) self.write(",", .{});
                    var buf: [64]u8 = undefined;
                    const repr = reg.format(&buf);
                    self.write("\"{s}\"", .{repr});
                }
                self.write("]", .{});
            },
            .compact => {
                // Include compact register state
                self.write("|", .{});
                for (regs) |reg| {
                    var buf: [32]u8 = undefined;
                    const repr = reg.format(&buf);
                    self.write("{s},", .{repr});
                }
            },
        }
    }
};

// ============================================================================
// Tests
// ============================================================================

test "Output: human format" {
    const allocator = std.testing.allocator;
    var buf = std.ArrayList(u8).init(allocator);
    defer buf.deinit();

    var output = Output.init(allocator, .{
        .format = .human,
        .target = .buffer,
        .color = false, // Disable for testing
    });
    output.setBuffer(&buf);

    output.writeOpcode(0x0004, .load_const, 10, "main", "r0, #3");

    try std.testing.expectEqualStrings("[main:10] 0x0004 load_const r0, #3\n", buf.items);
}

test "Output: json format" {
    const allocator = std.testing.allocator;
    var buf = std.ArrayList(u8).init(allocator);
    defer buf.deinit();

    var output = Output.init(allocator, .{
        .format = .json,
        .target = .buffer,
    });
    output.setBuffer(&buf);

    output.writeOpcode(0x0004, .load_const, 10, "main", null);

    try std.testing.expectEqualStrings(
        \\{"ip":4,"op":"load_const","line":10,"routine":"main"}
        \\
    , buf.items);
}

test "Output: compact format" {
    const allocator = std.testing.allocator;
    var buf = std.ArrayList(u8).init(allocator);
    defer buf.deinit();

    var output = Output.init(allocator, .{
        .format = .compact,
        .target = .buffer,
    });
    output.setBuffer(&buf);

    output.writeOpcode(0x0004, .load_const, 10, null, null);

    try std.testing.expectEqualStrings("0004:load_const:10\n", buf.items);
}
