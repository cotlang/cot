//! Error reporting for the Cot compiler frontend.
//!
//! Provides structured error and warning reporting with:
//!   - Numbered error codes (E100-E403) grouped by compiler stage
//!   - Numbered warning codes (W001-W005) for lint rules
//!   - Colored terminal output with source line and caret pointing
//!   - Attached notes for additional context
//!   - Error count limiting (stops after 10 to avoid noise)
//!   - Optional custom handler for programmatic error collection
//!
//! Error output follows the standard compiler format (GCC, Clang, Rust, Zig):
//!   filename:line:column: error[E300]: type mismatch
//!       let x: i64 = "hello"
//!                     ^~~~~~

const std = @import("std");
const source = @import("source.zig");

const Pos = source.Pos;
const Span = source.Span;
const Source = source.Source;

pub const Severity = enum { err, warning, note };


/// Warning codes for lint rules.
///
///   W001 — unused variable
///   W002 — unused parameter
///   W003 — variable shadowing
///   W004 — unreachable code
///   W005 — empty block
pub const WarningCode = enum(u16) {
    w001 = 1,
    w002 = 2,
    w003 = 3,
    w004 = 4,
    w005 = 5,

    pub fn code(self: WarningCode) u16 {
        return @intFromEnum(self);
    }

    pub fn description(self: WarningCode) []const u8 {
        return switch (self) {
            .w001 => "unused variable",
            .w002 => "unused parameter",
            .w003 => "variable shadowing",
            .w004 => "unreachable code",
            .w005 => "empty block",
        };
    }
};

/// Error codes grouped by compiler stage.
///
///   E1xx — Scanner errors (malformed tokens)
///   E2xx — Parser errors (unexpected syntax)
///   E3xx — Type errors (mismatched types, undefined names)
///   E4xx — Semantic errors (control flow violations)
pub const ErrorCode = enum(u16) {
    // Scanner (E100-E104)
    e100 = 100, // unterminated string literal
    e101 = 101, // unterminated character literal
    e102 = 102, // invalid escape sequence
    e103 = 103, // invalid number literal
    e104 = 104, // unexpected character

    // Parser (E200-E208)
    e200 = 200, // unexpected token
    e201 = 201, // expected expression
    e202 = 202, // expected type
    e203 = 203, // expected identifier
    e204 = 204, // expected '{'
    e205 = 205, // expected '}'
    e206 = 206, // expected '('
    e207 = 207, // expected ')'
    e208 = 208, // expected ';' or newline

    // Type checking (E300-E306)
    e300 = 300, // type mismatch
    e301 = 301, // undefined identifier
    e302 = 302, // redefined identifier
    e303 = 303, // invalid operation
    e304 = 304, // wrong number of arguments
    e305 = 305, // not callable
    e306 = 306, // field not found

    // Semantic (E400-E403)
    e400 = 400, // break outside loop
    e401 = 401, // continue outside loop
    e402 = 402, // return type mismatch
    e403 = 403, // missing return

    pub fn code(self: ErrorCode) u16 {
        return @intFromEnum(self);
    }

    pub fn description(self: ErrorCode) []const u8 {
        return switch (self) {
            .e100 => "unterminated string literal",
            .e101 => "unterminated character literal",
            .e102 => "invalid escape sequence",
            .e103 => "invalid number literal",
            .e104 => "unexpected character",
            .e200 => "unexpected token",
            .e201 => "expected expression",
            .e202 => "expected type",
            .e203 => "expected identifier",
            .e204 => "expected '{'",
            .e205 => "expected '}'",
            .e206 => "expected '('",
            .e207 => "expected ')'",
            .e208 => "expected ';' or newline",
            .e300 => "type mismatch",
            .e301 => "undefined identifier",
            .e302 => "redefined identifier",
            .e303 => "invalid operation",
            .e304 => "wrong number of arguments",
            .e305 => "not callable",
            .e306 => "field not found",
            .e400 => "break outside loop",
            .e401 => "continue outside loop",
            .e402 => "return type mismatch",
            .e403 => "missing return",
        };
    }
};


/// Additional context attached to an error (e.g., "previous definition here").
pub const Note = struct {
    span: Span,
    msg: []const u8,
};

/// A single compiler diagnostic: error, warning, or note.
///
/// Created via static constructors (`at`, `withCode`, `warn`) and consumed
/// by the ErrorReporter. Carries the source location, message, optional
/// error/warning code, and an optional attached note.
pub const Error = struct {
    span: Span,
    msg: []const u8,
    err_code: ?ErrorCode = null,
    severity: Severity = .err,
    warning_code: ?WarningCode = null,
    note: ?Note = null,

    pub fn at(pos: Pos, msg: []const u8) Error {
        return .{ .span = Span.fromPos(pos), .msg = msg };
    }

    pub fn withCode(pos: Pos, err_code: ErrorCode, msg: []const u8) Error {
        return .{ .span = Span.fromPos(pos), .msg = msg, .err_code = err_code };
    }

    pub fn withCodeAndNote(pos: Pos, err_code: ErrorCode, msg: []const u8, note_pos: Pos, note_msg: []const u8) Error {
        return .{
            .span = Span.fromPos(pos),
            .msg = msg,
            .err_code = err_code,
            .note = .{ .span = Span.fromPos(note_pos), .msg = note_msg },
        };
    }

    pub fn atSpan(span: Span, msg: []const u8) Error {
        return .{ .span = span, .msg = msg };
    }

    pub fn warn(pos: Pos, wc: WarningCode, msg: []const u8) Error {
        return .{ .span = Span.fromPos(pos), .msg = msg, .severity = .warning, .warning_code = wc };
    }
};

pub const ErrorHandler = *const fn (err: Error) void;
pub const MAX_ERRORS: u32 = 10;

/// Collects and reports compiler diagnostics.
///
/// Tracks error/warning counts, stores the first error for quick access,
/// and formats output to stderr with colored source context. Stops
/// reporting after MAX_ERRORS to avoid flooding the terminal.
///
/// Two modes:
///   - Default: prints directly to stderr (colored if TTY)
///   - Custom handler: calls a function pointer for each error
///     (used by the LSP and test harness to collect errors programmatically)
pub const ErrorReporter = struct {
    src: *Source,
    handler: ?ErrorHandler,
    first: ?Error,
    count: u32,
    warning_count: u32,
    suppressed: bool,

    pub fn init(src: *Source, handler: ?ErrorHandler) ErrorReporter {
        return .{
            .src = src,
            .handler = handler,
            .first = null,
            .count = 0,
            .warning_count = 0,
            .suppressed = false,
        };
    }

    pub fn errorAt(self: *ErrorReporter, pos: Pos, msg: []const u8) void {
        self.report(Error.at(pos, msg));
    }

    pub fn errorWithCode(self: *ErrorReporter, pos: Pos, err_code: ErrorCode, msg: []const u8) void {
        self.report(Error.withCode(pos, err_code, msg));
    }

    pub fn errorAtSpan(self: *ErrorReporter, span: Span, msg: []const u8) void {
        self.report(Error.atSpan(span, msg));
    }

    pub fn errorWithCodeAndNote(self: *ErrorReporter, pos: Pos, err_code: ErrorCode, msg: []const u8, note_pos: Pos, note_msg: []const u8) void {
        self.report(Error.withCodeAndNote(pos, err_code, msg, note_pos, note_msg));
    }

    pub fn warningWithCode(self: *ErrorReporter, pos: Pos, wc: WarningCode, msg: []const u8) void {
        self.warning_count += 1;
        const err = Error.warn(pos, wc, msg);
        if (self.handler) |h| h(err) else self.printError(err);
    }

    pub fn report(self: *ErrorReporter, err: Error) void {
        if (self.first == null) self.first = err;
        self.count += 1;

        if (self.count > MAX_ERRORS) {
            if (!self.suppressed) {
                self.suppressed = true;
                std.debug.print("error: too many errors ({d}), stopping\n", .{MAX_ERRORS});
            }
            return;
        }

        if (self.handler) |h| h(err) else self.printError(err);
    }

    /// Format and print a diagnostic to stderr with colored source context.
    ///
    /// Output format:
    ///   filename:line:column: error[E300]: message
    ///       source line text
    ///       ^~~~~~ (caret + underline spanning the error)
    ///   filename:line:column: note: additional context (if attached)
    fn printError(self: *ErrorReporter, err: Error) void {
        const pos = self.src.position(err.span.start);
        const is_tty = std.posix.isatty(2);

        const red = if (is_tty) "\x1b[1;31m" else "";
        const yellow = if (is_tty) "\x1b[1;33m" else "";
        const cyan = if (is_tty) "\x1b[1;36m" else "";
        const green = if (is_tty) "\x1b[1;32m" else "";
        const reset = if (is_tty) "\x1b[0m" else "";

        if (err.severity == .warning) {
            if (err.warning_code) |wc| {
                std.debug.print("{s}{s}:{d}:{d}:{s} {s}warning[W{d:0>3}]:{s} {s}\n", .{
                    cyan, pos.filename, pos.line, pos.column, reset,
                    yellow, wc.code(), reset,
                    err.msg,
                });
            } else {
                std.debug.print("{s}{s}:{d}:{d}:{s} {s}warning:{s} {s}\n", .{
                    cyan, pos.filename, pos.line, pos.column, reset,
                    yellow, reset,
                    err.msg,
                });
            }
        } else if (err.err_code) |ec| {
            std.debug.print("{s}{s}:{d}:{d}:{s} {s}error[E{d}]:{s} {s}\n", .{
                cyan, pos.filename, pos.line, pos.column, reset,
                red, ec.code(), reset,
                err.msg,
            });
        } else {
            std.debug.print("{s}{s}:{d}:{d}:{s} {s}error:{s} {s}\n", .{
                cyan, pos.filename, pos.line, pos.column, reset,
                red, reset,
                err.msg,
            });
        }

        // Source line with caret underline
        const line = self.src.getLine(err.span.start);
        std.debug.print("    {s}\n", .{line});

        if (pos.column > 0) {
            std.debug.print("    ", .{});
            var i: u32 = 0;
            while (i < pos.column - 1) : (i += 1) {
                if (i < line.len and line[i] == '\t') std.debug.print("\t", .{}) else std.debug.print(" ", .{});
            }
            const span_len = err.span.len();
            const underline_len = if (span_len > 1) span_len else 1;
            const underline_color = if (err.severity == .warning) yellow else green;
            std.debug.print("{s}^", .{underline_color});
            var j: u32 = 1;
            while (j < underline_len) : (j += 1) {
                std.debug.print("~", .{});
            }
            std.debug.print("{s}\n", .{reset});
        }

        // Attached note with its own source context
        if (err.note) |note| {
            const note_pos = self.src.position(note.span.start);
            const blue = if (is_tty) "\x1b[1;34m" else "";
            std.debug.print("{s}{s}:{d}:{d}:{s} {s}note:{s} {s}\n", .{
                cyan, note_pos.filename, note_pos.line, note_pos.column, reset,
                blue, reset,
                note.msg,
            });
            const note_line = self.src.getLine(note.span.start);
            std.debug.print("    {s}\n", .{note_line});
            if (note_pos.column > 0) {
                std.debug.print("    ", .{});
                var k: u32 = 0;
                while (k < note_pos.column - 1) : (k += 1) {
                    if (k < note_line.len and note_line[k] == '\t') std.debug.print("\t", .{}) else std.debug.print(" ", .{});
                }
                std.debug.print("{s}^{s}\n", .{ blue, reset });
            }
        }
    }

    pub fn hasErrors(self: *const ErrorReporter) bool {
        return self.count > 0;
    }

    pub fn errorCount(self: *const ErrorReporter) u32 {
        return self.count;
    }

    pub fn firstError(self: *const ErrorReporter) ?Error {
        return self.first;
    }
};


test "error code descriptions" {
    try std.testing.expectEqualStrings("unterminated string literal", ErrorCode.e100.description());
    try std.testing.expectEqualStrings("unexpected token", ErrorCode.e200.description());
    try std.testing.expectEqualStrings("type mismatch", ErrorCode.e300.description());
}

test "error code values" {
    try std.testing.expectEqual(@as(u16, 100), ErrorCode.e100.code());
    try std.testing.expectEqual(@as(u16, 200), ErrorCode.e200.code());
}

test "error creation" {
    const err = Error.at(Pos{ .offset = 10 }, "test error");
    try std.testing.expectEqual(@as(u32, 10), err.span.start.offset);
    try std.testing.expectEqualStrings("test error", err.msg);
    try std.testing.expect(err.err_code == null);

    const err2 = Error.withCode(Pos{ .offset = 5 }, .e200, "unexpected");
    try std.testing.expectEqual(ErrorCode.e200, err2.err_code.?);
}

test "reporter tracks errors" {
    const content = "fn main() {\n    x = 1\n}";
    var src = Source.init(std.testing.allocator, "test.cot", content);
    defer src.deinit();

    var reporter = ErrorReporter.init(&src, null);
    try std.testing.expect(!reporter.hasErrors());
    reporter.errorAt(Pos{ .offset = 16 }, "undefined variable 'x'");
    try std.testing.expect(reporter.hasErrors());
    try std.testing.expectEqual(@as(u32, 1), reporter.errorCount());
}

test "reporter with error code" {
    const content = "let s = \"unterminated";
    var src = Source.init(std.testing.allocator, "test.cot", content);
    defer src.deinit();

    var reporter = ErrorReporter.init(&src, null);
    reporter.errorWithCode(Pos{ .offset = 8 }, .e100, "string not terminated");
    try std.testing.expectEqual(ErrorCode.e100, reporter.firstError().?.err_code.?);
}

test "reporter multiple errors" {
    const content = "x y z";
    var src = Source.init(std.testing.allocator, "test.cot", content);
    defer src.deinit();

    var reporter = ErrorReporter.init(&src, null);
    reporter.errorAt(Pos{ .offset = 0 }, "error 1");
    reporter.errorAt(Pos{ .offset = 2 }, "error 2");
    reporter.errorAt(Pos{ .offset = 4 }, "error 3");
    try std.testing.expectEqual(@as(u32, 3), reporter.errorCount());
    try std.testing.expectEqualStrings("error 1", reporter.firstError().?.msg);
}

test "reporter custom handler" {
    const content = "test";
    var src = Source.init(std.testing.allocator, "test.cot", content);
    defer src.deinit();

    const handler = struct {
        fn handle(_: Error) void {}
    }.handle;

    var reporter = ErrorReporter.init(&src, handler);
    reporter.errorAt(Pos{ .offset = 0 }, "test");
    try std.testing.expectEqual(@as(u32, 1), reporter.errorCount());
}
