//! Convert Cot frontend errors to LSP Diagnostics.

const std = @import("std");
const errors_mod = @import("../frontend/errors.zig");
const lsp_types = @import("types.zig");

const Error = errors_mod.Error;
const Diagnostic = lsp_types.Diagnostic;
const DiagnosticSeverity = lsp_types.DiagnosticSeverity;

/// Convert collected Cot errors to LSP Diagnostics.
pub fn convertErrors(allocator: std.mem.Allocator, content: []const u8, errs: []const Error) ![]Diagnostic {
    var diags = std.ArrayListUnmanaged(Diagnostic){};
    for (errs) |err| {
        const code_str: ?[]const u8 = if (err.err_code) |ec| blk: {
            break :blk std.fmt.allocPrint(allocator, "E{d}", .{ec.code()}) catch null;
        } else null;

        try diags.append(allocator, .{
            .range = lsp_types.spanToRange(content, err.span),
            .severity = .@"error",
            .code = code_str,
            .message = err.msg,
        });
    }
    return diags.toOwnedSlice(allocator);
}
