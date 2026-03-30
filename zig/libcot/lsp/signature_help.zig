//! textDocument/signatureHelp — parameter hints while typing function arguments.
//! Triggered by `(` and `,` characters to show active parameter info.

const std = @import("std");
const types_mod = @import("../frontend/types.zig");
const checker_mod = @import("../frontend/checker.zig");
const analysis = @import("analysis.zig");

const TypeRegistry = types_mod.TypeRegistry;
const TypeIndex = types_mod.TypeIndex;
const Type = types_mod.Type;
const invalid_type = types_mod.invalid_type;
const AnalysisResult = analysis.AnalysisResult;

pub const ParameterInfo = struct {
    label: []const u8,
};

pub const SignatureInfo = struct {
    label: []const u8,
    parameters: []const ParameterInfo,
    active_parameter: u32,
};

/// Get signature help for the given byte offset (inside a function call).
pub fn getSignatureHelp(allocator: std.mem.Allocator, result: *AnalysisResult, byte_offset: u32) ?SignatureInfo {
    const content = result.src.content;
    if (byte_offset == 0 or byte_offset > content.len) return null;

    // Walk backwards to find the opening paren and the function name
    var paren_depth: i32 = 0;
    var comma_count: u32 = 0;
    var pos: u32 = byte_offset;

    while (pos > 0) {
        pos -= 1;
        const ch = content[pos];
        if (ch == ')') {
            paren_depth += 1;
        } else if (ch == '(') {
            if (paren_depth == 0) {
                // Found our opening paren
                break;
            }
            paren_depth -= 1;
        } else if (ch == ',' and paren_depth == 0) {
            comma_count += 1;
        }
    } else {
        return null;
    }

    // pos is now at the opening paren. Get the function name before it.
    if (pos == 0) return null;
    var name_end = pos;
    // Skip whitespace between name and paren
    while (name_end > 0 and content[name_end - 1] == ' ') {
        name_end -= 1;
    }
    var name_start = name_end;
    while (name_start > 0 and (std.ascii.isAlphanumeric(content[name_start - 1]) or content[name_start - 1] == '_')) {
        name_start -= 1;
    }

    const func_name = content[name_start..name_end];
    if (func_name.len == 0) return null;

    // Check if this is a method call (preceded by `.`)
    var is_method = false;
    var method_type_name: ?[]const u8 = null;
    if (name_start > 0 and content[name_start - 1] == '.') {
        is_method = true;
        // Get the base identifier before the dot
        const base_end = name_start - 1;
        var base_start = base_end;
        while (base_start > 0 and (std.ascii.isAlphanumeric(content[base_start - 1]) or content[base_start - 1] == '_')) {
            base_start -= 1;
        }
        const base_name = content[base_start..base_end];
        if (base_name.len > 0) {
            if (result.global_scope.lookup(base_name)) |sym| {
                var resolved_idx = sym.type_idx;
                const t = result.type_reg.get(resolved_idx);
                switch (t) {
                    .pointer => |ptr| resolved_idx = ptr.elem,
                    else => {},
                }
                method_type_name = result.type_reg.typeName(resolved_idx);
            }
        }
    }

    // Look up the function type
    var func_type: ?types_mod.FuncType = null;
    var skip_self = false;

    if (is_method) {
        if (method_type_name) |tn| {
            if (result.type_reg.lookupMethod(tn, func_name)) |method_info| {
                const mt = result.type_reg.get(method_info.func_type);
                switch (mt) {
                    .func => |ft| {
                        func_type = ft;
                        skip_self = true;
                    },
                    else => {},
                }
            }
        }
    } else {
        if (result.global_scope.lookup(func_name)) |sym| {
            const t = result.type_reg.get(sym.type_idx);
            switch (t) {
                .func => |ft| {
                    func_type = ft;
                },
                else => {},
            }
        }
    }

    const ft = func_type orelse return null;

    // Build signature label and parameter info
    var sig = std.ArrayListUnmanaged(u8){};
    sig.appendSlice(allocator, func_name) catch return null;
    sig.append(allocator, '(') catch return null;

    var params = std.ArrayListUnmanaged(ParameterInfo){};
    const start_idx: usize = if (skip_self and ft.params.len > 0) 1 else 0;

    for (ft.params[start_idx..], 0..) |param, i| {
        if (i > 0) sig.appendSlice(allocator, ", ") catch return null;
        const param_label = std.fmt.allocPrint(allocator, "{s}: {s}", .{
            param.name,
            result.type_reg.typeName(param.type_idx),
        }) catch return null;
        sig.appendSlice(allocator, param_label) catch return null;
        params.append(allocator, .{ .label = param_label }) catch return null;
    }

    sig.appendSlice(allocator, ") ") catch return null;
    sig.appendSlice(allocator, result.type_reg.typeName(ft.return_type)) catch return null;

    const active_param = @min(comma_count, if (params.items.len > 0) @as(u32, @intCast(params.items.len)) - 1 else 0);

    return .{
        .label = sig.toOwnedSlice(allocator) catch return null,
        .parameters = params.toOwnedSlice(allocator) catch return null,
        .active_parameter = active_param,
    };
}
