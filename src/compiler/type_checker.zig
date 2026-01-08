//! Type Checker
//!
//! Validates IR for type correctness before bytecode emission.
//! This pass runs after IR lowering and reports all type errors with
//! source locations for developer-friendly diagnostics.

const std = @import("std");
const Allocator = std.mem.Allocator;
const ir = @import("../ir/ir.zig");
const diagnostics = @import("diagnostics.zig");
const DiagnosticCollector = @import("diagnostic_collector.zig");
const type_rules = @import("type_rules.zig");
const SourceRange = diagnostics.SourceRange;
const Code = diagnostics.Code;
const Type = ir.Type;

const Self = @This();

allocator: Allocator,
collector: *DiagnosticCollector,
module: *const ir.Module,
file_path: []const u8,

// Current context
current_function: ?*const ir.Function = null,
current_block: ?*const ir.Block = null,

/// Initialize a new type checker
pub fn init(
    allocator: Allocator,
    collector: *DiagnosticCollector,
    module: *const ir.Module,
    file_path: []const u8,
) Self {
    return .{
        .allocator = allocator,
        .collector = collector,
        .module = module,
        .file_path = file_path,
    };
}

/// Run type checking on the entire module
pub fn check(self: *Self) void {
    // Check each function
    for (self.module.functions.items) |func| {
        self.checkFunction(func);
    }
}

/// Check a single function
fn checkFunction(self: *Self, func: *const ir.Function) void {
    self.current_function = func;

    // Check each block
    for (func.blocks.items) |block| {
        self.current_block = block;
        self.checkBlock(block);
    }

    self.current_function = null;
    self.current_block = null;
}

/// Check a block of instructions
fn checkBlock(self: *Self, block: *const ir.Block) void {
    for (block.instructions.items) |inst| {
        self.checkInstruction(inst);
    }
}

/// Check a single instruction
fn checkInstruction(self: *Self, inst: ir.Instruction) void {
    switch (inst) {
        // Memory operations
        .store => |s| self.checkStore(s),
        .load => {}, // Loads are type-safe by construction

        // Arithmetic operations (Cranelift names)
        .iadd => |op| self.checkBinaryOp(op, .arithmetic, "+"),
        .isub => |op| self.checkBinaryOp(op, .arithmetic, "-"),
        .imul => |op| self.checkBinaryOp(op, .arithmetic, "*"),
        .sdiv, .udiv => |op| self.checkBinaryOp(op, .arithmetic, "/"),
        .srem, .urem => |op| self.checkBinaryOp(op, .arithmetic, "%"),

        // Rounding operations (DBL legacy)
        .round, .trunc => {}, // Rounding is type-safe by construction

        // Comparison operations (Cranelift style - icmp with condition)
        .icmp => |op| self.checkIcmpOp(op),

        // Logical operations (always valid)
        .log_and, .log_or, .log_not => {},

        // String operations
        .str_concat => |op| self.checkStringConcat(op),
        .str_slice => {}, // Slice operations are type-safe
        .str_slice_store => {}, // Slice store operations are type-safe
        .str_byte_at => {}, // Byte access is type-safe (string, int -> int)

        // Function calls
        .call => |c| self.checkCall(c),
        .call_indirect => {}, // Indirect calls checked at runtime

        // Array operations
        .array_load, .array_load_opt => |a| self.checkArrayLoad(a),
        .array_store => |a| self.checkArrayStore(a),

        // These don't need type checking (Cranelift names)
        .alloca, .field_ptr, .jump, .brif, .br_table, .return_, .trap => {},
        .iconst, .f32const, .f64const, .const_string, .const_null => {},
        .fcvt_from_sint, .fcvt_from_uint, .fcvt_to_sint, .fcvt_to_uint, .sextend, .uextend, .ireduce, .bitcast, .format_decimal, .parse_decimal => {},
        .io_open, .io_close, .io_read, .io_write, .io_delete, .io_unlock => {},
        .load_struct_buf, .store_struct_buf, .str_compare, .str_copy, .str_len, .ineg => {},
        .try_begin, .try_end, .catch_begin, .throw => {},
        .wrap_optional, .unwrap_optional, .is_null, .is_type => {},
        .band, .bor, .bxor, .bnot, .ishl, .sshr, .ushr => {},
        .array_len, .array_slice, .debug_line, .select, .ptr_offset => {},
        .map_new, .map_set, .map_get, .map_delete, .map_has, .map_len, .map_clear, .map_keys, .map_values, .map_key_at => {},
        .list_new, .list_push, .list_pop, .list_get, .list_set, .list_len, .list_clear, .list_to_slice => {},
        // Closure operations
        .make_closure => {},
        // Weak reference operations
        .weak_ref, .weak_load => {},
        // ARC operations
        .arc_retain, .arc_release, .arc_move => {},
        // Trait object operations
        .make_trait_object, .call_trait_method => {},
        // Heap record operations
        .heap_alloc, .store_field_heap, .load_field_heap => {},
        // Variant operations (sum types)
        .variant_construct, .variant_get_tag => {},
    }
}

/// Check a store instruction
fn checkStore(self: *Self, store: ir.Instruction.Store) void {
    const target_ty = derefType(store.ptr.ty);
    const value_ty = store.value.ty;
    const loc_info = locToInfo(store.loc);
    const error_file = loc_info.file_path orelse self.file_path;

    // Debug logging to help diagnose type mismatches (enable with COT_DEBUG_TYPE_CHECK=1)
    if (target_ty == .void and value_ty != .void) {
        if (std.posix.getenv("COT_DEBUG_TYPE_CHECK")) |_| {
            var value_buf: [64]u8 = undefined;
            var ptr_buf: [64]u8 = undefined;
            const line = if (store.loc) |l| l.line else 0;
            std.debug.print("[TYPE_CHECK] Store to void ptr at line {d}, value type: {s}, ptr.id={d}, ptr.ty={s}, val.id={d}\n", .{
                line,
                type_rules.formatType(value_ty, &value_buf),
                store.ptr.id,
                type_rules.formatType(store.ptr.ty, &ptr_buf),
                store.value.id,
            });
        }
    }

    const compat = type_rules.isAssignable(target_ty, value_ty);

    switch (compat) {
        .compatible, .implicit_conversion => {
            // OK
        },
        .lossy_conversion => {
            // Warn about potential data loss
            var target_buf: [64]u8 = undefined;
            var value_buf: [64]u8 = undefined;
            self.collector.addWarning(
                .W003_implicit_conversion,
                error_file,
                loc_info.range,
                "assignment may lose precision: '{s}' to '{s}'",
                .{
                    type_rules.formatType(value_ty, &value_buf),
                    type_rules.formatType(target_ty, &target_buf),
                },
            );
        },
        .incompatible => {
            // Error
            var target_buf: [64]u8 = undefined;
            var value_buf: [64]u8 = undefined;
            self.collector.addError(
                .E200_type_mismatch,
                error_file,
                loc_info.range,
                "cannot assign '{s}' to '{s}'",
                .{
                    type_rules.formatType(value_ty, &value_buf),
                    type_rules.formatType(target_ty, &target_buf),
                },
            );
        },
    }
}

/// Check a binary operation
fn checkBinaryOp(self: *Self, op: ir.Instruction.BinaryOp, op_type: type_rules.BinaryOpType, op_name: []const u8) void {
    const result = type_rules.checkBinaryOp(op_type, op.lhs.ty, op.rhs.ty);
    const loc_info = locToInfo(op.loc);
    const error_file = loc_info.file_path orelse self.file_path;

    if (!result.ok) {
        var lhs_buf: [64]u8 = undefined;
        var rhs_buf: [64]u8 = undefined;
        self.collector.addError(
            .E206_incompatible_operands,
            error_file,
            loc_info.range,
            "operator '{s}' cannot be applied to '{s}' and '{s}'",
            .{
                op_name,
                type_rules.formatType(op.lhs.ty, &lhs_buf),
                type_rules.formatType(op.rhs.ty, &rhs_buf),
            },
        );
    }
}

/// Check icmp (integer comparison) instruction
fn checkIcmpOp(self: *Self, op: ir.Instruction.IcmpOp) void {
    const result = type_rules.checkBinaryOp(.comparison, op.lhs.ty, op.rhs.ty);
    const loc_info = locToInfo(op.loc);
    const error_file = loc_info.file_path orelse self.file_path;

    if (!result.ok) {
        var lhs_buf: [64]u8 = undefined;
        var rhs_buf: [64]u8 = undefined;
        self.collector.addError(
            .E206_incompatible_operands,
            error_file,
            loc_info.range,
            "comparison cannot be applied to '{s}' and '{s}'",
            .{
                type_rules.formatType(op.lhs.ty, &lhs_buf),
                type_rules.formatType(op.rhs.ty, &rhs_buf),
            },
        );
    }
}

/// Check string concatenation
fn checkStringConcat(self: *Self, op: ir.Instruction.BinaryOp) void {
    const result = type_rules.checkBinaryOp(.string_concat, op.lhs.ty, op.rhs.ty);
    const loc_info = locToInfo(op.loc);
    const error_file = loc_info.file_path orelse self.file_path;

    if (!result.ok) {
        var lhs_buf: [64]u8 = undefined;
        var rhs_buf: [64]u8 = undefined;
        self.collector.addError(
            .E206_incompatible_operands,
            error_file,
            loc_info.range,
            "string concatenation requires string operands, got '{s}' and '{s}'",
            .{
                type_rules.formatType(op.lhs.ty, &lhs_buf),
                type_rules.formatType(op.rhs.ty, &rhs_buf),
            },
        );
    }
}

/// Check a function call
fn checkCall(self: *Self, call: ir.Instruction.Call) void {
    const loc_info = locToInfo(call.loc);
    const error_file = loc_info.file_path orelse self.file_path;

    // Look up the function in the module
    for (self.module.functions.items) |func| {
        if (std.mem.eql(u8, func.name, call.callee)) {
            const params = func.signature.params;

            // Each parameter maps to one argument (no struct expansion)
            const expected_args: usize = params.len;

            // Found the function - check argument count
            if (call.args.len != expected_args) {
                self.collector.addError(
                    .E204_argument_count_mismatch,
                    error_file,
                    loc_info.range,
                    "function '{s}' expects {d} arguments, got {d}",
                    .{ call.callee, expected_args, call.args.len },
                );
                return;
            }

            // Check argument types directly (no struct expansion)
            for (params, 0..) |param, arg_idx| {
                if (arg_idx >= call.args.len) break;
                const arg = call.args[arg_idx];
                const compat = type_rules.isAssignable(param.ty, arg.ty);

                if (compat == .incompatible) {
                    var expected_buf: [64]u8 = undefined;
                    var actual_buf: [64]u8 = undefined;
                    self.collector.addError(
                        .E205_argument_type_mismatch,
                        error_file,
                        loc_info.range,
                        "argument {d} to '{s}': expected '{s}', got '{s}'",
                        .{
                            arg_idx + 1,
                            call.callee,
                            type_rules.formatType(param.ty, &expected_buf),
                            type_rules.formatType(arg.ty, &actual_buf),
                        },
                    );
                }
            }
            return;
        }
    }

    // Function not found in module - might be external or method
    // For now, we skip these - they'll be caught at runtime if truly undefined
}

/// Check array load
fn checkArrayLoad(self: *Self, arr: ir.Instruction.ArrayOp) void {
    const loc_info = locToInfo(arr.loc);
    const error_file = loc_info.file_path orelse self.file_path;

    // Index must be numeric
    if (!type_rules.isNumeric(arr.index.ty)) {
        var buf: [64]u8 = undefined;
        self.collector.addError(
            .E208_array_index_type,
            error_file,
            loc_info.range,
            "array index must be numeric, got '{s}'",
            .{type_rules.formatType(arr.index.ty, &buf)},
        );
    }
}

/// Check array store
fn checkArrayStore(self: *Self, arr: ir.Instruction.ArrayStore) void {
    const loc_info = locToInfo(arr.loc);
    const error_file = loc_info.file_path orelse self.file_path;

    // Index must be numeric
    if (!type_rules.isNumeric(arr.index.ty)) {
        var buf: [64]u8 = undefined;
        self.collector.addError(
            .E208_array_index_type,
            error_file,
            loc_info.range,
            "array index must be numeric, got '{s}'",
            .{type_rules.formatType(arr.index.ty, &buf)},
        );
    }

    // Value type must match array element type
    const element_ty = getArrayElementType(arr.array_ptr.ty);
    if (element_ty) |elem_ty| {
        const compat = type_rules.isAssignable(elem_ty, arr.value.ty);
        if (compat == .incompatible) {
            var expected_buf: [64]u8 = undefined;
            var actual_buf: [64]u8 = undefined;
            self.collector.addError(
                .E200_type_mismatch,
                error_file,
                loc_info.range,
                "cannot store '{s}' in array of '{s}'",
                .{
                    type_rules.formatType(arr.value.ty, &actual_buf),
                    type_rules.formatType(elem_ty, &expected_buf),
                },
            );
        }
    }
}

/// Location info extracted from IR source location
const LocInfo = struct {
    range: SourceRange,
    file_path: ?[]const u8,
};

/// Convert IR source location to diagnostic SourceRange and optional file_path
fn locToInfo(loc: ?ir.SourceLoc) LocInfo {
    if (loc) |l| {
        return .{
            .range = SourceRange.fromLoc(l.line, l.column),
            .file_path = l.file_path,
        };
    }
    return .{
        .range = SourceRange.none,
        .file_path = null,
    };
}

/// Convert IR source location to diagnostic SourceRange (legacy, no file_path)
fn locToRange(loc: ?ir.SourceLoc) SourceRange {
    if (loc) |l| {
        return SourceRange.fromLoc(l.line, l.column);
    }
    return SourceRange.none;
}

/// Dereference a pointer type
fn derefType(ty: Type) Type {
    return switch (ty) {
        .ptr => |p| p.*,
        else => ty,
    };
}

/// Get the element type of an array type
fn getArrayElementType(ty: Type) ?Type {
    const dereffed = derefType(ty);
    return switch (dereffed) {
        .array => |a| a.element.*,
        else => null,
    };
}

/// Check the module and return true if there are errors
pub fn hasErrors(self: *const Self) bool {
    return self.collector.hasErrors();
}

// Tests
test "type checker initialization" {
    const allocator = std.testing.allocator;
    var collector = DiagnosticCollector.init(allocator);
    defer collector.deinit();

    var module = ir.Module.init(allocator);
    defer module.deinit();

    var checker = init(allocator, &collector, &module, "test.cot");
    checker.check();

    // Should have no errors on empty module
    try std.testing.expect(!checker.hasErrors());
}

// Pull in comprehensive type checker tests
test {
    _ = @import("type_checker_test.zig");
}
