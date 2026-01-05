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

        // Function calls
        .call => |c| self.checkCall(c),
        .call_indirect => {}, // Indirect calls checked at runtime

        // Array operations
        .array_load => |a| self.checkArrayLoad(a),
        .array_store => |a| self.checkArrayStore(a),

        // These don't need type checking (Cranelift names)
        .alloca, .field_ptr, .jump, .brif, .br_table, .return_, .trap => {},
        .iconst, .f32const, .f64const, .const_string, .const_null => {},
        .fcvt_from_sint, .fcvt_from_uint, .fcvt_to_sint, .fcvt_to_uint, .sextend, .uextend, .ireduce, .bitcast, .format_decimal, .parse_decimal => {},
        .io_open, .io_close, .io_read, .io_write, .io_delete, .io_unlock => {},
        .load_struct_buf, .store_struct_buf, .str_compare, .str_copy, .str_len, .ineg => {},
        .try_begin, .try_end, .catch_begin, .throw => {},
        .wrap_optional, .unwrap_optional, .is_null => {},
        .band, .bor, .bxor, .bnot, .ishl, .sshr, .ushr => {},
        .array_len, .debug_line, .select, .ptr_offset => {},
        .map_new, .map_set, .map_get, .map_delete, .map_has, .map_len, .map_clear, .map_keys, .map_values => {},
        // Weak reference operations
        .weak_ref, .weak_load => {},
        // ARC operations
        .arc_retain, .arc_release, .arc_move => {},
    }
}

/// Check a store instruction
fn checkStore(self: *Self, store: ir.Instruction.Store) void {
    const target_ty = derefType(store.ptr.ty);
    const value_ty = store.value.ty;
    const loc = locToRange(store.loc);

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
                self.file_path,
                loc,
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
                self.file_path,
                loc,
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
    const loc = locToRange(op.loc);

    if (!result.ok) {
        var lhs_buf: [64]u8 = undefined;
        var rhs_buf: [64]u8 = undefined;
        self.collector.addError(
            .E206_incompatible_operands,
            self.file_path,
            loc,
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
    const loc = locToRange(op.loc);

    if (!result.ok) {
        var lhs_buf: [64]u8 = undefined;
        var rhs_buf: [64]u8 = undefined;
        self.collector.addError(
            .E206_incompatible_operands,
            self.file_path,
            loc,
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
    const loc = locToRange(op.loc);

    if (!result.ok) {
        var lhs_buf: [64]u8 = undefined;
        var rhs_buf: [64]u8 = undefined;
        self.collector.addError(
            .E206_incompatible_operands,
            self.file_path,
            loc,
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
    const loc = locToRange(call.loc);

    // Look up the function in the module
    for (self.module.functions.items) |func| {
        if (std.mem.eql(u8, func.name, call.callee)) {
            const params = func.signature.params;

            // Calculate expected argument count - struct params expand to field count
            var expected_args: usize = 0;
            for (params) |param| {
                if (param.ty == .@"struct") {
                    expected_args += param.ty.@"struct".fields.len;
                } else {
                    expected_args += 1;
                }
            }

            // Found the function - check argument count
            if (call.args.len != expected_args) {
                self.collector.addError(
                    .E204_argument_count_mismatch,
                    self.file_path,
                    loc,
                    "function '{s}' expects {d} arguments, got {d}",
                    .{ call.callee, expected_args, call.args.len },
                );
                return;
            }

            // Check argument types - account for struct expansion
            var arg_idx: usize = 0;
            for (params) |param| {
                if (param.ty == .@"struct") {
                    // Struct param expands to individual field args
                    const struct_type = param.ty.@"struct";
                    for (struct_type.fields) |field| {
                        if (arg_idx >= call.args.len) break;
                        const arg = call.args[arg_idx];
                        const compat = type_rules.isAssignable(field.ty, arg.ty);

                        if (compat == .incompatible) {
                            var expected_buf: [64]u8 = undefined;
                            var actual_buf: [64]u8 = undefined;
                            self.collector.addError(
                                .E205_argument_type_mismatch,
                                self.file_path,
                                loc,
                                "argument {d} to '{s}': expected '{s}', got '{s}'",
                                .{
                                    arg_idx + 1,
                                    call.callee,
                                    type_rules.formatType(field.ty, &expected_buf),
                                    type_rules.formatType(arg.ty, &actual_buf),
                                },
                            );
                        }
                        arg_idx += 1;
                    }
                } else {
                    if (arg_idx >= call.args.len) break;
                    const arg = call.args[arg_idx];
                    const compat = type_rules.isAssignable(param.ty, arg.ty);

                    if (compat == .incompatible) {
                        var expected_buf: [64]u8 = undefined;
                        var actual_buf: [64]u8 = undefined;
                        self.collector.addError(
                            .E205_argument_type_mismatch,
                            self.file_path,
                            loc,
                            "argument {d} to '{s}': expected '{s}', got '{s}'",
                            .{
                                arg_idx + 1,
                                call.callee,
                                type_rules.formatType(param.ty, &expected_buf),
                                type_rules.formatType(arg.ty, &actual_buf),
                            },
                        );
                    }
                    arg_idx += 1;
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
    const loc = locToRange(arr.loc);

    // Index must be numeric
    if (!type_rules.isNumeric(arr.index.ty)) {
        var buf: [64]u8 = undefined;
        self.collector.addError(
            .E208_array_index_type,
            self.file_path,
            loc,
            "array index must be numeric, got '{s}'",
            .{type_rules.formatType(arr.index.ty, &buf)},
        );
    }
}

/// Check array store
fn checkArrayStore(self: *Self, arr: ir.Instruction.ArrayStore) void {
    const loc = locToRange(arr.loc);

    // Index must be numeric
    if (!type_rules.isNumeric(arr.index.ty)) {
        var buf: [64]u8 = undefined;
        self.collector.addError(
            .E208_array_index_type,
            self.file_path,
            loc,
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
                self.file_path,
                loc,
                "cannot store '{s}' in array of '{s}'",
                .{
                    type_rules.formatType(arr.value.ty, &actual_buf),
                    type_rules.formatType(elem_ty, &expected_buf),
                },
            );
        }
    }
}

/// Convert IR source location to diagnostic SourceRange
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
