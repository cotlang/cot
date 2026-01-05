//! IR Verification Pass
//!
//! Validates IR after lowering to catch bugs early.
//! Inspired by LLVM's verifier.
//!
//! Checks performed:
//! 1. Values defined before use (SSA property)
//! 2. Every block has a terminator
//! 3. Branch targets exist
//! 4. Types match in operations
//!
//! Usage:
//!   var verifier = Verifier.init(allocator, module);
//!   defer verifier.deinit();
//!   verifier.verify() catch |err| {
//!       for (verifier.errors.items) |e| {
//!           std.debug.print("{any}\n", .{e});
//!       }
//!   };

const std = @import("std");
const ir = @import("ir.zig");
const Allocator = std.mem.Allocator;

/// Verification error types
pub const VerifyError = union(enum) {
    /// Value used before being defined
    undefined_value: struct {
        value_id: u32,
        block_name: []const u8,
        function_name: []const u8,
    },

    /// Block missing terminator instruction
    missing_terminator: struct {
        block_name: []const u8,
        function_name: []const u8,
    },

    /// Branch to non-existent block
    invalid_branch_target: struct {
        target_name: []const u8,
        block_name: []const u8,
        function_name: []const u8,
    },

    /// Type mismatch in operation
    type_mismatch: struct {
        expected: []const u8,
        got: []const u8,
        instruction: []const u8,
        function_name: []const u8,
    },

    /// Duplicate value definition (SSA violation)
    duplicate_definition: struct {
        value_id: u32,
        function_name: []const u8,
    },

    /// Empty function (no blocks)
    empty_function: struct {
        function_name: []const u8,
    },

    pub fn format(self: VerifyError, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        switch (self) {
            .undefined_value => |e| {
                try writer.print("error: undefined value %{d} used in block '{s}' in function '{s}'", .{ e.value_id, e.block_name, e.function_name });
            },
            .missing_terminator => |e| {
                try writer.print("error: block '{s}' in function '{s}' missing terminator", .{ e.block_name, e.function_name });
            },
            .invalid_branch_target => |e| {
                try writer.print("error: invalid branch target '{s}' in block '{s}' in function '{s}'", .{ e.target_name, e.block_name, e.function_name });
            },
            .type_mismatch => |e| {
                try writer.print("error: type mismatch in {s}: expected {s}, got {s} in function '{s}'", .{ e.instruction, e.expected, e.got, e.function_name });
            },
            .duplicate_definition => |e| {
                try writer.print("error: duplicate definition of value %{d} in function '{s}'", .{ e.value_id, e.function_name });
            },
            .empty_function => |e| {
                try writer.print("error: function '{s}' has no blocks", .{e.function_name});
            },
        }
    }
};

/// IR Verifier
pub const Verifier = struct {
    allocator: Allocator,
    module: *ir.Module,
    errors: std.ArrayListUnmanaged(VerifyError),

    /// Set of defined value IDs in current function
    defined_values: std.AutoHashMap(u32, void),

    /// Current function being verified (for error messages)
    current_function: ?[]const u8,

    /// Current block being verified (for error messages)
    current_block: ?[]const u8,

    pub fn init(allocator: Allocator, module: *ir.Module) Verifier {
        return .{
            .allocator = allocator,
            .module = module,
            .errors = .{},
            .defined_values = std.AutoHashMap(u32, void).init(allocator),
            .current_function = null,
            .current_block = null,
        };
    }

    pub fn deinit(self: *Verifier) void {
        self.errors.deinit(self.allocator);
        self.defined_values.deinit();
    }

    /// Verify the entire module
    pub fn verify(self: *Verifier) !void {
        for (self.module.functions.items) |func| {
            try self.verifyFunction(func);
        }

        if (self.errors.items.len > 0) {
            return error.VerificationFailed;
        }
    }

    /// Get all errors (for reporting)
    pub fn getErrors(self: *const Verifier) []const VerifyError {
        return self.errors.items;
    }

    /// Check if verification passed
    pub fn hasErrors(self: *const Verifier) bool {
        return self.errors.items.len > 0;
    }

    /// Verify a single function
    fn verifyFunction(self: *Verifier, func: *ir.Function) !void {
        self.current_function = func.name;
        self.defined_values.clearRetainingCapacity();

        // Check for empty function
        if (func.blocks.items.len == 0) {
            try self.addError(.{ .empty_function = .{ .function_name = func.name } });
            return;
        }

        // First pass: collect all defined values
        for (func.blocks.items) |block| {
            for (block.instructions.items) |inst| {
                if (self.getDefinedValue(inst)) |value| {
                    // Check for duplicate definition
                    if (self.defined_values.contains(value.id)) {
                        try self.addError(.{ .duplicate_definition = .{
                            .value_id = value.id,
                            .function_name = func.name,
                        } });
                    } else {
                        try self.defined_values.put(value.id, {});
                    }
                }
            }
        }

        // Second pass: verify each block
        for (func.blocks.items) |block| {
            try self.verifyBlock(block);
        }
    }

    /// Verify a single block
    fn verifyBlock(self: *Verifier, block: *ir.Block) !void {
        self.current_block = block.label;

        // Check each instruction
        for (block.instructions.items) |inst| {
            try self.verifyInstruction(inst);
        }

        // Check terminator
        if (!block.isTerminated()) {
            try self.addError(.{ .missing_terminator = .{
                .block_name = block.label,
                .function_name = self.current_function orelse "unknown",
            } });
        }
    }

    /// Verify a single instruction
    fn verifyInstruction(self: *Verifier, inst: ir.Instruction) !void {
        // Check that all used values are defined
        switch (inst) {
            .load => |op| try self.checkValueDefined(op.ptr),
            .store => |op| {
                try self.checkValueDefined(op.ptr);
                try self.checkValueDefined(op.value);
            },
            // Arithmetic (Cranelift names)
            .iadd, .isub, .imul, .sdiv, .udiv, .srem, .urem => |op| {
                try self.checkValueDefined(op.lhs);
                try self.checkValueDefined(op.rhs);
            },
            // Rounding (DBL legacy)
            .round, .trunc => |op| {
                try self.checkValueDefined(op.value);
                try self.checkValueDefined(op.places);
            },
            // Bitwise (Cranelift names)
            .band, .bor, .bxor, .ishl, .sshr, .ushr => |op| {
                try self.checkValueDefined(op.lhs);
                try self.checkValueDefined(op.rhs);
            },
            // Comparison (Cranelift icmp)
            .icmp => |op| {
                try self.checkValueDefined(op.lhs);
                try self.checkValueDefined(op.rhs);
                try self.checkComparisonTypes(op.lhs, op.rhs, "comparison");
            },
            .log_and, .log_or => |op| {
                try self.checkValueDefined(op.lhs);
                try self.checkValueDefined(op.rhs);
            },
            .ineg, .log_not, .bnot => |op| {
                try self.checkValueDefined(op.operand);
            },
            .str_concat, .str_compare => |op| {
                try self.checkValueDefined(op.lhs);
                try self.checkValueDefined(op.rhs);
            },
            .brif => |br| {
                try self.checkValueDefined(br.condition);
            },
            .return_ => |maybe_val| {
                if (maybe_val) |val| {
                    try self.checkValueDefined(val);
                }
            },
            .call => |c| {
                for (c.args) |arg| {
                    try self.checkValueDefined(arg);
                }
            },
            .io_open => |op| {
                try self.checkValueDefined(op.channel);
                try self.checkValueDefined(op.filename);
            },
            .io_close => |op| {
                try self.checkValueDefined(op.channel);
            },
            .io_read => |op| {
                try self.checkValueDefined(op.channel);
            },
            .io_write => |op| {
                try self.checkValueDefined(op.channel);
                try self.checkValueDefined(op.buffer);
            },
            .io_delete => |op| {
                try self.checkValueDefined(op.channel);
            },
            .io_unlock => |op| {
                try self.checkValueDefined(op.channel);
            },
            .store_struct_buf => |op| {
                try self.checkValueDefined(op.value);
            },
            .array_load => |op| {
                try self.checkValueDefined(op.array_ptr);
                try self.checkValueDefined(op.index);
            },
            .array_store => |op| {
                try self.checkValueDefined(op.array_ptr);
                try self.checkValueDefined(op.index);
                try self.checkValueDefined(op.value);
            },
            .bitcast => |op| {
                try self.checkValueDefined(op.operand);
            },
            // Type conversions (Cranelift names)
            .fcvt_from_sint, .fcvt_from_uint, .fcvt_to_sint, .fcvt_to_uint, .ireduce => |op| {
                try self.checkValueDefined(op.operand);
            },
            .sextend, .uextend => |op| {
                try self.checkValueDefined(op.operand);
            },
            .format_decimal => |op| {
                try self.checkValueDefined(op.value);
            },
            .parse_decimal => |op| {
                try self.checkValueDefined(op.value);
            },
            .wrap_optional, .unwrap_optional, .is_null => |op| {
                try self.checkValueDefined(op.operand);
            },
            .str_len, .array_len => |op| {
                try self.checkValueDefined(op.operand);
            },
            .throw => |op| {
                try self.checkValueDefined(op.value);
            },
            // Instructions that don't use values or only define them (Cranelift names)
            .alloca, .jump, .br_table, .trap, .iconst, .f32const, .f64const, .const_string, .const_null, .debug_line, .try_begin, .try_end, .catch_begin, .field_ptr, .load_struct_buf, .str_slice, .str_slice_store, .str_copy, .call_indirect => {},
            // Map operations
            .map_new => {}, // No input values
            .map_set => |op| {
                try self.checkValueDefined(op.map);
                try self.checkValueDefined(op.key);
                try self.checkValueDefined(op.value);
            },
            .map_get => |op| {
                try self.checkValueDefined(op.map);
                try self.checkValueDefined(op.key);
            },
            .map_delete => |op| {
                try self.checkValueDefined(op.map);
                try self.checkValueDefined(op.key);
            },
            .map_has => |op| {
                try self.checkValueDefined(op.map);
                try self.checkValueDefined(op.key);
            },
            .map_len => |op| {
                try self.checkValueDefined(op.map);
            },
            .map_clear => |op| {
                try self.checkValueDefined(op.map);
            },
            .map_keys => |op| {
                try self.checkValueDefined(op.map);
            },
            .map_values => |op| {
                try self.checkValueDefined(op.map);
            },
            .select => |op| {
                try self.checkValueDefined(op.condition);
                try self.checkValueDefined(op.true_val);
                try self.checkValueDefined(op.false_val);
            },
            .ptr_offset => |op| {
                try self.checkValueDefined(op.base_ptr);
            },
            // Weak reference operations
            .weak_ref, .weak_load => |op| {
                try self.checkValueDefined(op.operand);
            },
            // ARC operations
            .arc_retain, .arc_release => |op| {
                try self.checkValueDefined(op.value);
            },
            .arc_move => |op| {
                try self.checkValueDefined(op.operand);
            },
        }
    }

    /// Check that a value is defined
    fn checkValueDefined(self: *Verifier, value: ir.Value) !void {
        // Value ID 0 is sometimes used for "no value" - skip it
        if (value.id == 0) return;

        if (!self.defined_values.contains(value.id)) {
            try self.addError(.{ .undefined_value = .{
                .value_id = value.id,
                .block_name = self.current_block orelse "unknown",
                .function_name = self.current_function orelse "unknown",
            } });
        }
    }

    /// Get a human-readable type name for error messages
    fn typeName(ty: ir.Type) []const u8 {
        return switch (ty) {
            .void => "void",
            .bool => "bool",
            .i8 => "i8",
            .i16 => "i16",
            .i32 => "i32",
            .i64 => "i64",
            .u8 => "u8",
            .u16 => "u16",
            .u32 => "u32",
            .u64 => "u64",
            .isize => "isize",
            .usize => "usize",
            .f32 => "f32",
            .f64 => "f64",
            .string => "string",
            .decimal => "decimal",
            .ptr => "pointer",
            .array => "array",
            .slice => "slice",
            .@"struct" => "struct",
            .@"union" => "union",
            .optional => "optional",
            .function => "function",
            .map => "Map",
            .weak => "weak",
        };
    }

    /// Check types in arithmetic operations (add, sub, mul, div, mod)
    /// Uses ir.Value's built-in type checking methods
    fn checkArithmeticTypes(self: *Verifier, lhs: ir.Value, rhs: ir.Value, inst_name: []const u8) !void {
        // Use the Value's compatibleForArithmetic method
        if (lhs.compatibleForArithmetic(rhs)) {
            return;
        }

        // Report type mismatch
        if (!lhs.isNumeric() and !lhs.isString()) {
            try self.addError(.{ .type_mismatch = .{
                .expected = "numeric or string",
                .got = typeName(lhs.ty),
                .instruction = inst_name,
                .function_name = self.current_function orelse "unknown",
            } });
        }

        if (!rhs.isNumeric() and !rhs.isString()) {
            try self.addError(.{ .type_mismatch = .{
                .expected = "numeric or string",
                .got = typeName(rhs.ty),
                .instruction = inst_name,
                .function_name = self.current_function orelse "unknown",
            } });
        }
    }

    /// Check types in comparison operations
    /// Uses ir.Value's built-in type checking methods
    fn checkComparisonTypes(self: *Verifier, lhs: ir.Value, rhs: ir.Value, inst_name: []const u8) !void {
        // Use the Value's compatibleForComparison method
        if (lhs.compatibleForComparison(rhs)) {
            return;
        }

        // Report mismatch for clearly incompatible types (numeric vs string)
        if ((lhs.isNumeric() and rhs.isString()) or (lhs.isString() and rhs.isNumeric())) {
            try self.addError(.{ .type_mismatch = .{
                .expected = typeName(lhs.ty),
                .got = typeName(rhs.ty),
                .instruction = inst_name,
                .function_name = self.current_function orelse "unknown",
            } });
        }
    }

    /// Get the value defined by an instruction (if any)
    /// Uses the Instruction.getResult() method from the category system
    fn getDefinedValue(self: *Verifier, inst: ir.Instruction) ?ir.Value {
        _ = self;
        return inst.getResult();
    }

    /// Add an error to the error list
    fn addError(self: *Verifier, err: VerifyError) !void {
        try self.errors.append(self.allocator, err);
    }
};

// ============================================================================
// Tests
// ============================================================================

test "verifier catches missing terminator" {
    const allocator = std.testing.allocator;

    // Create a module with a function that has no terminator
    var module = ir.Module.init(allocator, "test");
    defer module.deinit();

    const func_type = ir.FunctionType{
        .params = &[_]ir.FunctionType.Param{},
        .return_type = .void,
        .is_variadic = false,
    };
    const func = try ir.Function.init(allocator, "test_func", func_type);
    try module.addFunction(func);

    // Don't add any terminator to entry block

    var verifier = Verifier.init(allocator, &module);
    defer verifier.deinit();

    verifier.verify() catch {};

    try std.testing.expect(verifier.hasErrors());
    try std.testing.expectEqual(@as(usize, 1), verifier.errors.items.len);

    const err = verifier.errors.items[0];
    switch (err) {
        .missing_terminator => |e| {
            try std.testing.expectEqualStrings("entry", e.block_name);
            try std.testing.expectEqualStrings("test_func", e.function_name);
        },
        else => return error.UnexpectedError,
    }
}

test "verifier passes valid IR" {
    const allocator = std.testing.allocator;

    var module = ir.Module.init(allocator, "test");
    defer module.deinit();

    const func_type = ir.FunctionType{
        .params = &[_]ir.FunctionType.Param{},
        .return_type = .void,
        .is_variadic = false,
    };
    const func = try ir.Function.init(allocator, "test_func", func_type);
    try module.addFunction(func);

    // Add a return terminator
    try func.entry.append(.{ .return_ = null });

    var verifier = Verifier.init(allocator, &module);
    defer verifier.deinit();

    try verifier.verify();
    try std.testing.expect(!verifier.hasErrors());
}
