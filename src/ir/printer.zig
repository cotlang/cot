//! Cot IR Printer
//!
//! Outputs a human-readable text representation of the IR.
//! Useful for debugging, testing, and understanding compilation output.
//!
//! Example output:
//! ```
//! ; Module: pricing
//! ; Library: pricing
//!
//! define export @cot_calculate_price(%0: d10.2, %1: d6, %2: d6.2) -> d10.2 {
//! entry:
//!     %3 = mul d10.2 %0, %1
//!     %4 = const d4.2 1.00
//!     %5 = sub d6.2 %4, %2
//!     %6 = mul d10.2 %3, %5
//!     ret %6
//! }
//! ```

const std = @import("std");
const ir = @import("ir.zig");

pub const Printer = struct {
    writer: std.io.AnyWriter,
    indent_level: u32,

    const Self = @This();

    pub fn init(writer: std.io.AnyWriter) Self {
        return .{
            .writer = writer,
            .indent_level = 0,
        };
    }

    /// Print an entire module
    pub fn printModule(self: *Self, module: *const ir.Module) !void {
        // Header
        try self.writer.print("; Module: {s}\n", .{module.name});
        if (module.library_name) |lib| {
            try self.writer.print("; Library: {s}\n", .{lib});
        }
        try self.writer.writeByte('\n');

        // Global structs
        for (module.structs.items) |rec| {
            try self.printStruct(rec);
            try self.writer.writeByte('\n');
        }

        // Global variables
        for (module.globals.items) |global| {
            try self.printGlobal(&global);
        }
        if (module.globals.items.len > 0) {
            try self.writer.writeByte('\n');
        }

        // Functions
        for (module.functions.items) |func| {
            try self.printFunction(func);
            try self.writer.writeByte('\n');
        }
    }

    /// Print a struct type definition
    fn printStruct(self: *Self, rec: *const ir.StructType) !void {
        try self.writer.print("struct {s} {{\n", .{rec.name});
        self.indent_level += 1;

        for (rec.fields) |field| {
            try self.printIndent();
            try self.writer.print("{s}: ", .{field.name});
            try self.printType(field.ty);
            try self.writer.print(" @ {d}\n", .{field.offset});
        }

        self.indent_level -= 1;
        try self.writer.writeAll("}\n");
    }

    /// Print a global variable
    fn printGlobal(self: *Self, global: *const ir.Module.Global) !void {
        try self.writer.print("global @{s}: ", .{global.name});
        try self.printType(global.ty);
        try self.writer.writeByte('\n');
    }

    /// Print a function
    fn printFunction(self: *Self, func: *const ir.Function) !void {
        // Function header
        try self.writer.writeAll("define ");

        // Linkage
        switch (func.linkage) {
            .internal => {},
            .export_c => try self.writer.writeAll("export "),
            .external => try self.writer.writeAll("external "),
        }

        // Name
        if (func.export_name) |exp| {
            try self.writer.print("@{s}", .{exp});
        } else {
            try self.writer.print("@{s}", .{func.name});
        }

        // Parameters
        try self.writer.writeByte('(');
        for (func.signature.params, 0..) |param, i| {
            if (i > 0) try self.writer.writeAll(", ");

            // Direction
            switch (param.direction) {
                .in => {},
                .out => try self.writer.writeAll("out "),
                .inout => try self.writer.writeAll("inout "),
            }

            try self.writer.print("%{s}: ", .{param.name});
            try self.printType(param.ty);
        }
        try self.writer.writeAll(") -> ");

        // Return type
        try self.printType(func.signature.return_type);
        try self.writer.writeAll(" {\n");

        // Blocks
        for (func.blocks.items) |block| {
            try self.printBlock(block);
        }

        try self.writer.writeAll("}\n");
    }

    /// Print a basic block
    fn printBlock(self: *Self, block: *const ir.Block) !void {
        try self.writer.print("{s}:\n", .{block.label});
        self.indent_level += 1;

        for (block.instructions.items) |inst| {
            try self.printIndent();
            try self.printInstruction(&inst);
            try self.writer.writeByte('\n');
        }

        self.indent_level -= 1;
    }

    /// Print an instruction
    fn printInstruction(self: *Self, inst: *const ir.Instruction) !void {
        switch (inst.*) {
            // Memory
            .alloca => |a| {
                try self.printValue(a.result);
                try self.writer.writeAll(" = alloca ");
                try self.printType(a.ty);
                try self.writer.print(" ; {s}", .{a.name});
            },
            .load => |l| {
                try self.printValue(l.result);
                try self.writer.writeAll(" = load ");
                try self.printValue(l.ptr);
            },
            .store => |s| {
                try self.writer.writeAll("store ");
                try self.printValue(s.value);
                try self.writer.writeAll(", ");
                try self.printValue(s.ptr);
            },
            .field_ptr => |f| {
                try self.printValue(f.result);
                try self.writer.writeAll(" = field_ptr ");
                try self.printValue(f.struct_ptr);
                try self.writer.print(", {d}", .{f.field_index});
            },

            // Arithmetic
            .add => |op| try self.printBinaryOp("add", op),
            .sub => |op| try self.printBinaryOp("sub", op),
            .mul => |op| try self.printBinaryOp("mul", op),
            .div => |op| try self.printBinaryOp("div", op),
            .mod => |op| try self.printBinaryOp("mod", op),
            .neg => |op| try self.printUnaryOp("neg", op),

            // Comparison
            .cmp_eq => |op| try self.printBinaryOp("cmp_eq", op),
            .cmp_ne => |op| try self.printBinaryOp("cmp_ne", op),
            .cmp_lt => |op| try self.printBinaryOp("cmp_lt", op),
            .cmp_le => |op| try self.printBinaryOp("cmp_le", op),
            .cmp_gt => |op| try self.printBinaryOp("cmp_gt", op),
            .cmp_ge => |op| try self.printBinaryOp("cmp_ge", op),

            // Logical
            .log_and => |op| try self.printBinaryOp("and", op),
            .log_or => |op| try self.printBinaryOp("or", op),
            .log_not => |op| try self.printUnaryOp("not", op),

            // Bitwise
            .bit_and => |op| try self.printBinaryOp("bit_and", op),
            .bit_or => |op| try self.printBinaryOp("bit_or", op),
            .bit_xor => |op| try self.printBinaryOp("bit_xor", op),
            .bit_not => |op| try self.printUnaryOp("bit_not", op),
            .shl => |op| try self.printBinaryOp("shl", op),
            .shr => |op| try self.printBinaryOp("shr", op),

            // String
            .str_concat => |op| try self.printBinaryOp("str_concat", op),
            .str_compare => |op| try self.printBinaryOp("str_compare", op),
            .str_copy => |s| {
                try self.writer.writeAll("str_copy ");
                try self.printValue(s.dest);
                try self.writer.writeAll(", ");
                try self.printValue(s.src);
            },
            .str_len => |op| try self.printUnaryOp("str_len", op),
            .str_slice => |s| {
                try self.printValue(s.result);
                try self.writer.writeAll(" = str_slice ");
                try self.printValue(s.source);
                try self.writer.writeAll(", ");
                try self.printValue(s.start);
                try self.writer.writeAll(", ");
                try self.printValue(s.length_or_end);
                try self.writer.print(", is_length={}", .{s.is_length});
            },
            .str_slice_store => |s| {
                try self.writer.writeAll("str_slice_store ");
                try self.printValue(s.target);
                try self.writer.writeAll(" (ptr=");
                try self.printValue(s.target_ptr);
                try self.writer.writeAll("), ");
                try self.printValue(s.start);
                try self.writer.writeAll(", ");
                try self.printValue(s.length_or_end);
                try self.writer.print(", is_length={}, ", .{s.is_length});
                try self.printValue(s.value);
            },

            // Control flow
            .br => |b| {
                try self.writer.print("br {s}", .{b.target.label});
            },
            .cond_br => |c| {
                try self.writer.writeAll("cond_br ");
                try self.printValue(c.condition);
                try self.writer.print(", {s}, {s}", .{ c.then_block.label, c.else_block.label });
            },
            .switch_br => |s| {
                try self.writer.writeAll("switch ");
                try self.printValue(s.value);
                try self.writer.print(" [default: {s}", .{s.default.label});
                for (s.cases) |case| {
                    try self.writer.print(", {d}: {s}", .{ case.value, case.target.label });
                }
                try self.writer.writeByte(']');
            },
            .ret => |v| {
                try self.writer.writeAll("ret");
                if (v) |val| {
                    try self.writer.writeByte(' ');
                    try self.printValue(val);
                }
            },

            // Calls
            .call => |c| {
                if (c.result) |r| {
                    try self.printValue(r);
                    try self.writer.writeAll(" = ");
                }
                try self.writer.print("call @{s}(", .{c.callee});
                for (c.args, 0..) |arg, i| {
                    if (i > 0) try self.writer.writeAll(", ");
                    try self.printValue(arg);
                }
                try self.writer.writeByte(')');
            },
            // Note: xcall is converted to regular .call in the IR lowerer

            // Type conversions
            .cast => |c| {
                try self.printValue(c.result);
                try self.writer.writeAll(" = cast ");
                try self.printValue(c.operand);
                try self.writer.writeAll(" to ");
                try self.printType(c.target_type);
            },
            .int_to_float => |op| try self.printUnaryOp("int_to_float", op),
            .float_to_int => |op| try self.printUnaryOp("float_to_int", op),
            .int_extend => |e| {
                try self.printValue(e.result);
                try self.writer.print(" = int_extend{s} ", .{if (e.signed) ".signed" else ".unsigned"});
                try self.printValue(e.operand);
            },
            .int_truncate => |op| try self.printUnaryOp("int_truncate", op),

            // Constants
            .const_int => |c| {
                try self.printValue(c.result);
                try self.writer.writeAll(" = const_int ");
                try self.printType(c.ty);
                try self.writer.print(" {d}", .{c.value});
            },
            .const_float => |c| {
                try self.printValue(c.result);
                try self.writer.writeAll(" = const_float ");
                try self.printType(c.ty);
                try self.writer.print(" {d}", .{c.value});
            },
            .const_string => |c| {
                try self.printValue(c.result);
                try self.writer.print(" = const_string \"{s}\"", .{c.value});
            },
            .const_bool => |c| {
                try self.printValue(c.result);
                try self.writer.print(" = const_bool {}", .{c.value});
            },
            .const_null => |c| {
                try self.printValue(c.result);
                try self.writer.writeAll(" = const_null ");
                try self.printType(c.ty);
            },

            // I/O
            .io_open => |o| {
                try self.writer.writeAll("io_open ");
                try self.printValue(o.channel);
                try self.writer.print(", {s}, ", .{@tagName(o.mode)});
                try self.printValue(o.filename);
            },
            .io_close => |c| {
                try self.writer.writeAll("io_close ");
                try self.printValue(c.channel);
            },
            .io_read => |r| {
                try self.writer.writeAll("io_read ");
                try self.printValue(r.channel);
                try self.writer.writeAll(", ");
                try self.printValue(r.buffer);
                if (r.key) |k| {
                    try self.writer.writeAll(", key=");
                    try self.printValue(k);
                }
            },
            .io_write => |w| {
                try self.writer.writeAll("io_write ");
                try self.printValue(w.channel);
                try self.writer.writeAll(", ");
                try self.printValue(w.buffer);
                if (w.is_insert) {
                    try self.writer.writeAll(" (insert)");
                }
            },
            .io_delete => |d| {
                try self.writer.writeAll("io_delete ");
                try self.printValue(d.channel);
            },
            .io_unlock => |u| {
                try self.writer.writeAll("io_unlock ");
                try self.printValue(u.channel);
            },

            .load_struct_buf => |sb| {
                try self.printValue(sb.result);
                try self.writer.print(" = load_struct_buf \"{s}\"", .{sb.struct_name});
            },

            // Array operations
            .array_load => |al| {
                try self.printValue(al.result);
                try self.writer.writeAll(" = array_load ");
                try self.printValue(al.array_ptr);
                try self.writer.writeAll("[");
                try self.printValue(al.index);
                try self.writer.writeAll("]");
            },
            .array_store => |as| {
                try self.writer.writeAll("array_store ");
                try self.printValue(as.array_ptr);
                try self.writer.writeAll("[");
                try self.printValue(as.index);
                try self.writer.writeAll("] = ");
                try self.printValue(as.value);
            },

            // Exception handling
            .try_begin => |t| {
                try self.writer.print("try_begin catch={s}", .{t.catch_block.label});
            },
            .try_end => {
                try self.writer.writeAll("try_end");
            },
            .catch_begin => |c| {
                if (c.error_type) |et| {
                    try self.writer.print("catch_begin {s}", .{et});
                } else {
                    try self.writer.writeAll("catch_begin (any)");
                }
            },
            .throw => |t| {
                try self.writer.writeAll("throw ");
                try self.printValue(t.value);
            },

            // Optional operations
            .wrap_optional => |op| try self.printUnaryOp("wrap_optional", op),
            .unwrap_optional => |op| try self.printUnaryOp("unwrap_optional", op),
            .is_null => |op| try self.printUnaryOp("is_null", op),

            // Store struct buffer
            .store_struct_buf => |sb| {
                try self.writer.print("store_struct_buf \"{s}\", ", .{sb.struct_name});
                try self.printValue(sb.value);
            },

            // Array length
            .array_len => |op| try self.printUnaryOp("array_len", op),

            // Debug info
            .debug_line => |d| {
                try self.writer.print("; line {d}:{d}", .{ d.line, d.column });
            },
        }
    }

    fn printBinaryOp(self: *Self, name: []const u8, op: ir.Instruction.BinaryOp) !void {
        try self.printValue(op.result);
        try self.writer.print(" = {s} ", .{name});
        try self.printValue(op.lhs);
        try self.writer.writeAll(", ");
        try self.printValue(op.rhs);
    }

    fn printUnaryOp(self: *Self, name: []const u8, op: ir.Instruction.UnaryOp) !void {
        try self.printValue(op.result);
        try self.writer.print(" = {s} ", .{name});
        try self.printValue(op.operand);
    }

    /// Print a value reference
    fn printValue(self: *Self, value: ir.Value) !void {
        try self.writer.print("%{d}", .{value.id});
    }

    /// Print a type
    fn printType(self: *Self, ty: ir.Type) !void {
        switch (ty) {
            .void => try self.writer.writeAll("void"),
            .bool => try self.writer.writeAll("bool"),
            .i8 => try self.writer.writeAll("i8"),
            .i16 => try self.writer.writeAll("i16"),
            .i32 => try self.writer.writeAll("i32"),
            .i64 => try self.writer.writeAll("i64"),
            .u8 => try self.writer.writeAll("u8"),
            .u16 => try self.writer.writeAll("u16"),
            .u32 => try self.writer.writeAll("u32"),
            .u64 => try self.writer.writeAll("u64"),
            .f32 => try self.writer.writeAll("f32"),
            .f64 => try self.writer.writeAll("f64"),
            .string => try self.writer.writeAll("string"),
            .string_fixed => |len| try self.writer.print("string[{d}]", .{len}),
            .decimal => |d| {
                if (d.scale > 0) {
                    try self.writer.print("decimal({d},{d})", .{ d.precision, d.scale });
                } else {
                    try self.writer.print("decimal({d})", .{d.precision});
                }
            },
            .ptr => |inner| {
                try self.writer.writeByte('*');
                try self.printType(inner.*);
            },
            .optional => |inner| {
                try self.writer.writeByte('?');
                try self.printType(inner.*);
            },
            .array => |a| {
                try self.writer.print("[{d}]", .{a.length});
                try self.printType(a.element.*);
            },
            .slice => |elem| {
                try self.writer.writeAll("[]");
                try self.printType(elem.*);
            },
            .@"struct" => |s| try self.writer.print("struct({s})", .{s.name}),
            .function => try self.writer.writeAll("fn"),
        }
    }

    /// Print indentation
    fn printIndent(self: *Self) !void {
        var i: u32 = 0;
        while (i < self.indent_level) : (i += 1) {
            try self.writer.writeAll("    ");
        }
    }
};

/// Convenience function to print a module to a string
pub fn printToString(allocator: std.mem.Allocator, module: *const ir.Module) ![]u8 {
    var list = std.ArrayList(u8).init(allocator);
    var printer = Printer.init(list.writer().any());
    try printer.printModule(module);
    return list.toOwnedSlice();
}

// ============================================================================
// Tests
// ============================================================================

test "print simple function" {
    const allocator = std.testing.allocator;

    const sig = ir.FunctionType{
        .params = &[_]ir.FunctionType.Param{
            .{ .name = "x", .ty = .{ .decimal = .{ .length = 8, .decimal_places = 2 } }, .direction = .in },
            .{ .name = "y", .ty = .{ .decimal = .{ .length = 8, .decimal_places = 2 } }, .direction = .in },
        },
        .return_type = .{ .decimal = .{ .length = 10, .decimal_places = 2 } },
        .is_variadic = false,
    };

    const func = try ir.Function.init(allocator, "add_values", sig);
    defer func.deinit();

    func.setExport("cot_add_values");

    // Add some instructions to entry block
    const x = ir.Value{ .id = 0, .ty = .{ .decimal = .{ .length = 8, .decimal_places = 2 } } };
    const y = ir.Value{ .id = 1, .ty = .{ .decimal = .{ .length = 8, .decimal_places = 2 } } };
    const result = func.newValue(.{ .decimal = .{ .length = 10, .decimal_places = 2 } });

    try func.entry.append(.{ .add = .{ .lhs = x, .rhs = y, .result = result } });
    try func.entry.append(.{ .ret = result });

    // Create module and add function
    var module = ir.Module.init(allocator, "test_module");
    defer module.deinit();
    module.library_name = "test";

    // Note: func is already owned, just add a reference
    try module.functions.append(allocator, func);

    // Don't let module.deinit() free the function since we defer func.deinit()
    _ = module.functions.pop();

    // Print to buffer
    var buf: [1024]u8 = undefined;
    var stream = std.io.fixedBufferStream(&buf);
    var printer = Printer.init(stream.writer().any());
    try printer.printFunction(func);

    const output = stream.getWritten();

    // Verify output contains expected elements
    try std.testing.expect(std.mem.indexOf(u8, output, "export") != null);
    try std.testing.expect(std.mem.indexOf(u8, output, "cot_add_values") != null);
    try std.testing.expect(std.mem.indexOf(u8, output, "d8.2") != null);
    try std.testing.expect(std.mem.indexOf(u8, output, "add") != null);
    try std.testing.expect(std.mem.indexOf(u8, output, "ret") != null);
}
