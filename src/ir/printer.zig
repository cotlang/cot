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

            // Show ref modifier if by reference
            if (param.is_ref) {
                try self.writer.writeAll("ref ");
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

            // Arithmetic (Cranelift names)
            .iadd => |op| try self.printBinaryOp("iadd", op),
            .isub => |op| try self.printBinaryOp("isub", op),
            .imul => |op| try self.printBinaryOp("imul", op),
            .sdiv => |op| try self.printBinaryOp("sdiv", op),
            .udiv => |op| try self.printBinaryOp("udiv", op),
            .srem => |op| try self.printBinaryOp("srem", op),
            .urem => |op| try self.printBinaryOp("urem", op),
            .ineg => |op| try self.printUnaryOp("ineg", op),

            // Rounding (DBL legacy)
            .round => |op| try self.printRoundOp("round", op),
            .trunc => |op| try self.printRoundOp("trunc", op),

            // Comparison (Cranelift icmp with condition code)
            .icmp => |op| try self.printIcmpOp(op),

            // Logical
            .log_and => |op| try self.printBinaryOp("log_and", op),
            .log_or => |op| try self.printBinaryOp("log_or", op),
            .log_not => |op| try self.printUnaryOp("log_not", op),

            // Bitwise (Cranelift names)
            .band => |op| try self.printBinaryOp("band", op),
            .bor => |op| try self.printBinaryOp("bor", op),
            .bxor => |op| try self.printBinaryOp("bxor", op),
            .bnot => |op| try self.printUnaryOp("bnot", op),
            .ishl => |op| try self.printBinaryOp("ishl", op),
            .sshr => |op| try self.printBinaryOp("sshr", op),
            .ushr => |op| try self.printBinaryOp("ushr", op),

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
            .str_byte_at => |s| {
                try self.printValue(s.result);
                try self.writer.writeAll(" = str_byte_at ");
                try self.printValue(s.string);
                try self.writer.writeAll(", ");
                try self.printValue(s.index);
            },

            // Control flow (Cranelift names)
            .jump => |b| {
                try self.writer.print("jump {s}", .{b.target.label});
            },
            .brif => |c| {
                try self.writer.writeAll("brif ");
                try self.printValue(c.condition);
                try self.writer.print(", {s}, {s}", .{ c.then_block.label, c.else_block.label });
            },
            .br_table => |s| {
                try self.writer.writeAll("br_table ");
                try self.printValue(s.value);
                try self.writer.print(" [default: {s}", .{s.default.label});
                for (s.cases) |case| {
                    try self.writer.print(", {d}: {s}", .{ case.value, case.target.label });
                }
                try self.writer.writeByte(']');
            },
            .return_ => |v| {
                try self.writer.writeAll("return");
                if (v) |val| {
                    try self.writer.writeByte(' ');
                    try self.printValue(val);
                }
            },
            .trap => |t| {
                try self.writer.print("trap {s}", .{@tagName(t)});
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
            .call_indirect => |c| {
                if (c.result) |r| {
                    try self.printValue(r);
                    try self.writer.writeAll(" = ");
                }
                try self.writer.writeAll("call_indirect ");
                try self.printValue(c.callee);
                try self.writer.writeByte('(');
                for (c.args, 0..) |arg, i| {
                    if (i > 0) try self.writer.writeAll(", ");
                    try self.printValue(arg);
                }
                try self.writer.writeByte(')');
            },

            // Type conversions (Cranelift names)
            .bitcast => |c| {
                try self.printValue(c.result);
                try self.writer.writeAll(" = bitcast ");
                try self.printValue(c.operand);
                try self.writer.writeAll(" to ");
                try self.printType(c.target_type);
            },
            .fcvt_from_sint => |op| try self.printUnaryOp("fcvt_from_sint", op),
            .fcvt_from_uint => |op| try self.printUnaryOp("fcvt_from_uint", op),
            .fcvt_to_sint => |op| try self.printUnaryOp("fcvt_to_sint", op),
            .fcvt_to_uint => |op| try self.printUnaryOp("fcvt_to_uint", op),
            .sextend => |op| try self.printIntExtend("sextend", op),
            .uextend => |op| try self.printIntExtend("uextend", op),
            .ireduce => |op| try self.printUnaryOp("ireduce", op),
            .format_decimal => |op| {
                try self.printValue(op.result);
                try self.writer.print(" = format_decimal ", .{});
                try self.printValue(op.value);
                try self.writer.print(" width={d}", .{op.width});
            },
            .parse_decimal => |op| {
                try self.printValue(op.result);
                try self.writer.print(" = parse_decimal ", .{});
                try self.printValue(op.value);
            },

            // Constants (Cranelift names)
            .iconst => |c| {
                try self.printValue(c.result);
                try self.writer.writeAll(" = iconst ");
                try self.printType(c.ty);
                try self.writer.print(" {d}", .{c.value});
            },
            .f32const => |c| {
                try self.printValue(c.result);
                try self.writer.print(" = f32const {d}", .{c.value});
            },
            .f64const => |c| {
                try self.printValue(c.result);
                try self.writer.print(" = f64const {d}", .{c.value});
            },
            .const_string => |c| {
                try self.printValue(c.result);
                try self.writer.print(" = const_string \"{s}\"", .{c.value});
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
            .array_load_opt => |al| {
                try self.printValue(al.result);
                try self.writer.writeAll(" = array_load_opt ");
                try self.printValue(al.array_ptr);
                try self.writer.writeAll("?[");
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
            .array_slice => |as| {
                try self.printValue(as.result);
                try self.writer.writeAll(" = array_slice ");
                try self.printValue(as.source);
                try self.writer.writeAll("[");
                try self.printValue(as.start);
                if (as.inclusive) {
                    try self.writer.writeAll("..=");
                } else {
                    try self.writer.writeAll("..");
                }
                try self.printValue(as.end);
                try self.writer.writeAll("]");
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
            .is_type => |op| {
                try self.writer.print("is_type ", .{});
                try self.printValue(op.operand);
                try self.writer.print(", {s} -> ", .{@tagName(op.type_tag)});
                try self.printValue(op.result);
            },

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

            // Map operations
            .map_new => |m| {
                try self.printValue(m.result);
                try self.writer.print(" = map_new flags={d}", .{m.flags});
            },
            .map_set => |m| {
                try self.writer.writeAll("map_set ");
                try self.printValue(m.map);
                try self.writer.writeAll("[");
                try self.printValue(m.key);
                try self.writer.writeAll("] = ");
                try self.printValue(m.value);
            },
            .map_get => |m| {
                try self.printValue(m.result);
                try self.writer.writeAll(" = map_get ");
                try self.printValue(m.map);
                try self.writer.writeAll("[");
                try self.printValue(m.key);
                try self.writer.writeAll("]");
            },
            .map_delete => |m| {
                try self.writer.writeAll("map_delete ");
                try self.printValue(m.map);
                try self.writer.writeAll("[");
                try self.printValue(m.key);
                try self.writer.writeAll("]");
            },
            .map_has => |m| {
                try self.printValue(m.result);
                try self.writer.writeAll(" = map_has ");
                try self.printValue(m.map);
                try self.writer.writeAll("[");
                try self.printValue(m.key);
                try self.writer.writeAll("]");
            },
            .map_len => |m| {
                try self.printValue(m.result);
                try self.writer.writeAll(" = map_len ");
                try self.printValue(m.map);
            },
            .map_clear => |m| {
                try self.writer.writeAll("map_clear ");
                try self.printValue(m.map);
            },
            .map_keys => |m| {
                try self.printValue(m.result);
                try self.writer.writeAll(" = map_keys ");
                try self.printValue(m.map);
            },
            .map_values => |m| {
                try self.printValue(m.result);
                try self.writer.writeAll(" = map_values ");
                try self.printValue(m.map);
            },
            .map_key_at => |m| {
                try self.printValue(m.result);
                try self.writer.writeAll(" = map_key_at ");
                try self.printValue(m.map);
                try self.writer.writeAll(", ");
                try self.printValue(m.index);
            },

            // List operations
            .list_new => |l| {
                try self.printValue(l.result);
                try self.writer.writeAll(" = list_new ");
                try self.printType(l.element_type.*);
            },
            .list_push => |l| {
                try self.writer.writeAll("list_push ");
                try self.printValue(l.list);
                try self.writer.writeAll(", ");
                try self.printValue(l.value);
            },
            .list_pop => |l| {
                try self.printValue(l.result);
                try self.writer.writeAll(" = list_pop ");
                try self.printValue(l.list);
            },
            .list_get => |l| {
                try self.printValue(l.result);
                try self.writer.writeAll(" = list_get ");
                try self.printValue(l.list);
                try self.writer.writeAll("[");
                try self.printValue(l.index);
                try self.writer.writeAll("]");
            },
            .list_set => |l| {
                try self.writer.writeAll("list_set ");
                try self.printValue(l.list);
                try self.writer.writeAll("[");
                try self.printValue(l.index);
                try self.writer.writeAll("] = ");
                try self.printValue(l.value);
            },
            .list_len => |l| {
                try self.printValue(l.result);
                try self.writer.writeAll(" = list_len ");
                try self.printValue(l.list);
            },
            .list_clear => |l| {
                try self.writer.writeAll("list_clear ");
                try self.printValue(l.list);
            },
            .list_to_slice => |l| {
                try self.printValue(l.result);
                try self.writer.writeAll(" = list_to_slice ");
                try self.printValue(l.list);
            },

            // Closure operations
            .make_closure => |c| {
                try self.printValue(c.result);
                try self.writer.print(" = make_closure \"{s}\", ", .{c.func_name});
                try self.printValue(c.env);
            },

            // Conditional selection
            .select => |s| {
                try self.printValue(s.result);
                try self.writer.writeAll(" = select ");
                try self.printValue(s.condition);
                try self.writer.writeAll(", ");
                try self.printValue(s.true_val);
                try self.writer.writeAll(", ");
                try self.printValue(s.false_val);
            },

            // Pointer offset (for field views)
            .ptr_offset => |p| {
                try self.printValue(p.result);
                try self.writer.writeAll(" = ptr_offset ");
                try self.printValue(p.base_ptr);
                try self.writer.print(", {d}", .{p.offset});
            },

            // Weak reference operations
            .weak_ref => |op| try self.printUnaryOp("weak_ref", op),
            .weak_load => |op| try self.printUnaryOp("weak_load", op),

            // ARC operations
            .arc_retain => |op| {
                try self.writer.writeAll("arc_retain ");
                try self.printValue(op.value);
            },
            .arc_release => |op| {
                try self.writer.writeAll("arc_release ");
                try self.printValue(op.value);
            },
            .arc_move => |op| try self.printUnaryOp("arc_move", op),

            // Trait object operations
            .make_trait_object => |m| {
                try self.printValue(m.result);
                try self.writer.print(" = make_trait_object \"{s}\" from ", .{m.trait_name});
                try self.printValue(m.value);
                try self.writer.print(" ({s})", .{m.type_name});
            },
            .call_trait_method => |c| {
                try self.printValue(c.result);
                try self.writer.print(" = call_trait_method \"{s}\" on ", .{c.method_name});
                try self.printValue(c.trait_object);
                try self.writer.writeAll(" (");
                for (c.args, 0..) |arg, i| {
                    if (i > 0) try self.writer.writeAll(", ");
                    try self.printValue(arg);
                }
                try self.writer.writeAll(")");
            },

            // Heap record operations
            .heap_alloc => |h| {
                try self.printValue(h.result);
                try self.writer.writeAll(" = heap_alloc ");
                try self.printType(h.ty);
                try self.writer.print(" (fields: {d})", .{h.field_count});
            },
            .store_field_heap => |s| {
                try self.writer.writeAll("store_field_heap ");
                try self.printValue(s.record);
                try self.writer.print("[{d}] = ", .{s.field_index});
                try self.printValue(s.value);
            },
            .load_field_heap => |l| {
                try self.printValue(l.result);
                try self.writer.writeAll(" = load_field_heap ");
                try self.printValue(l.record);
                try self.writer.print("[{d}]", .{l.field_index});
            },

            // Variant operations (sum types)
            .variant_construct => |v| {
                try self.printValue(v.result);
                try self.writer.print(" = variant_construct tag={d} (", .{v.tag});
                for (v.payload, 0..) |val, i| {
                    if (i > 0) try self.writer.writeAll(", ");
                    try self.printValue(val);
                }
                try self.writer.writeAll(")");
            },
            .variant_get_tag => |v| {
                try self.printValue(v.result);
                try self.writer.writeAll(" = variant_get_tag ");
                try self.printValue(v.variant);
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

    fn printRoundOp(self: *Self, name: []const u8, op: ir.Instruction.RoundOp) !void {
        try self.printValue(op.result);
        try self.writer.print(" = {s} ", .{name});
        try self.printValue(op.value);
        try self.writer.writeAll(", ");
        try self.printValue(op.places);
    }

    fn printIcmpOp(self: *Self, op: ir.Instruction.IcmpOp) !void {
        try self.printValue(op.result);
        try self.writer.print(" = icmp {s} ", .{@tagName(op.cond)});
        try self.printValue(op.lhs);
        try self.writer.writeAll(", ");
        try self.printValue(op.rhs);
    }

    fn printIntExtend(self: *Self, name: []const u8, op: ir.Instruction.IntExtend) !void {
        try self.printValue(op.result);
        try self.writer.print(" = {s} ", .{name});
        try self.printValue(op.operand);
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
            .isize => try self.writer.writeAll("isize"),
            .usize => try self.writer.writeAll("usize"),
            .f32 => try self.writer.writeAll("f32"),
            .f64 => try self.writer.writeAll("f64"),
            .string => try self.writer.writeAll("string"),
            .implied_decimal => |d| {
                if (d.scale > 0) {
                    try self.writer.print("decimal({d},{d})", .{ d.precision, d.scale });
                } else {
                    try self.writer.print("decimal({d})", .{d.precision});
                }
            },
            .fixed_decimal => |d| {
                try self.writer.print("fixed_decimal({d})", .{d.width});
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
            .@"union" => |u| try self.writer.print("union({s})", .{u.name}),
            .function => try self.writer.writeAll("fn"),
            .map => try self.writer.writeAll("Map"),
            .list => |l| {
                try self.writer.writeAll("List<");
                try self.printType(l.element_type.*);
                try self.writer.writeAll(">");
            },
            .heap_record => |s| try self.writer.print("new {s}", .{s.name}),
            .weak => |inner| {
                try self.writer.writeAll("weak ");
                try self.printType(inner.*);
            },
            .trait_object => |t| try self.writer.print("dyn {s}", .{t.trait_name}),
            .variant => try self.writer.writeAll("variant"),
            .@"enum" => |e| try self.writer.print("enum({s})", .{e.name}),
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
            .{ .name = "x", .ty = .{ .implied_decimal = .{ .precision = 8, .scale = 2 } }, .direction = .in },
            .{ .name = "y", .ty = .{ .implied_decimal = .{ .precision = 8, .scale = 2 } }, .direction = .in },
        },
        .return_type = .{ .implied_decimal = .{ .precision = 10, .scale = 2 } },
        .is_variadic = false,
    };

    const func = try ir.Function.init(allocator, "add_values", sig);
    defer func.deinit();

    func.setExport("cot_add_values");

    // Add some instructions to entry block
    const x = ir.Value{ .id = 0, .ty = .{ .implied_decimal = .{ .precision = 8, .scale = 2 } } };
    const y = ir.Value{ .id = 1, .ty = .{ .implied_decimal = .{ .precision = 8, .scale = 2 } } };
    const result = func.newValue(.{ .implied_decimal = .{ .precision = 10, .scale = 2 } });

    try func.entry.append(.{ .iadd = .{ .lhs = x, .rhs = y, .result = result } });
    try func.entry.append(.{ .return_ = result });

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
    try std.testing.expect(std.mem.indexOf(u8, output, "iadd") != null);
    try std.testing.expect(std.mem.indexOf(u8, output, "return") != null);
}
