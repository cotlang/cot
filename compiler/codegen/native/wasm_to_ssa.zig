//! WebAssembly to SSA conversion for AOT compilation.
//!
//! Converts Wasm stack machine operations to SSA form.
//! Used by the AOT pipeline: Wasm -> SSA -> Native.

const std = @import("std");
const wasm_parser = @import("wasm_parser.zig");
const wasm = @import("../wasm_opcodes.zig");
const Func = @import("../../ssa/func.zig").Func;
const Block = @import("../../ssa/block.zig").Block;
const BlockKind = @import("../../ssa/block.zig").BlockKind;
const Value = @import("../../ssa/value.zig").Value;
const Op = @import("../../ssa/op.zig").Op;
const TypeRegistry = @import("../../frontend/types.zig").TypeRegistry;

// ============================================================================
// Stack Machine Simulation
// ============================================================================

const ValueStack = struct {
    items: std.ArrayListUnmanaged(*Value),

    fn init() ValueStack {
        return .{ .items = .{} };
    }

    fn deinit(self: *ValueStack, allocator: std.mem.Allocator) void {
        self.items.deinit(allocator);
    }

    fn push(self: *ValueStack, allocator: std.mem.Allocator, v: *Value) !void {
        try self.items.append(allocator, v);
    }

    fn pop(self: *ValueStack) ?*Value {
        if (self.items.items.len == 0) return null;
        return self.items.pop();
    }

    fn peek(self: *ValueStack) ?*Value {
        if (self.items.items.len == 0) return null;
        return self.items.items[self.items.items.len - 1];
    }
};

// ============================================================================
// Control Flow Stack
// ============================================================================

const ControlFrame = struct {
    opcode: ControlOpcode,
    start_block: *Block,
    end_block: *Block,
    else_block: ?*Block,
    stack_height: usize,
};

const ControlOpcode = enum {
    block,
    loop,
    if_,
};

// ============================================================================
// Converter
// ============================================================================

pub const ConvertError = error{
    InvalidOpcode,
    StackUnderflow,
    OutOfMemory,
};

pub const WasmToSSA = struct {
    allocator: std.mem.Allocator,
    module: *const wasm_parser.WasmModule,

    pub fn init(allocator: std.mem.Allocator, module: *const wasm_parser.WasmModule) WasmToSSA {
        return .{
            .allocator = allocator,
            .module = module,
        };
    }

    /// Clean up converter resources (no-op, held references only).
    pub fn deinit(self: *WasmToSSA) void {
        _ = self;
    }

    /// Convert a function from Wasm to SSA.
    pub fn convert(self: *WasmToSSA, func_idx: usize) !*Func {
        const type_idx = self.module.funcs[func_idx];
        const func_type = self.module.types[type_idx];
        const code = self.module.code[func_idx];

        // Find name from exports, or generate one
        var name: []const u8 = "func";
        for (self.module.exports) |exp| {
            if (exp.kind == .func and exp.index == func_idx) {
                name = exp.name;
                break;
            }
        }

        // Create SSA function
        const func = try self.allocator.create(Func);
        func.* = Func.init(self.allocator, name);

        // Create entry block
        const entry = try func.newBlock(.plain);

        // Create SSA values for parameters
        var locals = std.ArrayListUnmanaged(?*Value){};
        defer locals.deinit(self.allocator);

        for (func_type.params, 0..) |_, i| {
            const arg = try func.newValue(.arg, TypeRegistry.I64, entry, .{});
            arg.aux_int = @intCast(i);
            try entry.addValue(self.allocator, arg);
            try locals.append(self.allocator, arg);
        }

        // Add locals (initialized to zero)
        for (code.locals) |local_decl| {
            for (0..local_decl.count) |_| {
                try locals.append(self.allocator, null); // Will be set on first store
            }
        }

        // Convert bytecode
        var state = ConvertState{
            .allocator = self.allocator,
            .func = func,
            .current_block = entry,
            .value_stack = ValueStack.init(),
            .control_stack = .{},
            .locals = locals,
            .code = code.body,
            .pos = 0,
            .module = self.module,
        };
        defer state.value_stack.deinit(self.allocator);
        defer state.control_stack.deinit(self.allocator);

        try state.convertBody();

        return func;
    }
};

const ConvertState = struct {
    allocator: std.mem.Allocator,
    func: *Func,
    current_block: *Block,
    value_stack: ValueStack,
    control_stack: std.ArrayListUnmanaged(ControlFrame),
    locals: std.ArrayListUnmanaged(?*Value),
    code: []const u8,
    pos: usize,
    module: *const wasm_parser.WasmModule,

    fn convertBody(self: *ConvertState) !void {
        while (self.pos < self.code.len) {
            const opcode = self.code[self.pos];
            self.pos += 1;

            if (opcode == wasm.Op.end) {
                // End of function or block
                if (self.control_stack.items.len > 0) {
                    const frame = self.control_stack.pop().?;
                    try self.current_block.addEdgeTo(self.allocator, frame.end_block);
                    self.current_block = frame.end_block;
                } else {
                    // End of function - create return
                    if (self.value_stack.pop()) |ret_val| {
                        self.current_block.setControl(ret_val);
                    }
                    self.current_block.kind = .ret;
                }
                continue;
            }

            try self.convertInstruction(opcode);
        }
    }

    fn convertInstruction(self: *ConvertState, opcode: u8) !void {
        switch (opcode) {
            // Constants
            wasm.Op.i64_const => {
                const value = self.readSLEB128();
                const v = try self.func.newValue(.const_int, TypeRegistry.I64, self.current_block, .{});
                v.aux_int = value;
                try self.current_block.addValue(self.allocator, v);
                try self.value_stack.push(self.allocator, v);
            },
            wasm.Op.i32_const => {
                const value = self.readSLEB128();
                const v = try self.func.newValue(.const_int, TypeRegistry.I32, self.current_block, .{});
                v.aux_int = value;
                try self.current_block.addValue(self.allocator, v);
                try self.value_stack.push(self.allocator, v);
            },

            // Global access (SP register for stack locals)
            wasm.Op.global_get => {
                const idx = self.readULEB128();
                // Global 0 is typically SP - create a stack pointer reference
                const v = try self.func.newValue(.sp, TypeRegistry.I32, self.current_block, .{});
                v.aux_int = @intCast(idx);
                try self.current_block.addValue(self.allocator, v);
                try self.value_stack.push(self.allocator, v);
            },
            wasm.Op.global_set => {
                const idx = self.readULEB128();
                const value = self.value_stack.pop() orelse return ConvertError.StackUnderflow;
                // For SP updates, create a store to SP
                const v = try self.func.newValue(.store_sp, TypeRegistry.VOID, self.current_block, .{});
                v.aux_int = @intCast(idx);
                v.addArg(value);
                try self.current_block.addValue(self.allocator, v);
            },

            // 32-bit arithmetic (for address calculations)
            wasm.Op.i32_add => try self.emitBinaryOp(.add),
            wasm.Op.i32_sub => try self.emitBinaryOp(.sub),

            // Binary arithmetic
            wasm.Op.i64_add => try self.emitBinaryOp(.add),
            wasm.Op.i64_sub => try self.emitBinaryOp(.sub),
            wasm.Op.i64_mul => try self.emitBinaryOp(.mul),
            wasm.Op.i64_div_s => try self.emitBinaryOp(.div),
            wasm.Op.i64_rem_s => try self.emitBinaryOp(.mod),
            wasm.Op.i64_and => try self.emitBinaryOp(.and_),
            wasm.Op.i64_or => try self.emitBinaryOp(.or_),
            wasm.Op.i64_xor => try self.emitBinaryOp(.xor),
            wasm.Op.i64_shl => try self.emitBinaryOp(.shl),
            wasm.Op.i64_shr_s => try self.emitBinaryOp(.sar),
            wasm.Op.i64_shr_u => try self.emitBinaryOp(.shr),

            // Comparison
            wasm.Op.i64_eq => try self.emitBinaryOp(.eq),
            wasm.Op.i64_ne => try self.emitBinaryOp(.ne),
            wasm.Op.i64_lt_s => try self.emitBinaryOp(.lt),
            wasm.Op.i64_le_s => try self.emitBinaryOp(.le),
            wasm.Op.i64_gt_s => try self.emitBinaryOp(.gt),
            wasm.Op.i64_ge_s => try self.emitBinaryOp(.ge),
            wasm.Op.i64_eqz => try self.emitUnaryOp(.wasm_i64_eqz),

            // Local access
            wasm.Op.local_get => {
                const idx: usize = @intCast(self.readULEB128());
                const local_val = self.locals.items[idx] orelse {
                    // Local not yet set, create zero
                    const zero = try self.func.newValue(.const_int, TypeRegistry.I64, self.current_block, .{});
                    zero.aux_int = 0;
                    try self.current_block.addValue(self.allocator, zero);
                    self.locals.items[idx] = zero;
                    try self.value_stack.push(self.allocator, zero);
                    return;
                };
                try self.value_stack.push(self.allocator, local_val);
            },
            wasm.Op.local_set => {
                const idx: usize = @intCast(self.readULEB128());
                const v = self.value_stack.pop() orelse return ConvertError.StackUnderflow;
                if (idx < self.locals.items.len) {
                    self.locals.items[idx] = v;
                }
            },
            wasm.Op.local_tee => {
                const idx: usize = @intCast(self.readULEB128());
                const v = self.value_stack.peek() orelse return ConvertError.StackUnderflow;
                if (idx < self.locals.items.len) {
                    self.locals.items[idx] = v;
                }
            },

            // Control flow
            wasm.Op.block => {
                _ = self.code[self.pos]; // block type
                self.pos += 1;
                const end_block = try self.func.newBlock(.plain);
                try self.control_stack.append(self.allocator, .{
                    .opcode = .block,
                    .start_block = self.current_block,
                    .end_block = end_block,
                    .else_block = null,
                    .stack_height = self.value_stack.items.items.len,
                });
            },
            wasm.Op.loop => {
                _ = self.code[self.pos]; // block type
                self.pos += 1;
                const loop_block = try self.func.newBlock(.plain);
                const end_block = try self.func.newBlock(.plain);
                try self.current_block.addEdgeTo(self.allocator, loop_block);
                try self.control_stack.append(self.allocator, .{
                    .opcode = .loop,
                    .start_block = loop_block,
                    .end_block = end_block,
                    .else_block = null,
                    .stack_height = self.value_stack.items.items.len,
                });
                self.current_block = loop_block;
            },
            wasm.Op.if_op => {
                _ = self.code[self.pos]; // block type
                self.pos += 1;
                const cond = self.value_stack.pop() orelse return ConvertError.StackUnderflow;
                const then_block = try self.func.newBlock(.plain);
                const else_block = try self.func.newBlock(.plain);
                const end_block = try self.func.newBlock(.plain);

                self.current_block.kind = .if_;
                self.current_block.setControl(cond);
                try self.current_block.addEdgeTo(self.allocator, then_block);
                try self.current_block.addEdgeTo(self.allocator, else_block);

                try self.control_stack.append(self.allocator, .{
                    .opcode = .if_,
                    .start_block = self.current_block,
                    .end_block = end_block,
                    .else_block = else_block,
                    .stack_height = self.value_stack.items.items.len,
                });
                self.current_block = then_block;
            },
            wasm.Op.else_op => {
                if (self.control_stack.items.len > 0) {
                    var frame = &self.control_stack.items[self.control_stack.items.len - 1];
                    try self.current_block.addEdgeTo(self.allocator, frame.end_block);
                    if (frame.else_block) |else_blk| {
                        self.current_block = else_blk;
                        frame.else_block = null;
                    }
                }
            },
            wasm.Op.br => {
                const label = self.readULEB128();
                if (self.control_stack.items.len > label) {
                    const frame = self.control_stack.items[self.control_stack.items.len - 1 - @as(usize, @intCast(label))];
                    const target = if (frame.opcode == .loop) frame.start_block else frame.end_block;
                    try self.current_block.addEdgeTo(self.allocator, target);
                }
            },
            wasm.Op.br_if => {
                const label = self.readULEB128();
                const cond = self.value_stack.pop() orelse return ConvertError.StackUnderflow;
                if (self.control_stack.items.len > label) {
                    const frame = self.control_stack.items[self.control_stack.items.len - 1 - @as(usize, @intCast(label))];
                    const target = if (frame.opcode == .loop) frame.start_block else frame.end_block;
                    const cont_block = try self.func.newBlock(.plain);

                    self.current_block.kind = .if_;
                    self.current_block.setControl(cond);
                    try self.current_block.addEdgeTo(self.allocator, target);
                    try self.current_block.addEdgeTo(self.allocator, cont_block);
                    self.current_block = cont_block;
                }
            },
            wasm.Op.return_op => {
                if (self.value_stack.pop()) |ret_val| {
                    self.current_block.setControl(ret_val);
                }
                self.current_block.kind = .ret;
            },

            // Function calls
            wasm.Op.call => {
                const func_idx = self.readULEB128();

                // Get function type to determine parameter count
                const type_idx = self.module.funcs[@intCast(func_idx)];
                const func_type = self.module.types[type_idx];
                const param_count = func_type.params.len;

                // Pop arguments from stack into temp buffer
                var args_buf: [16]*Value = undefined;
                for (0..param_count) |i| {
                    args_buf[param_count - 1 - i] = self.value_stack.pop() orelse return ConvertError.StackUnderflow;
                }

                const call_val = try self.func.newValue(.static_call, TypeRegistry.I64, self.current_block, .{});
                call_val.aux_int = @intCast(func_idx);

                // Add arguments to call value
                for (args_buf[0..param_count]) |arg| {
                    call_val.addArg(arg);
                }

                // Look up function name from exports
                const func_name = self.getFuncName(@intCast(func_idx));
                call_val.aux = .{ .string = func_name };

                try self.current_block.addValue(self.allocator, call_val);

                // Push result if function returns a value
                if (func_type.results.len > 0) {
                    try self.value_stack.push(self.allocator, call_val);
                }
            },

            // Drop and select
            wasm.Op.drop => {
                _ = self.value_stack.pop();
            },
            wasm.Op.select => {
                const cond = self.value_stack.pop() orelse return ConvertError.StackUnderflow;
                const false_val = self.value_stack.pop() orelse return ConvertError.StackUnderflow;
                const true_val = self.value_stack.pop() orelse return ConvertError.StackUnderflow;
                const v = try self.func.newValue(.wasm_select, TypeRegistry.I64, self.current_block, .{});
                v.addArg(cond);
                v.addArg(true_val);
                v.addArg(false_val);
                try self.current_block.addValue(self.allocator, v);
                try self.value_stack.push(self.allocator, v);
            },

            wasm.Op.nop => {},

            // Memory operations
            wasm.Op.i64_load => {
                _ = self.readULEB128(); // align
                const offset = self.readULEB128(); // offset
                const addr = self.value_stack.pop() orelse return ConvertError.StackUnderflow;
                const v = try self.func.newValue(.load, TypeRegistry.I64, self.current_block, .{});
                v.addArg(addr);
                v.aux_int = @intCast(offset);
                try self.current_block.addValue(self.allocator, v);
                try self.value_stack.push(self.allocator, v);
            },
            wasm.Op.i32_load => {
                _ = self.readULEB128();
                const offset = self.readULEB128();
                const addr = self.value_stack.pop() orelse return ConvertError.StackUnderflow;
                const v = try self.func.newValue(.load, TypeRegistry.I32, self.current_block, .{});
                v.addArg(addr);
                v.aux_int = @intCast(offset);
                try self.current_block.addValue(self.allocator, v);
                try self.value_stack.push(self.allocator, v);
            },
            wasm.Op.i64_store => {
                _ = self.readULEB128(); // align
                const offset = self.readULEB128(); // offset
                const value = self.value_stack.pop() orelse return ConvertError.StackUnderflow;
                const addr = self.value_stack.pop() orelse return ConvertError.StackUnderflow;
                const v = try self.func.newValue(.store, TypeRegistry.VOID, self.current_block, .{});
                v.addArg(addr);
                v.addArg(value);
                v.aux_int = @intCast(offset);
                try self.current_block.addValue(self.allocator, v);
            },
            wasm.Op.i32_store => {
                _ = self.readULEB128();
                const offset = self.readULEB128();
                const value = self.value_stack.pop() orelse return ConvertError.StackUnderflow;
                const addr = self.value_stack.pop() orelse return ConvertError.StackUnderflow;
                const v = try self.func.newValue(.store, TypeRegistry.VOID, self.current_block, .{});
                v.addArg(addr);
                v.addArg(value);
                v.aux_int = @intCast(offset);
                try self.current_block.addValue(self.allocator, v);
            },

            else => {
                // Skip unknown opcodes
            },
        }
    }

    fn emitBinaryOp(self: *ConvertState, op: Op) !void {
        const rhs = self.value_stack.pop() orelse return ConvertError.StackUnderflow;
        const lhs = self.value_stack.pop() orelse return ConvertError.StackUnderflow;
        const v = try self.func.newValue(op, TypeRegistry.I64, self.current_block, .{});
        v.addArg(lhs);
        v.addArg(rhs);
        try self.current_block.addValue(self.allocator, v);
        try self.value_stack.push(self.allocator, v);
    }

    fn emitUnaryOp(self: *ConvertState, op: Op) !void {
        const arg = self.value_stack.pop() orelse return ConvertError.StackUnderflow;
        const v = try self.func.newValue(op, TypeRegistry.I64, self.current_block, .{});
        v.addArg(arg);
        try self.current_block.addValue(self.allocator, v);
        try self.value_stack.push(self.allocator, v);
    }

    fn readULEB128(self: *ConvertState) u64 {
        var result: u64 = 0;
        var shift: u6 = 0;
        while (self.pos < self.code.len) {
            const byte = self.code[self.pos];
            self.pos += 1;
            result |= @as(u64, byte & 0x7F) << shift;
            if (byte & 0x80 == 0) return result;
            shift +|= 7;
        }
        return result;
    }

    fn readSLEB128(self: *ConvertState) i64 {
        var result: i64 = 0;
        var shift: u6 = 0;
        var byte: u8 = 0;
        while (self.pos < self.code.len) {
            byte = self.code[self.pos];
            self.pos += 1;
            const low7: u7 = @truncate(byte);
            result |= @as(i64, low7) << shift;
            shift +|= 7;
            if (byte & 0x80 == 0) break;
        }
        if (shift < 64 and (byte & 0x40) != 0) {
            result |= ~@as(i64, 0) << shift;
        }
        return result;
    }

    /// Get function name from exports, or return "unknown" for internal functions.
    fn getFuncName(self: *ConvertState, func_idx: usize) []const u8 {
        for (self.module.exports) |exp| {
            if (exp.kind == .func and exp.index == func_idx) {
                return exp.name;
            }
        }
        return "unknown";
    }
};

// ============================================================================
// Tests
// ============================================================================

const testing = std.testing;

test "convert simple constant function" {
    // Module: (func (result i64) (i64.const 42))
    const bytes = [_]u8{
        0x00, 0x61, 0x73, 0x6D, // magic
        0x01, 0x00, 0x00, 0x00, // version
        // Type section: fn() -> i64
        0x01, 0x05, 0x01, 0x60, 0x00, 0x01, 0x7E,
        // Function section
        0x03, 0x02, 0x01, 0x00,
        // Code section
        0x0A, 0x06, 0x01,
        0x04, // body size
        0x00, // 0 locals
        0x42, 0x2A, // i64.const 42
        0x0B, // end
    };

    var module = try wasm_parser.parse(testing.allocator, &bytes);
    defer module.deinit();

    var converter = WasmToSSA.init(testing.allocator, &module);
    const func = try converter.convert(0);
    defer {
        func.deinit();
        testing.allocator.destroy(func);
    }

    // Should have at least one block
    try testing.expect(func.blocks.items.len >= 1);
}

test "convert add function" {
    // Module: (func (param i64 i64) (result i64) (i64.add (local.get 0) (local.get 1)))
    const bytes = [_]u8{
        0x00, 0x61, 0x73, 0x6D, // magic
        0x01, 0x00, 0x00, 0x00, // version
        // Type section: fn(i64, i64) -> i64
        0x01, 0x07, 0x01, 0x60, 0x02, 0x7E, 0x7E, 0x01, 0x7E,
        // Function section
        0x03, 0x02, 0x01, 0x00,
        // Code section
        0x0A, 0x09, 0x01,
        0x07, // body size
        0x00, // 0 locals
        0x20, 0x00, // local.get 0
        0x20, 0x01, // local.get 1
        0x7C, // i64.add
        0x0B, // end
    };

    var module = try wasm_parser.parse(testing.allocator, &bytes);
    defer module.deinit();

    var converter = WasmToSSA.init(testing.allocator, &module);
    const func = try converter.convert(0);
    defer {
        func.deinit();
        testing.allocator.destroy(func);
    }

    // Should have entry block with arg ops and add op
    try testing.expect(func.blocks.items.len >= 1);
    const entry = func.entry.?;
    try testing.expect(entry.values.items.len >= 3); // 2 args + 1 add
}

test "WasmToSSA init" {
    var module = wasm_parser.WasmModule{
        .allocator = testing.allocator,
        .types = &.{},
        .funcs = &.{},
        .memory = null,
        .globals = &.{},
        .exports = &.{},
        .code = &.{},
    };

    const converter = WasmToSSA.init(testing.allocator, &module);
    _ = converter;
}
