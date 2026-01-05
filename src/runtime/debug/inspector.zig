//! Runtime State Inspector
//!
//! Provides read-only access to VM state for debugging:
//! - Register inspection
//! - Stack inspection
//! - Local variable inspection
//! - Call stack inspection
//! - Current instruction context
//!
//! Designed for use by CLI debug commands and LSP/DAP integration.

const std = @import("std");
const Allocator = std.mem.Allocator;
const Value = @import("../bytecode/value.zig").Value;
const module_mod = @import("../bytecode/module.zig");
const Module = module_mod.Module;
const RoutineDef = module_mod.RoutineDef;
const Constant = module_mod.Constant;
const Opcode = @import("../bytecode/opcodes.zig").Opcode;

/// Information about a local variable
pub const LocalInfo = struct {
    /// Variable name (from debug info)
    name: []const u8,
    /// Slot index in local frame
    slot: u16,
    /// Current value
    value: Value,
    /// Type name (if available)
    type_name: []const u8,
    /// Scope: local, parameter, or global
    scope: Scope,

    pub const Scope = enum { local, parameter, global };

    pub fn format(self: LocalInfo, buf: []u8) []const u8 {
        return self.value.debugRepr(buf);
    }
};

/// Information about a call stack frame
pub const FrameInfo = struct {
    /// Frame index (0 = current, 1 = caller, etc.)
    index: u32,
    /// Routine name
    routine_name: []const u8,
    /// Return address (instruction pointer)
    return_ip: u32,
    /// Source line number
    source_line: u32,
    /// Number of local variables
    local_count: u16,
    /// Number of parameters
    param_count: u8,
    /// Frame pointer offset
    fp_offset: u32,
};

/// Information about the current instruction
pub const InstructionInfo = struct {
    /// Instruction pointer
    ip: u32,
    /// Opcode
    opcode: Opcode,
    /// Raw operand bytes
    operands: []const u8,
    /// Source line number
    source_line: u32,
    /// Disassembled instruction text
    disassembly: []const u8,
    /// Size in bytes
    size: u8,
};

/// Source code context around current line
pub const SourceContext = struct {
    /// Current source line number
    current_line: u32,
    /// Source file path (if available)
    file: ?[]const u8,
    /// Lines of source code
    lines: []const SourceLine,

    pub const SourceLine = struct {
        number: u32,
        text: []const u8,
        is_current: bool,
    };
};

/// Register state with type information
pub const RegisterInfo = struct {
    /// Register index (0-15)
    index: u4,
    /// Current value
    value: Value,

    /// Format register value for display
    pub fn format(self: RegisterInfo, buf: []u8) []const u8 {
        return self.value.debugRepr(buf);
    }

    /// Get type name for the value
    pub fn typeName(self: RegisterInfo) []const u8 {
        return self.value.tag().name();
    }
};

/// VM state snapshot for inspection
pub const VMSnapshot = struct {
    /// Current instruction pointer
    ip: u32,
    /// Current source line
    line: u32,
    /// Stack pointer
    sp: u32,
    /// Frame pointer
    fp: u32,
    /// Call depth
    call_depth: u32,
    /// Current routine name
    routine: ?[]const u8,
    /// Is VM running
    is_running: bool,
    /// Last error (if any)
    last_error: ?[]const u8,
};

/// State inspector - provides read-only access to VM state
pub const StateInspector = struct {
    allocator: Allocator,
    /// Reference to VM registers (const)
    registers: *const [16]Value,
    /// Reference to VM stack (const)
    stack: []const Value,
    /// Stack pointer
    sp: u32,
    /// Frame pointer
    fp: u32,
    /// Current IP
    ip: u32,
    /// Current source line
    line: u32,
    /// Call stack frames
    call_frames: []const CallFrame,
    /// Current module
    module: ?*const Module,

    /// Internal call frame representation
    pub const CallFrame = struct {
        return_ip: u32,
        return_fp: u32,
        routine_name: []const u8,
        param_count: u8,
        local_count: u16,
    };

    const Self = @This();

    // =========================================================================
    // Register Access
    // =========================================================================

    /// Get value in a specific register
    pub fn getRegister(self: *const Self, reg: u4) Value {
        return self.registers[reg];
    }

    /// Get all 16 registers
    pub fn getAllRegisters(self: *const Self) [16]Value {
        return self.registers.*;
    }

    /// Get register info with formatting
    pub fn getRegisterInfo(self: *const Self, reg: u4) RegisterInfo {
        return .{
            .index = reg,
            .value = self.registers[reg],
        };
    }

    /// Format register state as string (r0=42 r1=nil ...)
    pub fn formatRegisters(self: *const Self, buf: []u8) []const u8 {
        var pos: usize = 0;

        for (0..16) |i| {
            const reg: u4 = @intCast(i);
            const val = self.registers[reg];

            // Skip null registers after r7 for brevity
            if (i >= 8 and val.isNull()) continue;

            // Format: "rN=value "
            if (pos + 20 > buf.len) break;

            const written = std.fmt.bufPrint(buf[pos..], "r{d}=", .{i}) catch break;
            pos += written.len;

            var val_buf: [64]u8 = undefined;
            const val_str = val.debugRepr(&val_buf);
            if (pos + val_str.len + 1 > buf.len) break;
            @memcpy(buf[pos..][0..val_str.len], val_str);
            pos += val_str.len;
            buf[pos] = ' ';
            pos += 1;
        }

        return buf[0..pos];
    }

    // =========================================================================
    // Stack Access
    // =========================================================================

    /// Get current stack depth
    pub fn getStackDepth(self: *const Self) u32 {
        return self.sp;
    }

    /// Get value at stack offset from bottom
    pub fn getStackValue(self: *const Self, offset: u32) ?Value {
        if (offset >= self.sp) return null;
        return self.stack[offset];
    }

    /// Get value at offset from frame pointer
    pub fn getLocalValue(self: *const Self, slot: u16) ?Value {
        const offset = self.fp + slot;
        if (offset >= self.sp) return null;
        return self.stack[offset];
    }

    /// Get slice of stack values
    pub fn getStackSlice(self: *const Self, start: u32, count: u32) []const Value {
        const end = @min(start + count, self.sp);
        if (start >= end) return &.{};
        return self.stack[start..end];
    }

    /// Get the top N values from stack
    pub fn getTopOfStack(self: *const Self, count: u32) []const Value {
        if (self.sp < count) return self.stack[0..self.sp];
        return self.stack[self.sp - count .. self.sp];
    }

    // =========================================================================
    // Local Variables
    // =========================================================================

    /// Get all local variables in current frame
    pub fn getLocals(self: *const Self, allocator: Allocator) ![]LocalInfo {
        var list: std.ArrayListUnmanaged(LocalInfo) = .empty;
        errdefer list.deinit(allocator);

        // Get current routine info from module
        const mod = self.module orelse return list.toOwnedSlice(allocator);
        const routine = self.findCurrentRoutine(mod) orelse return list.toOwnedSlice(allocator);

        // Add parameters first
        for (routine.params, 0..) |param, i| {
            const slot: u16 = @intCast(i);
            const value = self.getLocalValue(slot) orelse Value.null_val;
            const name = getIdentifierName(mod, param.name_index) orelse "?";

            try list.append(allocator, .{
                .name = name,
                .slot = slot,
                .value = value,
                .type_name = @tagName(param.data_type),
                .scope = .parameter,
            });
        }

        // Add local variables
        for (routine.locals) |local| {
            const value = self.getLocalValue(local.slot) orelse Value.null_val;
            const name = getIdentifierName(mod, local.name_index) orelse "?";

            try list.append(allocator, .{
                .name = name,
                .slot = local.slot,
                .value = value,
                .type_name = @tagName(local.data_type),
                .scope = .local,
            });
        }

        return list.toOwnedSlice(allocator);
    }

    /// Get a local variable by name
    pub fn getLocalByName(self: *const Self, name: []const u8) ?LocalInfo {
        const mod = self.module orelse return null;
        const routine = self.findCurrentRoutine(mod) orelse return null;

        // Check parameters
        for (routine.params, 0..) |param, i| {
            const param_name = getIdentifierName(mod, param.name_index) orelse continue;
            if (std.mem.eql(u8, param_name, name)) {
                const slot: u16 = @intCast(i);
                return .{
                    .name = param_name,
                    .slot = slot,
                    .value = self.getLocalValue(slot) orelse Value.null_val,
                    .type_name = @tagName(param.data_type),
                    .scope = .parameter,
                };
            }
        }

        // Check locals
        for (routine.locals) |local| {
            const local_name = getIdentifierName(mod, local.name_index) orelse continue;
            if (std.mem.eql(u8, local_name, name)) {
                return .{
                    .name = local_name,
                    .slot = local.slot,
                    .value = self.getLocalValue(local.slot) orelse Value.null_val,
                    .type_name = @tagName(local.data_type),
                    .scope = .local,
                };
            }
        }

        return null;
    }

    // =========================================================================
    // Call Stack
    // =========================================================================

    /// Get call stack depth
    pub fn getCallDepth(self: *const Self) usize {
        return self.call_frames.len;
    }

    /// Get call stack frames
    pub fn getCallStack(self: *const Self, allocator: Allocator) ![]FrameInfo {
        var list: std.ArrayListUnmanaged(FrameInfo) = .empty;
        errdefer list.deinit(allocator);

        for (self.call_frames, 0..) |frame, i| {
            try list.append(allocator, .{
                .index = @intCast(i),
                .routine_name = frame.routine_name,
                .return_ip = frame.return_ip,
                .source_line = 0, // Would need line mapping
                .local_count = frame.local_count,
                .param_count = frame.param_count,
                .fp_offset = frame.return_fp,
            });
        }

        return list.toOwnedSlice(allocator);
    }

    /// Format call stack as backtrace string
    pub fn formatBacktrace(self: *const Self, buf: []u8) []const u8 {
        var pos: usize = 0;

        for (self.call_frames, 0..) |frame, i| {
            const line = std.fmt.bufPrint(buf[pos..], "#{d} {s} at 0x{x:0>4}\n", .{
                i,
                frame.routine_name,
                frame.return_ip,
            }) catch break;
            pos += line.len;
        }

        return buf[0..pos];
    }

    // =========================================================================
    // Instruction Context
    // =========================================================================

    /// Get current instruction info
    pub fn getCurrentInstruction(self: *const Self) ?InstructionInfo {
        const mod = self.module orelse return null;
        if (self.ip >= mod.code.len) return null;

        const opcode: Opcode = @enumFromInt(mod.code[self.ip]);
        const size = getInstructionSize(opcode);

        // Build disassembly string
        var disasm_buf: [128]u8 = undefined;
        const disasm = std.fmt.bufPrint(&disasm_buf, "{s}", .{@tagName(opcode)}) catch "?";

        return .{
            .ip = @intCast(self.ip),
            .opcode = opcode,
            .operands = if (self.ip + size <= mod.code.len)
                mod.code[self.ip + 1 .. self.ip + size]
            else
                &.{},
            .source_line = self.line,
            .disassembly = disasm,
            .size = size,
        };
    }

    /// Get VM snapshot
    pub fn getSnapshot(self: *const Self) VMSnapshot {
        return .{
            .ip = self.ip,
            .line = self.line,
            .sp = self.sp,
            .fp = self.fp,
            .call_depth = @intCast(self.call_frames.len),
            .routine = if (self.call_frames.len > 0)
                self.call_frames[self.call_frames.len - 1].routine_name
            else
                null,
            .is_running = true,
            .last_error = null,
        };
    }

    // =========================================================================
    // Helper Functions
    // =========================================================================

    fn findCurrentRoutine(self: *const Self, mod: *const Module) ?*const RoutineDef {
        for (mod.routines) |*routine| {
            const start = routine.code_offset;
            const end = start + routine.code_length;
            if (self.ip >= start and self.ip < end) {
                return routine;
            }
        }
        return null;
    }

    /// Helper to get identifier name from constant index
    fn getIdentifierName(mod: *const Module, index: u16) ?[]const u8 {
        if (mod.getConstant(index)) |c| {
            return switch (c) {
                .identifier => |name| name,
                else => null,
            };
        }
        return null;
    }
};

/// Get instruction size for an opcode
fn getInstructionSize(opcode: Opcode) u8 {
    return switch (opcode) {
        // 1-byte instructions
        .nop, .halt, .ret, .dup, .drop => 1,

        // 2-byte instructions (opcode + reg)
        .println, .print, .inc, .dec => 2,

        // 3-byte instructions
        .movi, .load_local, .store_local => 3,

        // 4-byte instructions
        .add, .sub, .mul, .div, .mod_ => 4,
        .cmp_eq, .cmp_ne, .cmp_lt, .cmp_le, .cmp_gt, .cmp_ge => 4,
        .log_and, .log_or => 4,
        .load_const, .jmp, .jz, .jnz => 4,
        .debug_line => 4,

        // Variable size - return minimum
        else => 1,
    };
}

// ============================================================================
// Tests
// ============================================================================

test "StateInspector: register access" {
    var registers: [16]Value = undefined;
    for (&registers, 0..) |*r, i| {
        r.* = Value.initInt(@intCast(i * 10));
    }

    const inspector = StateInspector{
        .allocator = std.testing.allocator,
        .registers = &registers,
        .stack = &.{},
        .sp = 0,
        .fp = 0,
        .ip = 0,
        .line = 0,
        .call_frames = &.{},
        .module = null,
    };

    try std.testing.expectEqual(Value.initInt(0), inspector.getRegister(0));
    try std.testing.expectEqual(Value.initInt(50), inspector.getRegister(5));
}

test "StateInspector: format registers" {
    var registers: [16]Value = [_]Value{Value.null_val} ** 16;
    registers[0] = Value.initInt(42);
    registers[1] = Value.initBool(true);
    registers[2] = Value.initInt(100);

    const inspector = StateInspector{
        .allocator = std.testing.allocator,
        .registers = &registers,
        .stack = &.{},
        .sp = 0,
        .fp = 0,
        .ip = 0,
        .line = 0,
        .call_frames = &.{},
        .module = null,
    };

    var buf: [256]u8 = undefined;
    const result = inspector.formatRegisters(&buf);
    try std.testing.expect(result.len > 0);
}
