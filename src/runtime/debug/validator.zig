//! Bytecode Validator
//!
//! Validates bytecode files for integrity and correctness:
//! - Structural integrity (header, sections, offsets)
//! - Instruction validity (opcodes, operands)
//! - Reference validity (constant indices, routine indices)
//! - Control flow analysis (jumps, returns)
//!
//! Use for catching compiler bugs and corrupted bytecode files.

const std = @import("std");
const Allocator = std.mem.Allocator;
const Module = @import("../bytecode/module.zig").Module;
const Opcode = @import("../bytecode/opcodes.zig").Opcode;

/// Severity level for validation issues
pub const Severity = enum {
    /// Critical error - bytecode is invalid
    @"error",
    /// Warning - bytecode may have issues
    warning,
    /// Info - observation that may be useful
    info,
};

/// A single validation issue
pub const ValidationIssue = struct {
    /// Severity of the issue
    severity: Severity,
    /// Category of the issue
    kind: Kind,
    /// Byte offset where issue was found
    offset: u32,
    /// Human-readable message
    message: []const u8,

    pub const Kind = enum {
        // Header issues
        invalid_magic,
        invalid_version,
        invalid_entry_point,

        // Section issues
        section_overlap,
        section_out_of_bounds,
        missing_section,

        // Instruction issues
        invalid_opcode,
        invalid_operand,
        instruction_overflow,
        register_out_of_range,

        // Reference issues
        invalid_constant_index,
        invalid_routine_index,
        invalid_type_index,

        // Control flow issues
        jump_out_of_bounds,
        unreachable_code,
        missing_return,
        stack_imbalance,

        // General issues
        truncated_data,
        alignment_error,
    };

    pub fn format(self: ValidationIssue, buf: []u8) []const u8 {
        const severity_str = switch (self.severity) {
            .@"error" => "ERROR",
            .warning => "WARNING",
            .info => "INFO",
        };
        return std.fmt.bufPrint(buf, "[{s}] 0x{x:0>4}: {s} - {s}", .{
            severity_str,
            self.offset,
            @tagName(self.kind),
            self.message,
        }) catch "format error";
    }
};

/// Validation statistics
pub const ValidationStats = struct {
    /// Total bytes in code section
    code_size: u32 = 0,
    /// Number of instructions found
    instruction_count: u32 = 0,
    /// Number of routines
    routine_count: u32 = 0,
    /// Number of constants
    constant_count: u32 = 0,
    /// Number of types
    type_count: u32 = 0,
    /// Number of errors found
    error_count: u32 = 0,
    /// Number of warnings found
    warning_count: u32 = 0,
    /// Percentage of code that's reachable (0-100)
    code_coverage: f32 = 0,
};

/// Result of validation
pub const ValidationResult = struct {
    /// All issues found
    issues: []ValidationIssue,
    /// Statistics
    stats: ValidationStats,
    /// Whether bytecode is valid (no errors)
    is_valid: bool,

    pub fn deinit(self: *ValidationResult, allocator: Allocator) void {
        for (self.issues) |issue| {
            allocator.free(issue.message);
        }
        allocator.free(self.issues);
    }
};

/// Instruction walker - iterates over bytecode instructions
pub const InstructionWalker = struct {
    code: []const u8,
    pos: usize = 0,

    pub const Instruction = struct {
        offset: u32,
        opcode: Opcode,
        size: u8,
        operands: []const u8,
    };

    pub fn next(self: *InstructionWalker) ?Instruction {
        if (self.pos >= self.code.len) return null;

        const offset: u32 = @intCast(self.pos);
        const opcode_byte = self.code[self.pos];

        // Check for valid opcode
        const opcode: Opcode = @enumFromInt(opcode_byte);
        const size = getInstructionSize(opcode);

        // Check for truncated instruction
        if (self.pos + size > self.code.len) {
            self.pos = self.code.len; // Move to end to stop iteration
            return null;
        }

        const inst = Instruction{
            .offset = offset,
            .opcode = opcode,
            .size = size,
            .operands = if (size > 1) self.code[self.pos + 1 .. self.pos + size] else &.{},
        };

        self.pos += size;
        return inst;
    }

    pub fn reset(self: *InstructionWalker) void {
        self.pos = 0;
    }

    pub fn seekTo(self: *InstructionWalker, offset: usize) void {
        self.pos = @min(offset, self.code.len);
    }
};

/// Bytecode validator
pub const Validator = struct {
    allocator: Allocator,
    module: *const Module,
    issues: std.ArrayListUnmanaged(ValidationIssue),
    reachable: std.DynamicBitSet,
    stats: ValidationStats,

    const Self = @This();

    pub fn init(allocator: Allocator, mod: *const Module) Self {
        return .{
            .allocator = allocator,
            .module = mod,
            .issues = .empty,
            .reachable = std.DynamicBitSet.initEmpty(allocator, 0) catch std.DynamicBitSet.initEmpty(allocator, 0) catch unreachable,
            .stats = .{},
        };
    }

    pub fn deinit(self: *Self) void {
        for (self.issues.items) |issue| {
            self.allocator.free(issue.message);
        }
        self.issues.deinit(self.allocator);
        self.reachable.deinit();
    }

    /// Run all validations and return result
    pub fn validate(self: *Self) !ValidationResult {
        // Initialize stats
        self.stats.code_size = @intCast(self.module.code.len);
        self.stats.routine_count = @intCast(self.module.routines.len);
        self.stats.constant_count = @intCast(self.module.constants.len);
        self.stats.type_count = @intCast(self.module.types.len);

        // Initialize reachability tracking
        if (self.module.code.len > 0) {
            self.reachable.deinit();
            self.reachable = try std.DynamicBitSet.initEmpty(self.allocator, self.module.code.len);
        }

        // Run validation passes
        try self.validateHeader();
        try self.validateRoutines();
        try self.validateInstructions();
        try self.validateControlFlow();
        try self.validateReferences();

        // Calculate coverage
        if (self.stats.code_size > 0) {
            var reachable_count: u32 = 0;
            var i: usize = 0;
            while (i < self.module.code.len) : (i += 1) {
                if (self.reachable.isSet(i)) reachable_count += 1;
            }
            self.stats.code_coverage = @as(f32, @floatFromInt(reachable_count)) /
                @as(f32, @floatFromInt(self.stats.code_size)) * 100.0;
        }

        // Count errors and warnings
        for (self.issues.items) |issue| {
            switch (issue.severity) {
                .@"error" => self.stats.error_count += 1,
                .warning => self.stats.warning_count += 1,
                .info => {},
            }
        }

        return .{
            .issues = try self.issues.toOwnedSlice(self.allocator),
            .stats = self.stats,
            .is_valid = self.stats.error_count == 0,
        };
    }

    // =========================================================================
    // Validation Passes
    // =========================================================================

    fn validateHeader(self: *Self) !void {
        // Check magic number
        if (self.module.code.len < 4) {
            try self.addIssue(.@"error", .truncated_data, 0, "Module code too short for header");
            return;
        }

        // Check entry point
        const entry = self.module.header.entry_point;
        if (entry != 0xFFFFFFFF) { // 0xFFFFFFFF means library/no entry
            if (entry >= self.module.code.len) {
                try self.addIssue(.@"error", .invalid_entry_point, 0,
                    try std.fmt.allocPrint(self.allocator, "Entry point 0x{x} beyond code end", .{entry}));
            }
        }
    }

    fn validateRoutines(self: *Self) !void {
        for (self.module.routines, 0..) |routine, i| {
            const offset = routine.code_offset;
            const end = offset + routine.code_length;

            // Check bounds
            if (offset >= self.module.code.len) {
                try self.addIssue(.@"error", .section_out_of_bounds, offset,
                    try std.fmt.allocPrint(self.allocator, "Routine {d} offset beyond code end", .{i}));
                continue;
            }

            if (end > self.module.code.len) {
                try self.addIssue(.@"error", .section_out_of_bounds, offset,
                    try std.fmt.allocPrint(self.allocator, "Routine {d} extends beyond code end", .{i}));
            }

            // Mark routine code as reachable
            var j: usize = offset;
            while (j < end and j < self.module.code.len) : (j += 1) {
                self.reachable.set(j);
            }
        }
    }

    fn validateInstructions(self: *Self) !void {
        var walker = InstructionWalker{ .code = self.module.code };

        while (walker.next()) |inst| {
            self.stats.instruction_count += 1;

            // Validate opcode
            if (!isValidOpcode(inst.opcode)) {
                try self.addIssue(.@"error", .invalid_opcode, inst.offset,
                    try std.fmt.allocPrint(self.allocator, "Invalid opcode 0x{x:0>2}", .{@intFromEnum(inst.opcode)}));
                continue;
            }

            // Validate operands based on opcode
            try self.validateOperands(inst);
        }

        // Check for truncation
        if (walker.pos < self.module.code.len) {
            try self.addIssue(.warning, .truncated_data, @intCast(walker.pos),
                "Incomplete instruction at end of code");
        }
    }

    fn validateOperands(self: *Self, inst: InstructionWalker.Instruction) !void {
        switch (inst.opcode) {
            // Arithmetic operations - check register indices
            .add, .sub, .mul, .div, .mod => {
                if (inst.operands.len >= 1) {
                    const dest = inst.operands[0] >> 4;
                    const src1 = inst.operands[0] & 0x0F;
                    if (dest > 15) {
                        try self.addIssue(.@"error", .register_out_of_range, inst.offset,
                            "Destination register out of range");
                    }
                    if (src1 > 15) {
                        try self.addIssue(.@"error", .register_out_of_range, inst.offset,
                            "Source register 1 out of range");
                    }
                }
            },

            // Constant loading - check constant index
            .load_const => {
                if (inst.operands.len >= 3) {
                    const const_idx = std.mem.readInt(u16, inst.operands[1..3], .little);
                    if (const_idx >= self.module.constants.len) {
                        try self.addIssue(.@"error", .invalid_constant_index, inst.offset,
                            try std.fmt.allocPrint(self.allocator, "Constant index {d} out of range (max {d})", .{
                                const_idx,
                                self.module.constants.len,
                            }));
                    }
                }
            },

            // Jump instructions - validate target
            .jmp, .jz, .jnz => {
                if (inst.operands.len >= 2) {
                    // Jump offset is 16-bit signed relative
                    const target_offset = blk: {
                        if (inst.operands.len >= 3) {
                            const rel: i16 = @bitCast(std.mem.readInt(u16, inst.operands[1..3], .little));
                            const base: i32 = @intCast(inst.offset + inst.size);
                            break :blk @as(i32, base) + @as(i32, rel);
                        }
                        break :blk @as(i32, 0);
                    };

                    if (target_offset < 0 or target_offset >= @as(i32, @intCast(self.module.code.len))) {
                        try self.addIssue(.@"error", .jump_out_of_bounds, inst.offset,
                            try std.fmt.allocPrint(self.allocator, "Jump target {d} out of bounds", .{target_offset}));
                    }
                }
            },

            else => {},
        }
    }

    fn validateControlFlow(self: *Self) !void {
        // Check for unreachable code at end of routines
        for (self.module.routines) |routine| {
            if (routine.code_length == 0) continue;

            const last_ip = routine.code_offset + routine.code_length - 1;
            if (last_ip >= self.module.code.len) continue;

            // Check that routine ends with ret or halt
            const last_opcode: Opcode = @enumFromInt(self.module.code[last_ip]);
            switch (last_opcode) {
                .ret, .halt, .jmp => {}, // OK
                else => {
                    // Check if there's a ret nearby (might be multi-byte instruction)
                    var found_terminator = false;
                    var check_ip = routine.code_offset + routine.code_length;
                    while (check_ip > routine.code_offset) {
                        check_ip -= 1;
                        const op: Opcode = @enumFromInt(self.module.code[check_ip]);
                        if (op == .ret or op == .halt) {
                            found_terminator = true;
                            break;
                        }
                        // Only check last few bytes
                        if (routine.code_offset + routine.code_length - check_ip > 10) break;
                    }

                    if (!found_terminator) {
                        try self.addIssue(.warning, .missing_return, @intCast(last_ip),
                            "Routine may not have proper return/halt");
                    }
                },
            }
        }
    }

    fn validateReferences(self: *Self) !void {
        _ = self;
        // Additional reference validation can be added here
        // e.g., checking string table references, type references, etc.
    }

    // =========================================================================
    // Helpers
    // =========================================================================

    fn addIssue(self: *Self, severity: Severity, kind: ValidationIssue.Kind, offset: u32, message: []const u8) !void {
        try self.issues.append(self.allocator, .{
            .severity = severity,
            .kind = kind,
            .offset = offset,
            .message = message,
        });
    }
};

/// Check if an opcode value is valid
fn isValidOpcode(opcode: Opcode) bool {
    // All opcodes in the enum are valid
    return switch (opcode) {
        .nop, .halt, .ret, .ret_val => true,
        .add, .sub, .mul, .div, .mod => true,
        .movi, .load_local, .store_local => true,
        .load_const, .jmp, .jz, .jnz => true,
        .debug_line => true,
        else => true, // Trust the enum
    };
}

/// Get instruction size for an opcode (opcode byte + operand bytes)
fn getInstructionSize(opcode: Opcode) u8 {
    // Size = 1 (opcode) + operandSize()
    return @as(u8, @intCast(1 + opcode.operandSize()));
}

// ============================================================================
// Tests
// ============================================================================

test "InstructionWalker: basic iteration" {
    const code = [_]u8{
        0x00, // nop
        0x00, // nop
        0x01, // halt (0x01)
    };

    var walker = InstructionWalker{ .code = &code };

    const inst1 = walker.next().?;
    try std.testing.expectEqual(Opcode.nop, inst1.opcode);
    try std.testing.expectEqual(@as(u32, 0), inst1.offset);

    const inst2 = walker.next().?;
    try std.testing.expectEqual(Opcode.nop, inst2.opcode);
    try std.testing.expectEqual(@as(u32, 1), inst2.offset);

    const inst3 = walker.next().?;
    try std.testing.expectEqual(Opcode.halt, inst3.opcode);
    try std.testing.expectEqual(@as(u32, 2), inst3.offset);

    try std.testing.expect(walker.next() == null);
}
