//! Cot IR to Bytecode Emitter
//!
//! Transforms IR into bytecode that can be executed by the Cot VM.
//! This replaces the direct AST-to-bytecode compilation path.

const std = @import("std");
const ir = @import("ir.zig");
const cot_runtime = @import("cot_runtime");
const module = cot_runtime.bytecode.module;
const opcodes = cot_runtime.bytecode.opcodes;
const debug = cot_runtime.debug;

const Allocator = std.mem.Allocator;
const Opcode = opcodes.Opcode;

/// Builtin function definition for performance-critical opcodes
const BuiltinDef = struct {
    opcode: Opcode,
    min_args: u8,
    max_args: u8,
};

/// Performance-critical functions that have dedicated opcodes
/// Everything else routes through xcall to native functions
const opcode_builtins = std.StaticStringMap(BuiltinDef).initComptime(.{
    // Only keep opcodes for extremely high-volume operations
    .{ "trim", BuiltinDef{ .opcode = .str_trim, .min_args = 1, .max_args = 1 } },
    .{ "atrim", BuiltinDef{ .opcode = .str_trim, .min_args = 1, .max_args = 1 } },
    .{ "len", BuiltinDef{ .opcode = .str_len, .min_args = 1, .max_args = 1 } },
    .{ "size", BuiltinDef{ .opcode = .fn_size, .min_args = 1, .max_args = 1 } },
});

/// Core Cot I/O functions with dedicated opcodes (no return value)
const io_functions = std.StaticStringMap(Opcode).initComptime(.{
    .{ "println", .console_writeln },
    .{ "print", .console_write },
});

/// Native functions - these are implemented in Zig and called via xcall
/// All other "builtin" functions now route through this mechanism
const native_functions = std.StaticStringMap(void).initComptime(.{
    // Type conversion
    .{ "string", {} },
    .{ "integer", {} },
    .{ "decimal", {} },
    .{ "char", {} },
    .{ "alpha", {} },
    .{ "boolean", {} },
    // String operations
    .{ "instr", {} },
    .{ "str_delete_last", {} },
    .{ "upper", {} },
    .{ "lower", {} },
    .{ "ltrim", {} },
    // Math functions
    .{ "abs", {} },
    .{ "sqrt", {} },
    .{ "sin", {} },
    .{ "cos", {} },
    .{ "tan", {} },
    .{ "log", {} },
    .{ "log10", {} },
    .{ "exp", {} },
    .{ "round", {} },
    .{ "trunc", {} },
    // Date/time
    .{ "date", {} },
    .{ "time", {} },
    // System
    .{ "error", {} },
    .{ "mem", {} },
});

/// Bytecode emitter errors
pub const EmitError = error{
    OutOfMemory,
    TooManyConstants,
    TooManyLocals,
    TooManyGlobals,
    JumpTooFar,
    InvalidInstruction,
    UndefinedBlock,
    StackOverflow,
};

/// Pending jump that needs to be patched
const PendingJump = struct {
    /// Offset in code where the jump offset should be written
    patch_offset: u32,
    /// Target block
    target_block: *const ir.Block,
    /// Whether this is a wide jump (i32) or normal (i16)
    is_wide: bool,
};

/// Variable location information
const VarLocation = struct {
    slot: u16,
    is_global: bool,
};

/// Register allocator for register-based bytecode emission
/// Uses a linear scan approach with 16 virtual registers (r0-r15)
pub const RegisterAllocator = struct {
    /// Bitmask of available registers (1 = free, 0 = in use)
    /// r0-r7: caller-saved, r8-r13: callee-saved, r14: frame pointer (reserved), r15: return (reserved)
    free_regs: u16,

    /// Maps IR value ID to register number (0-15)
    value_to_reg: std.AutoHashMap(u32, u4),

    /// Maps register to IR value ID (for spilling)
    reg_to_value: [16]?u32,

    /// Spill slots for values that couldn't fit in registers
    spill_slots: std.ArrayList(u32),

    /// Allocator for internal data structures
    alloc: Allocator,

    const Self = @This();

    /// Initial free registers: r0-r13 (0x3FFF), r14-r15 reserved
    const INITIAL_FREE: u16 = 0x3FFF;

    pub fn init(allocator: Allocator) Self {
        return .{
            .free_regs = INITIAL_FREE,
            .value_to_reg = std.AutoHashMap(u32, u4).init(allocator),
            .reg_to_value = [_]?u32{null} ** 16,
            .spill_slots = .{},
            .alloc = allocator,
        };
    }

    pub fn deinit(self: *Self) void {
        self.value_to_reg.deinit();
        self.spill_slots.deinit(self.alloc);
    }

    /// Reset allocator state for a new function
    pub fn reset(self: *Self) void {
        self.free_regs = INITIAL_FREE;
        self.value_to_reg.clearRetainingCapacity();
        self.reg_to_value = [_]?u32{null} ** 16;
        self.spill_slots.clearRetainingCapacity();
    }

    /// Allocate a register for a value. Returns null if all registers are in use.
    pub fn allocate(self: *Self, value_id: u32) ?u4 {
        // Find first free register (prefer caller-saved r0-r7 first)
        var reg: u4 = 0;
        while (reg < 14) : (reg += 1) {
            const mask = @as(u16, 1) << reg;
            if (self.free_regs & mask != 0) {
                // Found a free register
                self.free_regs &= ~mask;
                self.value_to_reg.put(value_id, reg) catch return null;
                self.reg_to_value[reg] = value_id;
                return reg;
            }
        }
        return null; // All registers in use
    }

    /// Get the register holding a value, if any
    pub fn getRegister(self: *Self, value_id: u32) ?u4 {
        return self.value_to_reg.get(value_id);
    }

    /// Free a register (value is no longer needed)
    pub fn free(self: *Self, reg: u4) void {
        if (reg >= 14) return; // Don't free reserved registers
        const mask = @as(u16, 1) << reg;
        self.free_regs |= mask;
        if (self.reg_to_value[reg]) |value_id| {
            _ = self.value_to_reg.remove(value_id);
        }
        self.reg_to_value[reg] = null;
    }

    /// Free the register holding a specific value
    pub fn freeValue(self: *Self, value_id: u32) void {
        if (self.value_to_reg.get(value_id)) |reg| {
            self.free(reg);
        }
    }

    /// Mark a value as being in a specific register (for parameters)
    pub fn assignRegister(self: *Self, value_id: u32, reg: u4) !void {
        if (reg >= 14) return;
        const mask = @as(u16, 1) << reg;
        self.free_regs &= ~mask;
        try self.value_to_reg.put(value_id, reg);
        self.reg_to_value[reg] = value_id;
    }

    /// Check if a specific register is free
    pub fn isRegisterFree(self: *Self, reg: u4) bool {
        if (reg >= 14) return false;
        const mask = @as(u16, 1) << reg;
        return self.free_regs & mask != 0;
    }

    /// Count of free registers
    pub fn freeCount(self: *Self) u32 {
        return @popCount(self.free_regs & INITIAL_FREE);
    }
};

/// Bytecode emitter
pub const BytecodeEmitter = struct {
    allocator: Allocator,

    // Output buffers
    code: std.ArrayList(u8),
    constants: std.ArrayList(module.Constant),
    types: std.ArrayList(module.TypeDef),
    routines: std.ArrayList(module.RoutineDef),

    // Symbol tables
    globals: std.StringHashMap(VarLocation),
    locals: std.StringHashMap(VarLocation),
    global_count: u16,
    local_count: u16,

    // Constant deduplication
    string_pool: std.StringHashMap(u16),
    int_pool: std.AutoHashMap(i64, u16),

    // Function name to routine index mapping (for forward references)
    function_indices: std.StringHashMap(u16),

    // (Stack tracking removed - now register-based only)

    // Block management
    block_offsets: std.AutoHashMap(*const ir.Block, u32), // block -> code_offset
    pending_jumps: std.ArrayList(PendingJump),

    // IR value to stack slot mapping
    value_slots: std.AutoHashMap(u32, u16), // value_id -> stack_slot

    // IR value to constant pool index (for constants that can be loaded on demand)
    value_consts: std.AutoHashMap(u32, u16), // value_id -> constant_pool_index

    // Current routine being emitted
    current_routine_start: u32,

    // Current function being emitted (for label lookup)
    current_func: ?*const ir.Function,

    // Register allocator for register-based bytecode
    reg_alloc: RegisterAllocator,

    // Track the last computed intermediate result (for SSA value chains)
    // This avoids permanently allocating registers for temp results
    last_result_value: ?u32 = null,
    last_result_reg: u4 = 0,

    const Self = @This();

    /// Initialize the bytecode emitter (register-based)
    pub fn init(allocator: Allocator) Self {
        return .{
            .allocator = allocator,
            .code = .{},
            .constants = .{},
            .types = .{},
            .routines = .{},
            .globals = std.StringHashMap(VarLocation).init(allocator),
            .locals = std.StringHashMap(VarLocation).init(allocator),
            .global_count = 0,
            .local_count = 0,
            .string_pool = std.StringHashMap(u16).init(allocator),
            .int_pool = std.AutoHashMap(i64, u16).init(allocator),
            .function_indices = std.StringHashMap(u16).init(allocator),
            .block_offsets = std.AutoHashMap(*const ir.Block, u32).init(allocator),
            .pending_jumps = .{},
            .value_slots = std.AutoHashMap(u32, u16).init(allocator),
            .value_consts = std.AutoHashMap(u32, u16).init(allocator),
            .current_routine_start = 0,
            .current_func = null,
            .reg_alloc = RegisterAllocator.init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        self.code.deinit(self.allocator);
        self.constants.deinit(self.allocator);
        self.types.deinit(self.allocator);
        self.routines.deinit(self.allocator);
        self.globals.deinit();
        self.locals.deinit();
        self.string_pool.deinit();
        self.int_pool.deinit();
        self.function_indices.deinit();
        self.block_offsets.deinit();
        self.pending_jumps.deinit(self.allocator);
        self.value_slots.deinit();
        self.value_consts.deinit();
        self.reg_alloc.deinit();
    }

    /// Emit bytecode module from IR module
    pub fn emit(self: *Self, ir_module: *const ir.Module) EmitError!module.Module {
        // Pre-pass: assign routine indices to all functions (for forward references)
        for (ir_module.functions.items, 0..) |func, i| {
            try self.function_indices.put(func.name, @intCast(i));
        }

        // First pass: emit record types and register their fields as globals
        for (ir_module.structs.items) |record| {
            try self.emitRecordType(record);

            // Register the struct base name as a global (points to first field slot)
            // This is needed for field_ptr to find the struct pointer
            const base_slot = self.global_count;
            try self.globals.put(record.name, .{
                .slot = base_slot,
                .is_global = true,
            });

            // Register struct fields as globals
            for (record.fields) |field| {
                const qualified_name = std.fmt.allocPrint(
                    self.allocator,
                    "{s}.{s}",
                    .{ record.name, field.name },
                ) catch return EmitError.OutOfMemory;

                try self.globals.put(qualified_name, .{
                    .slot = self.global_count,
                    .is_global = true,
                });
                self.global_count += 1;
            }
        }

        // Second pass: emit all functions
        for (ir_module.functions.items) |func| {
            try self.emitFunction(func);
        }

        // Emit halt at end
        try self.emitOpcode(.halt);

        // Build the module
        const routines_slice = try self.routines.toOwnedSlice(self.allocator);
        debug.print(.emit, "Module has {d} routines:", .{routines_slice.len});
        for (routines_slice) |routine| {
            const routine_name = switch (self.constants.items[routine.name_index]) {
                .identifier => |name| name,
                else => "(unknown)",
            };
            debug.print(.emit, "  - {s} (offset={d}, len={d})", .{ routine_name, routine.code_offset, routine.code_length });
        }

        // Build exports - export all routines except main (DBL semantics: all subroutines are globally available)
        var exports_list: std.ArrayList(module.ExportEntry) = .{};
        for (routines_slice, 0..) |routine, i| {
            // Get routine name
            const routine_name = switch (self.constants.items[routine.name_index]) {
                .identifier => |name| name,
                else => continue,
            };

            // Export all routines except "main" (which is the entry point)
            if (!std.mem.eql(u8, routine_name, "main")) {
                try exports_list.append(self.allocator, .{
                    .name_index = routine.name_index,
                    .kind = .routine,
                    .flags = 0,
                    .index = @intCast(i),
                });
            }
        }

        var result = module.Module{
            .allocator = self.allocator,
            .header = module.ModuleHeader.init(),
            .constants = try self.constants.toOwnedSlice(self.allocator),
            .types = try self.types.toOwnedSlice(self.allocator),
            .routines = routines_slice,
            .code = try self.code.toOwnedSlice(self.allocator),
            .exports = try exports_list.toOwnedSlice(self.allocator),
            .imports = &[_]module.ImportEntry{},
            .source_file = null,
            .line_table = null,
            .local_vars = null,
            .globals_count = 0, // Will be calculated at load time
        };

        // Find "main" function and set as entry point
        result.header.entry_point = 0; // Default to first routine
        for (routines_slice) |routine| {
            const name = result.getConstant(routine.name_index) orelse continue;
            const routine_name = switch (name) {
                .identifier => |n| n,
                .string => |s| s,
                else => continue,
            };
            if (std.mem.eql(u8, routine_name, "main")) {
                result.header.entry_point = routine.code_offset;
                break;
            }
        }

        return result;
    }

    /// Emit a struct type definition
    fn emitRecordType(self: *Self, record: *const ir.StructType) EmitError!void {
        var fields: std.ArrayList(module.FieldDef) = .{};
        errdefer fields.deinit(self.allocator);

        for (record.fields, 0..) |field, i| {
            const name_idx = try self.addIdentifier(field.name);
            try fields.append(self.allocator, .{
                .name_index = name_idx,
                .data_type = irTypeToDataType(field.ty),
                .flags = 0,
                .offset = @intCast(field.offset),
                .size = @intCast(field.ty.sizeInBytes()),
                .precision = if (field.ty == .decimal) field.ty.decimal.scale else 0,
                .array_dims = &[_]u16{},
            });

            // Note: DO NOT register field names as globals here!
            // Globals are registered via alloca instructions with qualified names (e.g., "app.ch_cust")
            // Registering unqualified names here causes conflicts with subroutine parameters.
            _ = i;
        }

        const name_idx = try self.addIdentifier(record.name);
        try self.types.append(self.allocator, .{
            .type_id = @intCast(self.types.items.len),
            .kind = .record,
            .flags = 0,
            .name_index = name_idx,
            .total_size = record.size,
            .fields = try fields.toOwnedSlice(self.allocator),
        });
    }

    /// Emit a function
    fn emitFunction(self: *Self, func: *const ir.Function) EmitError!void {
        // Track current function for label lookup
        self.current_func = func;

        // Clear per-function state
        self.locals.clearRetainingCapacity();
        self.local_count = 0;
        self.block_offsets.clearRetainingCapacity();
        self.pending_jumps.clearRetainingCapacity();
        self.value_slots.clearRetainingCapacity();
        self.value_consts.clearRetainingCapacity();
        self.reg_alloc.reset(); // Reset register allocation for new function
        self.last_result_value = null; // Reset last result tracking

        self.current_routine_start = @intCast(self.code.items.len);

        // Allocate locals for parameters
        // For struct-typed parameters, flatten into individual field slots with qualified names
        for (func.signature.params) |param| {
            if (param.ty == .@"struct") {
                // Structure parameter - allocate slots for each field with qualified names
                const struct_type = param.ty.@"struct";
                for (struct_type.fields) |field| {
                    const qualified_name = std.fmt.allocPrint(
                        self.allocator,
                        "{s}.{s}",
                        .{ param.name, field.name },
                    ) catch return EmitError.OutOfMemory;
                    try self.locals.put(qualified_name, .{
                        .slot = self.local_count,
                        .is_global = false,
                    });
                    self.local_count += 1;
                }
            } else {
                // Normal parameter - single slot
                try self.locals.put(param.name, .{
                    .slot = self.local_count,
                    .is_global = false,
                });
                self.local_count += 1;
            }
        }

        // First pass: record block offsets
        // We need to know where each block starts before emitting jumps
        var estimated_offset: u32 = @intCast(self.code.items.len);
        for (func.blocks.items) |block| {
            try self.block_offsets.put(block, estimated_offset);
            // Rough estimate of block size (will be adjusted)
            estimated_offset += @intCast(block.instructions.items.len * 4);
        }

        // Second pass: emit blocks
        for (func.blocks.items) |block| {
            try self.emitBlock(block);
        }

        // Patch pending jumps
        try self.patchJumps();

        // Record routine definition
        const name_idx = try self.addIdentifier(func.name);
        const code_len = @as(u32, @intCast(self.code.items.len)) - self.current_routine_start;

        // Build locals debug info from the locals symbol table
        var locals_list: std.ArrayListUnmanaged(module.LocalDef) = .empty;
        var slot_iter = self.locals.iterator();
        while (slot_iter.next()) |entry| {
            const var_name_idx = try self.addIdentifier(entry.key_ptr.*);
            try locals_list.append(self.allocator, .{
                .name_index = var_name_idx,
                .slot = entry.value_ptr.slot,
                .data_type = .string, // Default type; could be enhanced
            });
        }

        try self.routines.append(self.allocator, .{
            .name_index = name_idx,
            .flags = if (func.linkage == .external) module.RoutineFlags{ .is_public = true } else module.RoutineFlags{},
            .code_offset = self.current_routine_start,
            .code_length = code_len,
            .param_count = @intCast(func.signature.params.len),
            .local_count = self.local_count,
            .max_stack = 0, // Register-based: stack not used
            .params = &[_]module.ParamDef{}, // TODO: populate if needed
            .locals = try locals_list.toOwnedSlice(self.allocator),
        });
    }

    /// Emit a basic block
    fn emitBlock(self: *Self, block: *const ir.Block) EmitError!void {
        // Update actual block offset
        try self.block_offsets.put(block, @intCast(self.code.items.len));

        for (block.instructions.items) |inst| {
            try self.emitInstruction(&inst);
        }
    }

    /// Emit a single IR instruction as bytecode
    fn emitInstruction(self: *Self, inst: *const ir.Instruction) EmitError!void {
        switch (inst.*) {
            .alloca => |a| {
                // Check if this is a record field (already registered as global)
                if (self.globals.get(a.name)) |global_info| {
                    // Use the existing global slot
                    try self.value_slots.put(a.result.id, global_info.slot | 0x8000); // Set high bit to mark as global
                } else if (self.locals.get(a.name)) |local_info| {
                    // This is a parameter - use the existing parameter slot
                    try self.value_slots.put(a.result.id, local_info.slot);
                } else {
                    // Check if this is a struct type - if so, allocate slots for all fields
                    if (a.ty == .@"struct") {
                        const struct_type = a.ty.@"struct";
                        const base_slot = self.local_count;
                        // Register the struct base slot
                        try self.locals.put(a.name, .{
                            .slot = base_slot,
                            .is_global = false,
                        });
                        try self.value_slots.put(a.result.id, base_slot);
                        // Allocate a slot for each field and register with qualified name
                        for (struct_type.fields) |field| {
                            const qualified_name = std.fmt.allocPrint(
                                self.allocator,
                                "{s}.{s}",
                                .{ a.name, field.name },
                            ) catch return EmitError.OutOfMemory;
                            try self.locals.put(qualified_name, .{
                                .slot = self.local_count,
                                .is_global = false,
                            });
                            self.local_count += 1;
                        }
                    } else {
                        // Allocate a new local variable slot (primitive type)
                        try self.locals.put(a.name, .{
                            .slot = self.local_count,
                            .is_global = false,
                        });
                        try self.value_slots.put(a.result.id, self.local_count);
                        self.local_count += 1;
                    }
                }
            },

            .load => |l| {
                // Register-based: track slot mapping, load on demand
                // Don't allocate a register now - will load when value is used
                if (self.value_slots.get(l.ptr.id)) |slot_info| {
                    // Forward the slot info to the result value
                    try self.value_slots.put(l.result.id, slot_info);
                } else if (self.value_consts.get(l.ptr.id)) |const_idx| {
                    // Pointer is a constant - forward constant mapping
                    try self.value_consts.put(l.result.id, const_idx);
                } else if (self.last_result_value) |last_id| {
                    if (last_id == l.ptr.id) {
                        // Pointer is the last computed result - set result as last result too
                        self.setLastResult(l.result.id, self.last_result_reg);
                    } else {
                        debug.print(.emit, "WARNING: .load ptr.id={d} not found, last_result={d}", .{ l.ptr.id, last_id });
                    }
                } else {
                    debug.print(.emit, "WARNING: .load ptr.id={d} not found", .{l.ptr.id});
                }
            },

            .store => |s| {
                // Register-based: store from register to memory
                const src_reg = try self.getValueInReg(s.value, 0);
                if (self.value_slots.get(s.ptr.id)) |slot_info| {
                    if (slot_info & 0x8000 != 0) {
                        // Global variable
                        const slot = slot_info & 0x7FFF;
                        try self.emitRegStoreGlobal(src_reg, slot);
                    } else {
                        // Local variable
                        if (slot_info < 256) {
                            try self.emitRegStoreLocal(src_reg, @intCast(slot_info));
                        } else {
                            return EmitError.TooManyLocals;
                        }
                    }
                }
            },

            .field_ptr => |fp| {
                // field_ptr computes a pointer to a struct field
                // We need to track the slot in value_slots for both register and stack modes
                // because store/load use value_slots to find the target slot
                if (self.value_slots.get(fp.struct_ptr.id)) |struct_slot| {
                    // Compute the effective slot: struct_slot + field_index
                    // This is a simplified approach - assumes fields are contiguous locals
                    const field_slot = (struct_slot & 0x7FFF) + @as(u16, @intCast(fp.field_index));
                    const is_global = (struct_slot & 0x8000) != 0;
                    try self.value_slots.put(fp.result.id, field_slot | (if (is_global) @as(u16, 0x8000) else 0));
                } else {
                    std.debug.print("[EMIT] WARNING: field_ptr struct_ptr.id={d} not in value_slots\n", .{fp.struct_ptr.id});
                }
            },

            .iconst => |c| {
                // Register-based: store in constant pool, load on demand
                // Don't allocate a register now - will load when needed
                const const_idx = try self.addConstant(.{ .integer = c.value });
                try self.value_consts.put(c.result.id, const_idx);
            },

            .f32const => |c| {
                // Store float as fixed-point constant
                const int_val: i64 = @intFromFloat(@as(f64, c.value) * 100);
                const const_idx = try self.addConstant(.{ .integer = int_val });
                try self.value_consts.put(c.result.id, const_idx);
            },

            .f64const => |c| {
                // Store float as fixed-point constant
                const int_val: i64 = @intFromFloat(c.value * 100);
                const const_idx = try self.addConstant(.{ .integer = int_val });
                try self.value_consts.put(c.result.id, const_idx);
            },

            .const_string => |c| {
                const const_idx = try self.addString(c.value);
                // Register-based: store in constant pool, load on demand
                try self.value_consts.put(c.result.id, const_idx);
            },

            .const_null => {
                // Note: const_null doesn't have a result value, so we can't track it
                // This is for legacy compatibility - should rarely be used
            },

            .iadd => |a| {
                // Register-based emission - use temp registers for operands
                const lhs_reg = try self.getValueInReg(a.lhs, 0); // r0 as temp
                const rhs_reg = try self.getValueInReg(a.rhs, 1); // r1 as temp
                const dest_reg: u4 = 2; // Result always goes to r2
                try self.emitRegArith(.add, dest_reg, lhs_reg, rhs_reg);
                // Track the result in r2 without permanent allocation
                self.setLastResult(a.result.id, dest_reg);
            },

            .isub => |s| {
                const lhs_reg = try self.getValueInReg(s.lhs, 0);
                const rhs_reg = try self.getValueInReg(s.rhs, 1);
                const dest_reg: u4 = 2;
                try self.emitRegArith(.sub, dest_reg, lhs_reg, rhs_reg);
                self.setLastResult(s.result.id, dest_reg);
            },

            .imul => |m| {
                const lhs_reg = try self.getValueInReg(m.lhs, 0);
                const rhs_reg = try self.getValueInReg(m.rhs, 1);
                const dest_reg: u4 = 2;
                try self.emitRegArith(.mul, dest_reg, lhs_reg, rhs_reg);
                self.setLastResult(m.result.id, dest_reg);
            },

            .sdiv, .udiv => |d| {
                const lhs_reg = try self.getValueInReg(d.lhs, 0);
                const rhs_reg = try self.getValueInReg(d.rhs, 1);
                const dest_reg: u4 = 2;
                try self.emitRegArith(.div, dest_reg, lhs_reg, rhs_reg);
                self.setLastResult(d.result.id, dest_reg);
            },

            .srem, .urem => |m| {
                const lhs_reg = try self.getValueInReg(m.lhs, 0);
                const rhs_reg = try self.getValueInReg(m.rhs, 1);
                const dest_reg: u4 = 2;
                try self.emitRegArith(.mod, dest_reg, lhs_reg, rhs_reg);
                self.setLastResult(m.result.id, dest_reg);
            },

            .ineg => |n| {
                const src_reg = try self.getValueInReg(n.operand, 0);
                const dest_reg: u4 = 1;
                try self.emitRegUnary(.neg, dest_reg, src_reg);
                self.setLastResult(n.result.id, dest_reg);
            },

            .icmp => |c| {
                const lhs_reg = try self.getValueInReg(c.lhs, 0);
                const rhs_reg = try self.getValueInReg(c.rhs, 1);
                const dest_reg: u4 = 2;
                // Map IntCC condition to bytecode opcode
                const opcode: Opcode = switch (c.cond) {
                    .eq => if (c.lhs.ty == .string) .cmp_str_eq else .cmp_eq,
                    .ne => .cmp_ne,
                    .slt, .ult => .cmp_lt,
                    .sle, .ule => .cmp_le,
                    .sgt, .ugt => .cmp_gt,
                    .sge, .uge => .cmp_ge,
                };
                try self.emitRegArith(opcode, dest_reg, lhs_reg, rhs_reg);
                self.setLastResult(c.result.id, dest_reg);
            },

            .log_and => |l| {
                const lhs_reg = try self.getValueInReg(l.lhs, 0);
                const rhs_reg = try self.getValueInReg(l.rhs, 1);
                const dest_reg: u4 = 2;
                try self.emitRegArith(.log_and, dest_reg, lhs_reg, rhs_reg);
                self.setLastResult(l.result.id, dest_reg);
            },

            .log_or => |l| {
                const lhs_reg = try self.getValueInReg(l.lhs, 0);
                const rhs_reg = try self.getValueInReg(l.rhs, 1);
                const dest_reg: u4 = 2;
                try self.emitRegArith(.log_or, dest_reg, lhs_reg, rhs_reg);
                self.setLastResult(l.result.id, dest_reg);
            },

            .log_not => |l| {
                const src_reg = try self.getValueInReg(l.operand, 0);
                const dest_reg: u4 = 1;
                try self.emitRegUnary(.log_not, dest_reg, src_reg);
                self.setLastResult(l.result.id, dest_reg);
            },

            .str_concat => |s| {
                // Register-based: str_concat rd, rs1, rs2
                // Format: [rd:4|rs1:4] [rs2:4|0]
                const rs1 = try self.getValueInReg(s.lhs, 0);
                const rs2 = try self.getValueInReg(s.rhs, 1);
                const rd: u4 = 2;
                try self.emitOpcode(.str_concat);
                try self.emitU8((@as(u8, rd) << 4) | rs1);
                try self.emitU8(@as(u8, rs2) << 4);
                self.setLastResult(s.result.id, rd);
            },

            .str_slice => |s| {
                // Emit: source string, start position, length/end
                // Register-based: get values into registers
                const src_reg = try self.getValueInReg(s.source, 0);
                const start_reg = try self.getValueInReg(s.start, 1);
                const len_reg = try self.getValueInReg(s.length_or_end, 2);
                const dest_reg: u4 = 3;
                try self.emitOpcode(.str_slice);
                try self.emitU8((@as(u8, dest_reg) << 4) | src_reg);
                try self.emitU8((@as(u8, start_reg) << 4) | len_reg);
                try self.emitU8(if (s.is_length) 1 else 0);
                self.setLastResult(s.result.id, dest_reg);
            },

            .str_slice_store => |s| {
                // str_substr_store: variable(start:length) = value OR variable(start,end) = value
                // Register-based: load target, compute slice, store back
                const target_reg = try self.getValueInReg(s.target, 0);
                const start_reg = try self.getValueInReg(s.start, 1);
                const len_reg = try self.getValueInReg(s.length_or_end, 2);
                const value_reg = try self.getValueInReg(s.value, 3);

                // Emit str_slice_store opcode with register operands
                try self.emitOpcode(.str_slice_store);
                try self.emitU8((@as(u8, target_reg) << 4) | start_reg);
                try self.emitU8((@as(u8, len_reg) << 4) | value_reg);
                try self.emitU8(if (s.is_length) 1 else 0);

                // Store the result back to the target variable
                if (self.value_slots.get(s.target_ptr.id)) |slot_info| {
                    const is_global = slot_info & 0x8000 != 0;
                    const slot = slot_info & 0x7FFF;
                    if (is_global) {
                        try self.emitRegStoreGlobal(target_reg, slot);
                    } else {
                        if (slot < 256) {
                            try self.emitRegStoreLocal(target_reg, @intCast(slot));
                        }
                    }
                } else {
                    debug.print(.emit, "WARNING: str_substr_store target_ptr id={d} not found in value_slots", .{s.target_ptr.id});
                }
            },

            .str_compare, .str_copy => {
                // These are handled by the builtin system
                return EmitError.InvalidInstruction;
            },

            .jump => |b| {
                try self.emitRegJmp(b.target);
            },

            .brif => |c| {
                // Conditional branch - jump to else if condition is false (jz)
                const cond_reg = try self.getValueInReg(c.condition, 0);
                try self.emitRegCondJmp(.jz, cond_reg, c.else_block);

                // Fall through to then block, or jump if not next
                try self.emitRegJmp(c.then_block);
            },

            .return_ => |r| {
                if (r) |val| {
                    const src_reg = try self.getValueInReg(val, 0);
                    try self.emitRegRet(src_reg);
                } else {
                    try self.emitRegRet(null);
                }
            },

            .call => |c| {
                // Check for built-in method calls (Console.WriteLine, console.log, etc.)
                // Method calls now have qualified names like "Console.WriteLine" in callee
                if (std.mem.eql(u8, c.callee, "Console.WriteLine")) {
                    // Console.WriteLine - register-based: load args to registers
                    for (c.args, 0..) |arg, i| {
                        if (i < 8) {
                            try self.emitValueToReg(arg, @intCast(i));
                        }
                    }
                    try self.emitOpcode(.console_writeln);
                    // Format: [rs:4|argc:4] [0] - 2 operand bytes
                    const argc: u8 = @intCast(c.args.len);
                    try self.emitU8((0 << 4) | (argc & 0xF)); // r0 in upper nibble, argc in lower
                    try self.emitU8(0); // Second unused byte
                    return; // No result value
                } else if (std.mem.eql(u8, c.callee, "Console.Write")) {
                    // Console.Write - register-based: load args to registers
                    for (c.args, 0..) |arg, i| {
                        if (i < 8) {
                            try self.emitValueToReg(arg, @intCast(i));
                        }
                    }
                    try self.emitOpcode(.console_write);
                    // Format: [rs:4|argc:4] [0] - 2 operand bytes
                    const argc: u8 = @intCast(c.args.len);
                    try self.emitU8((0 << 4) | (argc & 0xF)); // r0 in upper nibble, argc in lower
                    try self.emitU8(0); // Second unused byte
                    return;
                } else if (std.mem.eql(u8, c.callee, "console.log")) {
                    // console.log - register-based: load args to registers (writes to dev pane)
                    for (c.args, 0..) |arg, i| {
                        if (i < 8) {
                            try self.emitValueToReg(arg, @intCast(i));
                        }
                    }
                    try self.emitOpcode(.console_log);
                    // Format: [rs:4|argc:4] [0] - 2 operand bytes
                    const argc: u8 = @intCast(c.args.len);
                    try self.emitU8((0 << 4) | (argc & 0xF)); // r0 in upper nibble, argc in lower
                    try self.emitU8(0); // Second unused byte
                    return;
                }

                // Core Cot I/O functions: print(), println()
                if (io_functions.get(c.callee)) |opcode| {
                    // Register-based: ensure arguments are in registers and emit with register info
                    // For println/print, we expect the value in r0
                    if (c.args.len > 0) {
                        const arg = c.args[0];
                        // Check if we already have a register for this value
                        if (self.reg_alloc.getRegister(arg.id)) |arg_reg| {
                            // Move to r0 if not already there
                            if (arg_reg != 0) {
                                try self.emitRegMov(0, arg_reg);
                            }
                        } else {
                            // Value wasn't in a register, emit it to r0
                            try self.emitValueToReg(arg, 0);
                        }
                    }
                    try self.emitOpcode(opcode);
                    // Format: [rs:4|argc:4] [0] - 2 operand bytes
                    const argc: u8 = @intCast(c.args.len);
                    const reg_argc: u8 = (0 << 4) | (argc & 0xF); // r0 in upper nibble, argc in lower
                    try self.emitU8(reg_argc);
                    try self.emitU8(0); // Second unused byte
                    return;
                }

                // Check for performance-critical functions with dedicated opcodes
                if (opcode_builtins.get(c.callee)) |builtin| {
                    // Register-based: emit arguments to registers
                    for (c.args, 0..) |arg, i| {
                        if (i < 8) {
                            try self.emitValueToReg(arg, @intCast(i));
                        }
                    }
                    // Emit the builtin opcode
                    try self.emitOpcode(builtin.opcode);
                    return;
                }

                // Check for native functions (implemented in Zig, called via xcall)
                if (native_functions.has(c.callee)) {
                    // Load arguments into registers r0, r1, r2, ... (VM reads from registers)
                    // Two-pass approach: first emit last_result args (to prevent source clobbering),
                    // then emit all other args
                    if (self.last_result_value) |last_id| {
                        for (c.args, 0..) |arg, i| {
                            if (i < 8 and arg.id == last_id) {
                                try self.emitValueToReg(arg, @intCast(i));
                            }
                        }
                    }
                    for (c.args, 0..) |arg, i| {
                        if (i < 8) {
                            // Skip if already emitted in first pass
                            if (self.last_result_value) |last_id| {
                                if (arg.id == last_id) continue;
                            }
                            try self.emitValueToReg(arg, @intCast(i));
                        }
                    }
                    const name_idx = try self.addIdentifier(c.callee);
                    // Format: [opcode] [argc:4|0] [name_idx:16]
                    try self.emitOpcode(.call_dynamic);
                    try self.emitU8(@intCast(c.args.len << 4));
                    try self.emitU16(name_idx);
                    // Native functions return a value (result stays in r0)
                    return;
                }

                // Regular function call (user-defined functions)
                // Look up the routine index from our function_indices map
                if (self.function_indices.get(c.callee)) |routine_idx| {
                    // Register-based: emit arguments to registers r0..r(n-1)
                    // Two-pass approach: first emit last_result args to prevent clobbering
                    if (self.last_result_value) |last_id| {
                        for (c.args, 0..) |arg, i| {
                            if (i < 8 and arg.id == last_id) {
                                try self.emitValueToReg(arg, @intCast(i));
                            }
                        }
                    }
                    for (c.args, 0..) |arg, i| {
                        if (i < 8) {
                            if (self.last_result_value) |last_id| {
                                if (arg.id == last_id) continue;
                            }
                            try self.emitValueToReg(arg, @intCast(i));
                        }
                    }
                    // Format: [argc:4|0] [routine_idx:16]
                    try self.emitOpcode(.call);
                    const argc: u8 = @intCast(c.args.len);
                    try self.emitU8(argc << 4); // argc in upper nibble
                    try self.emitU16(routine_idx);
                } else {
                    // Function not found - emit call_dynamic for runtime resolution
                    // Load arguments into registers r0, r1, r2, ...
                    // Two-pass approach: first emit last_result args to prevent clobbering
                    if (self.last_result_value) |last_id| {
                        for (c.args, 0..) |arg, i| {
                            if (i < 8 and arg.id == last_id) {
                                try self.emitValueToReg(arg, @intCast(i));
                            }
                        }
                    }
                    for (c.args, 0..) |arg, i| {
                        if (i < 8) {
                            if (self.last_result_value) |last_id| {
                                if (arg.id == last_id) continue;
                            }
                            try self.emitValueToReg(arg, @intCast(i));
                        }
                    }
                    const name_idx = try self.addIdentifier(c.callee);
                    try self.emitOpcode(.call_dynamic);
                    try self.emitU8(@intCast(c.args.len << 4));
                    try self.emitU16(name_idx);
                }
            },

            // Note: xcall is converted to regular .call in the IR lowerer

            .io_open => |o| {
                // OPEN - emit xcall to native "open" function
                // Args: [cursor_id, filename] - mode is ignored (deprecated)
                // Register-based: load args to r0, r1
                try self.emitValueToReg(o.channel, 0);
                try self.emitValueToReg(o.filename, 1);
                const name_idx = try self.addIdentifier("open");
                // Format: [opcode] [argc:4|0] [name_idx:16]
                try self.emitOpcode(.call_dynamic);
                try self.emitU8(2 << 4); // 2 args in upper nibble
                try self.emitU16(name_idx);
            },

            .io_close => |c| {
                // CLOSE - emit xcall to native "close" function
                // Register-based: load channel to r0
                try self.emitValueToReg(c.channel, 0);
                const name_idx = try self.addIdentifier("close");
                // Format: [opcode] [argc:4|0] [name_idx:16]
                try self.emitOpcode(.call_dynamic);
                try self.emitU8(1 << 4); // 1 arg in upper nibble
                try self.emitU16(name_idx);
            },

            .io_read => |r| {
                // READ - emit xcall to native "read" function
                // For keyed reads: args = [cursor_id, key_num, key_value]
                // For sequential reads (reads): args = [cursor_id]
                // Register-based: load channel to r0
                try self.emitValueToReg(r.channel, 0);
                if (r.key) |key| {
                    // Keyed read: read(cursor_id, key_num, key_value)
                    // Load key_num as constant to r1
                    const key_num_idx = try self.addConstant(.{ .integer = r.qualifiers.key_index });
                    try self.emitOpcode(.load_const);
                    try self.emitU8(1); // r1 as destination
                    try self.emitU16(key_num_idx);
                    // Load key value to r2
                    try self.emitValueToReg(key, 2);
                    const name_idx = try self.addIdentifier("read");
                    // Format: [opcode] [argc:4|0] [name_idx:16]
                    try self.emitOpcode(.call_dynamic);
                    try self.emitU8(3 << 4); // 3 args in upper nibble
                    try self.emitU16(name_idx);
                    // Result is in r0
                } else {
                    // Sequential read: reads(cursor_id)
                    const name_idx = try self.addIdentifier("reads");
                    // Format: [opcode] [argc:4|0] [name_idx:16]
                    try self.emitOpcode(.call_dynamic);
                    try self.emitU8(1 << 4); // 1 arg in upper nibble
                    try self.emitU16(name_idx);
                    // Result is in r0
                }
                // If this is a structure read, emit store_record_buf to distribute to locals
                if (r.struct_name) |struct_name| {
                    try self.emitStoreRecordBuf(struct_name, r.base_name);
                }
            },

            .io_write => |w| {
                // WRITE - emit xcall to native "write" or "store" function
                // Args: [cursor_id, record_buffer]
                // Register-based: load args to r0, r1
                try self.emitValueToReg(w.channel, 0);
                try self.emitValueToReg(w.buffer, 1);
                const name_idx = try self.addIdentifier(if (w.is_insert) "store" else "write");
                // Format: [opcode] [argc:4|0] [name_idx:16]
                try self.emitOpcode(.call_dynamic);
                try self.emitU8(2 << 4); // 2 args in upper nibble
                try self.emitU16(name_idx);
            },

            // Note: io_store is now part of io_write with is_insert flag

            .load_struct_buf => |sb| {
                // Find the type index and type definition for this structure
                var type_idx: u16 = 0;
                var found = false;
                var type_def: ?*const module.TypeDef = null;
                debug.print(.emit, "load_struct_buf: base='{s}' struct='{s}', types.len={d}", .{ sb.base_name, sb.struct_name, self.types.items.len });
                for (self.types.items, 0..) |*t, i| {
                    // Look up the type name from constants
                    if (t.name_index < self.constants.items.len) {
                        const name_const = self.constants.items[t.name_index];
                        const type_name = switch (name_const) {
                            .string => |s| s,
                            .identifier => |s| s,
                            else => continue,
                        };
                        debug.print(.emit, "  type[{d}] name='{s}'", .{ i, type_name });
                        if (std.mem.eql(u8, type_name, sb.struct_name)) {
                            type_idx = @intCast(i);
                            type_def = t;
                            found = true;
                            break;
                        }
                    }
                }
                if (!found) {
                    debug.print(.emit, "WARNING: struct '{s}' not found in types!", .{sb.struct_name});
                }

                // Find local_base by looking up the first field's name
                var local_base: u16 = 0;
                if (type_def) |td| {
                    if (td.fields.len > 0) {
                        const first_field = td.fields[0];
                        // Get the field name from constants
                        if (first_field.name_index < self.constants.items.len) {
                            const field_name_const = self.constants.items[first_field.name_index];
                            const field_name = switch (field_name_const) {
                                .string => |s| s,
                                .identifier => |s| s,
                                else => "",
                            };
                            // Try plain field name first (for named records like "record customer")
                            debug.print(.emit, "load_struct_buf: looking up first field '{s}'", .{field_name});
                            if (self.locals.get(field_name)) |loc| {
                                local_base = loc.slot;
                                debug.print(.emit, "load_struct_buf: local_base={d} (plain name)", .{local_base});
                            } else {
                                // Try qualified name: base_name.field_name (for struct-typed fields)
                                var qualified_buf: [256]u8 = undefined;
                                const qualified_name = std.fmt.bufPrint(&qualified_buf, "{s}.{s}", .{ sb.base_name, field_name }) catch "";
                                debug.print(.emit, "load_struct_buf: trying qualified name '{s}'", .{qualified_name});
                                if (self.locals.get(qualified_name)) |loc| {
                                    local_base = loc.slot;
                                    debug.print(.emit, "load_struct_buf: local_base={d} (qualified)", .{local_base});
                                } else {
                                    debug.print(.emit, "WARNING: first field '{s}' not found in locals!", .{field_name});
                                }
                            }
                        }
                    }
                }

                // Allocate a register for the result and emit the opcode
                // Format: [rd:4|0] [type_idx:16] [local_base:16]
                const rd = try self.getOrAllocReg(sb.result);
                debug.print(.emit, "load_struct_buf: allocated r{d} for result", .{rd});
                try self.emitOpcode(.load_record_buf);
                try self.emitU8(@as(u8, rd) << 4);
                try self.emitU16(type_idx);
                try self.emitU16(local_base);
                // Track this register for the result value
                self.last_result_value = sb.result.id;
                self.last_result_reg = rd;
            },

            .store_struct_buf => |sb| {
                // Register-based: load value (buffer from db_read) to r0
                try self.emitValueToReg(sb.value, 0);
                // Then emit store_record_buf to unpack it into struct fields
                debug.print(.emit, "store_struct_buf: base='{s}' struct='{s}'", .{ sb.base_name, sb.struct_name });
                try self.emitStoreRecordBuf(sb.struct_name, sb.base_name);
            },

            .debug_line => |d| {
                // Emit debug line info for debugger
                // Format: [0] [line:16]
                try self.emitOpcode(.debug_line);
                try self.emitU8(0); // Register field (unused for debug_line)
                try self.emitU16(@intCast(d.line));
            },

            // Exception handling - using modern try/catch
            .try_begin => |t| {
                // Set up error handler to catch block
                try self.emitOpcode(.set_error_handler);
                try self.addPendingJump(t.catch_block, false);
                try self.emitI16(0); // Placeholder - will be patched
            },

            .try_end => {
                // Clear the error handler (normal exit from try block)
                try self.emitOpcode(.clear_error_handler);
            },

            .catch_begin => {
                // Start of catch block - error handler clears itself
                try self.emitOpcode(.nop);
            },

            .throw => |t| {
                // Throw exception - emit value and trigger error
                // Note: Runtime doesn't have throw opcode yet, use error jump instead
                // Register-based: load value to r0
                try self.emitValueToReg(t.value, 0);
                // For now, just emit nop (throw not implemented)
                try self.emitOpcode(.nop);
            },

            .array_load => |al| {
                // Array element access: array[index]
                // Get the array slot
                const array_slot = if (self.value_slots.get(al.array_ptr.id)) |slot_info|
                    slot_info & 0x7FFF
                else
                    0;

                // Register-based: load index to r0
                try self.emitValueToReg(al.index, 0);

                // Emit array_load opcode with the slot
                try self.emitOpcode(.array_load);
                try self.emitU8(0); // index is in r0, result goes to r0
                try self.emitU16(array_slot);

                // Result is in r0
                self.setLastResult(al.result.id, 0);
            },

            .array_store => |as| {
                // Array element assignment: array[index] = value
                // Get the array slot
                const array_slot = if (self.value_slots.get(as.array_ptr.id)) |slot_info|
                    slot_info & 0x7FFF
                else
                    0;

                // Register-based: load index to r0, value to r1
                try self.emitValueToReg(as.index, 0);
                try self.emitValueToReg(as.value, 1);

                // Emit array_store opcode with the slot
                try self.emitOpcode(.array_store);
                try self.emitU8((0 << 4) | 1); // r0 = index, r1 = value
                try self.emitU16(array_slot);
            },

            .br_table => |s| {
                // Switch/jump table - emit as a series of comparisons for now
                // Future: could emit as computed jump table for dense integer ranges
                //
                // For each case:
                //   load case_value to r1
                //   cmp_eq r0, r1 -> r2
                //   jnz r2, case_target
                // After all cases: jmp default

                // Load the switch value to r0
                const val_reg = try self.getValueInReg(s.value, 0);
                _ = val_reg;

                for (s.cases) |case| {
                    // Load case constant to r1
                    const case_idx = try self.addConstant(.{ .integer = case.value });
                    try self.emitOpcode(.load_const);
                    try self.emitU8(1); // r1
                    try self.emitU16(case_idx);

                    // Compare r0 == r1, result in r2
                    try self.emitOpcode(.cmp_eq);
                    try self.emitU8((0 << 4) | 1); // r0, r1
                    try self.emitU8(2); // result in r2

                    // Jump to case block if equal (jnz r2, target)
                    try self.emitRegCondJmp(.jnz, 2, case.target);
                }

                // Fall through to default
                try self.emitRegJmp(s.default);
            },

            else => {
                // Other instructions not yet implemented
            },
        }
    }

    /// Emit store_record_buf with type_idx and local_base
    fn emitStoreRecordBuf(self: *Self, struct_name: []const u8, base_name: ?[]const u8) EmitError!void {
        // Find the type index for this structure
        var type_idx: u16 = 0;
        var type_def: ?*const module.TypeDef = null;
        for (self.types.items, 0..) |*t, i| {
            if (t.name_index < self.constants.items.len) {
                const name_const = self.constants.items[t.name_index];
                const type_name = switch (name_const) {
                    .string => |s| s,
                    .identifier => |s| s,
                    else => continue,
                };
                if (std.mem.eql(u8, type_name, struct_name)) {
                    type_idx = @intCast(i);
                    type_def = t;
                    break;
                }
            }
        }

        // Calculate local_base from base_name (same logic as load_struct_buf)
        var local_base: u16 = 0;
        if (type_def) |td| {
            if (td.fields.len > 0) {
                const first_field = td.fields[0];
                if (first_field.name_index < self.constants.items.len) {
                    const field_name_const = self.constants.items[first_field.name_index];
                    const field_name = switch (field_name_const) {
                        .string => |s| s,
                        .identifier => |s| s,
                        else => "",
                    };
                    // Try plain field name first (for named records like "record customer")
                    if (self.locals.get(field_name)) |loc| {
                        local_base = loc.slot;
                    } else if (base_name) |bn| {
                        // Try qualified name: base_name.field_name (for struct-typed fields)
                        var qualified_buf: [256]u8 = undefined;
                        const qualified_name = std.fmt.bufPrint(&qualified_buf, "{s}.{s}", .{ bn, field_name }) catch "";
                        if (self.locals.get(qualified_name)) |loc| {
                            local_base = loc.slot;
                        }
                    }
                }
            }
        }

        // Register-based: store_record_buf reads from r0 (set by call_dynamic)
        try self.emitOpcode(.store_record_buf);
        try self.emitU16(type_idx);
        try self.emitU16(local_base);
    }

    /// Emit opcode byte
    fn emitOpcode(self: *Self, op: Opcode) EmitError!void {
        try self.code.append(self.allocator, @intFromEnum(op));
    }

    /// Emit unsigned 8-bit value
    fn emitU8(self: *Self, val: u8) EmitError!void {
        try self.code.append(self.allocator, val);
    }

    /// Emit unsigned 16-bit value (little-endian)
    fn emitU16(self: *Self, val: u16) EmitError!void {
        try self.code.appendSlice(self.allocator, &std.mem.toBytes(val));
    }

    /// Emit signed 16-bit value (little-endian)
    fn emitI16(self: *Self, val: i16) EmitError!void {
        try self.code.appendSlice(self.allocator, &std.mem.toBytes(val));
    }

    /// Emit unsigned 32-bit value (little-endian)
    fn emitU32(self: *Self, val: u32) EmitError!void {
        try self.code.appendSlice(self.allocator, &std.mem.toBytes(val));
    }

    /// Emit signed 32-bit value (little-endian)
    fn emitI32(self: *Self, val: i32) EmitError!void {
        try self.code.appendSlice(self.allocator, &std.mem.toBytes(val));
    }

    /// Emit load local instruction (stack-based fallback using r0 as temp)
    fn emitLoadLocal(self: *Self, slot: u16) EmitError!void {
        // Use register-based load_local with r0 as implicit destination
        if (slot < 256) {
            try self.emitOpcode(.load_local);
            try self.emitU8(0); // r0 as destination
            try self.emitU8(@intCast(slot));
        } else {
            try self.emitOpcode(.load_local16);
            try self.emitU8(0); // r0 as destination
            try self.emitU16(slot);
        }
    }

    /// Emit store local instruction (stack-based fallback using r0 as temp)
    fn emitStoreLocal(self: *Self, slot: u16) EmitError!void {
        // Use register-based store_local with r0 as implicit source
        if (slot < 256) {
            try self.emitOpcode(.store_local);
            try self.emitU8(0); // r0 as source
            try self.emitU8(@intCast(slot));
        } else {
            try self.emitOpcode(.store_local16);
            try self.emitU8(0); // r0 as source
            try self.emitU16(slot);
        }
    }

    /// Emit load global instruction (using r0 as destination for stack-based compatibility)
    fn emitLoadGlobal(self: *Self, slot: u16) EmitError!void {
        try self.emitOpcode(.load_global);
        try self.emitU8(0); // r0 as destination
        try self.emitU16(slot);
    }

    /// Emit store global instruction (using r0 as source for stack-based compatibility)
    fn emitStoreGlobal(self: *Self, slot: u16) EmitError!void {
        try self.emitOpcode(.store_global);
        try self.emitU8(0); // r0 as source
        try self.emitU16(slot);
    }

    // ============================================
    // Register-Based Bytecode Emission Helpers
    // ============================================

    /// Emit register arithmetic: op dest, src1, src2
    fn emitRegArith(self: *Self, op: Opcode, dest: u4, src1: u4, src2: u4) EmitError!void {
        try self.emitOpcode(op);
        try self.emitU8((@as(u8, dest) << 4) | @as(u8, src1));
        try self.emitU8(@as(u8, src2) << 4);
    }

    /// Emit register unary: op dest, src (neg, not, etc.)
    fn emitRegUnary(self: *Self, op: Opcode, dest: u4, src: u4) EmitError!void {
        try self.emitOpcode(op);
        try self.emitU8((@as(u8, dest) << 4) | @as(u8, src));
        try self.emitU8(0);
    }

    /// Emit register immediate arithmetic: op dest, src, imm8
    fn emitRegArithImm(self: *Self, op: Opcode, dest: u4, src: u4, imm: i8) EmitError!void {
        try self.emitOpcode(op);
        try self.emitU8((@as(u8, dest) << 4) | @as(u8, src));
        try self.emitU8(@bitCast(imm));
    }

    /// Emit register move: mov dest, src
    fn emitRegMov(self: *Self, dest: u4, src: u4) EmitError!void {
        try self.emitOpcode(.mov);
        try self.emitU8((@as(u8, dest) << 4) | @as(u8, src));
        try self.emitU8(0);
    }

    /// Emit register move immediate: movi dest, imm8
    fn emitRegMovImm(self: *Self, dest: u4, imm: i8) EmitError!void {
        try self.emitOpcode(.movi);
        try self.emitU8(@as(u8, dest) << 4);
        try self.emitU8(@bitCast(imm));
    }

    /// Emit register move immediate 16: movi16 dest, imm16
    fn emitRegMovImm16(self: *Self, dest: u4, imm: i16) EmitError!void {
        try self.emitOpcode(.movi16);
        try self.emitU8(@as(u8, dest) << 4);
        try self.emitI16(imm);
    }

    /// Emit register load constant: load_const dest, const_idx
    fn emitRegLoadConst(self: *Self, dest: u4, const_idx: u16) EmitError!void {
        try self.emitOpcode(.load_const);
        try self.emitU8(@as(u8, dest) << 4);
        try self.emitU16(const_idx);
    }

    /// Emit register load null/true/false
    fn emitRegLoadLiteral(self: *Self, op: Opcode, dest: u4) EmitError!void {
        try self.emitOpcode(op);
        try self.emitU8(@as(u8, dest) << 4);
        try self.emitU8(0);
    }

    /// Emit register load local: load_local dest, slot
    fn emitRegLoadLocal(self: *Self, dest: u4, slot: u8) EmitError!void {
        try self.emitOpcode(.load_local);
        try self.emitU8(@as(u8, dest) << 4);
        try self.emitU8(slot);
    }

    /// Emit register store local: store_local src, slot
    fn emitRegStoreLocal(self: *Self, src: u4, slot: u8) EmitError!void {
        try self.emitOpcode(.store_local);
        try self.emitU8(@as(u8, src) << 4);
        try self.emitU8(slot);
    }

    /// Emit register load global: load_global dest, global_idx
    fn emitRegLoadGlobal(self: *Self, dest: u4, idx: u16) EmitError!void {
        try self.emitOpcode(.load_global);
        try self.emitU8(@as(u8, dest) << 4);
        try self.emitU16(idx);
    }

    /// Emit register store global: store_global src, global_idx
    fn emitRegStoreGlobal(self: *Self, src: u4, idx: u16) EmitError!void {
        try self.emitOpcode(.store_global);
        try self.emitU8(@as(u8, src) << 4);
        try self.emitU16(idx);
    }

    /// Emit register comparison: cmp dest, src1, src2
    fn emitRegCmp(self: *Self, op: Opcode, dest: u4, src1: u4, src2: u4) EmitError!void {
        try self.emitOpcode(op);
        try self.emitU8((@as(u8, dest) << 4) | @as(u8, src1));
        try self.emitU8(@as(u8, src2) << 4);
    }

    /// Emit register jump: jmp offset
    fn emitRegJmp(self: *Self, target: *const ir.Block) EmitError!void {
        try self.emitOpcode(.jmp);
        try self.emitU8(0); // Unused first byte
        try self.addPendingJump(target, false);
        try self.emitI16(0); // Placeholder
    }

    /// Emit register conditional jump: jz/jnz cond, offset
    fn emitRegCondJmp(self: *Self, op: Opcode, cond: u4, target: *const ir.Block) EmitError!void {
        try self.emitOpcode(op);
        try self.emitU8(@as(u8, cond) << 4);
        try self.addPendingJump(target, false);
        try self.emitI16(0); // Placeholder
    }

    /// Emit register call: call argc, routine_idx
    fn emitRegCall(self: *Self, argc: u4, routine_idx: u16) EmitError!void {
        try self.emitOpcode(.call);
        try self.emitU8(@as(u8, argc) << 4);
        try self.emitU16(routine_idx);
    }

    /// Emit register return: ret or ret_val src
    fn emitRegRet(self: *Self, src: ?u4) EmitError!void {
        if (src) |s| {
            try self.emitOpcode(.ret_val);
            try self.emitU8(@as(u8, s) << 4);
        } else {
            try self.emitOpcode(.ret);
            try self.emitU8(0);
        }
        try self.emitU8(0);
    }

    /// Get or allocate a register for an IR value
    fn getOrAllocReg(self: *Self, value: ir.Value) EmitError!u4 {
        // Check if value already has a register
        if (self.reg_alloc.getRegister(value.id)) |reg| {
            return reg;
        }

        // Allocate a new register
        if (self.reg_alloc.allocate(value.id)) |reg| {
            return reg;
        }

        // Out of registers - need to spill (fall back to stack-based for now)
        std.debug.print("[EMIT] ERROR: Out of registers for value.id={d}, free_regs=0x{x:0>4}\n", .{ value.id, self.reg_alloc.free_regs });
        return EmitError.TooManyLocals;
    }

    /// Emit a value into a specific register (for register-based mode)
    fn emitValueToReg(self: *Self, value: ir.Value, dest: u4) EmitError!void {
        // Check if this is the last computed result (SSA chaining)
        if (self.last_result_value) |last_id| {
            if (last_id == value.id) {
                if (self.last_result_reg != dest) {
                    try self.emitRegMov(dest, self.last_result_reg);
                }
                return;
            }
        }

        // If value is already in a register, move it
        if (self.reg_alloc.getRegister(value.id)) |src| {
            if (src != dest) {
                try self.emitRegMov(dest, src);
            }
            return;
        }

        // Check if value is a constant - load from constant pool
        if (self.value_consts.get(value.id)) |const_idx| {
            try self.emitRegLoadConst(dest, const_idx);
            return;
        }

        // Otherwise, load from value source based on how it was defined
        if (self.value_slots.get(value.id)) |slot_info| {
            if (slot_info & 0x8000 != 0) {
                // Global variable
                const slot = slot_info & 0x7FFF;
                try self.emitRegLoadGlobal(dest, slot);
            } else {
                // Local variable
                if (slot_info < 256) {
                    try self.emitRegLoadLocal(dest, @intCast(slot_info));
                } else {
                    return EmitError.TooManyLocals;
                }
            }
        }
    }

    /// Get or allocate a register for a value, or load it into a temp register if it's a constant/slot
    /// This version prefers using existing registers or loading on demand without permanent allocation
    fn getValueInReg(self: *Self, value: ir.Value, temp_reg: u4) EmitError!u4 {
        // Check if this is the last computed result (SSA chaining)
        if (self.last_result_value) |last_id| {
            if (last_id == value.id) {
                return self.last_result_reg;
            }
        }

        // If value is already in a register, use that
        if (self.reg_alloc.getRegister(value.id)) |src| {
            return src;
        }

        // Check if value is a constant - load into temp register
        if (self.value_consts.get(value.id)) |const_idx| {
            try self.emitRegLoadConst(temp_reg, const_idx);
            return temp_reg;
        }

        // Check if value is in a slot - load into temp register
        if (self.value_slots.get(value.id)) |slot_info| {
            if (slot_info & 0x8000 != 0) {
                // Global variable
                const slot = slot_info & 0x7FFF;
                try self.emitRegLoadGlobal(temp_reg, slot);
            } else {
                // Local variable
                if (slot_info < 256) {
                    try self.emitRegLoadLocal(temp_reg, @intCast(slot_info));
                } else {
                    return EmitError.TooManyLocals;
                }
            }
            return temp_reg;
        }

        // Fall back to allocating a new register (this shouldn't happen often)
        debug.print(.emit, "getValueInReg fallback: value.id={d}, ty={any}", .{ value.id, value.ty });
        return try self.getOrAllocReg(value);
    }

    /// Set the last computed result (for SSA value chains without permanent register allocation)
    fn setLastResult(self: *Self, value_id: u32, reg: u4) void {
        self.last_result_value = value_id;
        self.last_result_reg = reg;
    }

    /// Emit integer value to r0 (for stack-based compatibility)
    fn emitPushInt(self: *Self, val: i64) EmitError!void {
        // Use register-based movi with r0 as destination
        if (val >= -128 and val <= 127) {
            try self.emitOpcode(.movi);
            try self.emitU8(0); // r0 as destination
            try self.emitU8(@bitCast(@as(i8, @intCast(val))));
        } else if (val >= -32768 and val <= 32767) {
            try self.emitOpcode(.movi16);
            try self.emitU8(0); // r0 as destination
            try self.emitI16(@intCast(val));
        } else if (val >= -2147483648 and val <= 2147483647) {
            try self.emitOpcode(.movi32);
            try self.emitU8(0); // r0 as destination
            try self.emitI32(@intCast(val));
        } else {
            // For 64-bit values, load from constant pool
            const const_idx = try self.addConstant(.{ .integer = val });
            try self.emitOpcode(.load_const);
            try self.emitU8(0); // r0 as destination
            try self.emitU16(const_idx);
        }
    }

    /// Emit jump to block
    fn emitJump(self: *Self, target: *const ir.Block) EmitError!void {
        try self.emitOpcode(.jmp);
        try self.emitU8(0); // Unused byte
        try self.addPendingJump(target, false);
        try self.emitI16(0); // Placeholder
    }

    /// Add pending jump for later patching
    fn addPendingJump(self: *Self, target_block: *const ir.Block, is_wide: bool) EmitError!void {
        try self.pending_jumps.append(self.allocator, .{
            .patch_offset = @intCast(self.code.items.len),
            .target_block = target_block,
            .is_wide = is_wide,
        });
    }

    /// Patch all pending jumps with actual offsets
    fn patchJumps(self: *Self) EmitError!void {
        for (self.pending_jumps.items) |jump| {
            const target_offset = self.block_offsets.get(jump.target_block) orelse
                return EmitError.UndefinedBlock;

            const current_offset = jump.patch_offset + 2; // After the offset bytes
            const relative_offset = @as(i32, @intCast(target_offset)) - @as(i32, @intCast(current_offset));

            if (jump.is_wide) {
                std.mem.writeInt(i32, self.code.items[jump.patch_offset..][0..4], relative_offset, .little);
            } else {
                if (relative_offset > 32767 or relative_offset < -32768) {
                    return EmitError.JumpTooFar;
                }
                std.mem.writeInt(i16, self.code.items[jump.patch_offset..][0..2], @intCast(relative_offset), .little);
            }
        }
    }

    /// Add constant to pool, returning index
    fn addConstant(self: *Self, constant: module.Constant) EmitError!u16 {
        const idx = self.constants.items.len;
        if (idx > 65535) return EmitError.TooManyConstants;
        try self.constants.append(self.allocator, constant);
        return @intCast(idx);
    }

    /// Add string constant with deduplication
    fn addString(self: *Self, str: []const u8) EmitError!u16 {
        if (self.string_pool.get(str)) |idx| {
            return idx;
        }
        // Duplicate the string so the module owns it
        const owned_str = self.allocator.dupe(u8, str) catch return EmitError.OutOfMemory;
        const idx = try self.addConstant(.{ .string = owned_str });
        try self.string_pool.put(owned_str, idx);
        return idx;
    }

    /// Add identifier constant with deduplication
    fn addIdentifier(self: *Self, name: []const u8) EmitError!u16 {
        if (self.string_pool.get(name)) |idx| {
            return idx;
        }
        // Duplicate the string so the module owns it
        const owned_name = self.allocator.dupe(u8, name) catch return EmitError.OutOfMemory;
        const idx = try self.addConstant(.{ .identifier = owned_name });
        try self.string_pool.put(owned_name, idx);
        return idx;
    }

};

/// Convert IR type to bytecode data type code
fn irTypeToDataType(ty: ir.Type) module.DataTypeCode {
    return switch (ty) {
        .void => .string, // Void maps to empty string
        .bool => .int8, // Boolean is 1 byte
        .i8 => .int8,
        .i16 => .int16,
        .i32 => .int32,
        .i64 => .int64,
        .u8 => .int8,
        .u16 => .int16,
        .u32 => .int32,
        .u64 => .int64,
        .f32 => .fixed_point,
        .f64 => .fixed_point,
        .string => .string,
        .string_fixed => .string,
        .decimal => .decimal,
        .ptr => .int64, // Pointers are 64-bit
        .optional => .int64, // Optionals are pointer-sized
        .array => .string, // Arrays treated as string for now
        .slice => .string, // Slices treated as string for now
        .@"struct" => .structure, // Structs are structures
        .function => .int64, // Function pointers
    };
}

// ============================================================================
// Public API
// ============================================================================

/// Emit bytecode from IR module
pub fn emitBytecode(allocator: Allocator, ir_module: *const ir.Module) EmitError!module.Module {
    var emitter = BytecodeEmitter.init(allocator);
    defer emitter.deinit();
    return emitter.emit(ir_module);
}

// ============================================================================
// Tests
// ============================================================================

test "emit simple bytecode" {
    const allocator = std.testing.allocator;

    var ir_mod = ir.Module.init(allocator, "test");
    defer ir_mod.deinit();

    const result = try emitBytecode(allocator, &ir_mod);
    defer {
        allocator.free(result.code);
        allocator.free(result.constants);
        allocator.free(result.types);
        allocator.free(result.routines);
    }

    try std.testing.expect(result.header.isValid());
}
