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

// Builtin function definitions (extracted to reduce file size)
const emit_builtins = @import("emit_builtins.zig");
const BuiltinDef = emit_builtins.BuiltinDef;
const opcode_builtins = emit_builtins.opcode_builtins;
const io_functions = emit_builtins.io_functions;
const native_functions = emit_builtins.native_functions;

// Instruction emission handlers (extracted to reduce file size)
const emit_inst = @import("emit_instruction.zig");

// Scoped logging (Ghostty pattern) - enable with std_options or runtime filter
const log = std.log.scoped(.@"bytecode-emit");

const Allocator = std.mem.Allocator;
const Opcode = opcodes.Opcode;

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

/// Error context for better error reporting
pub const ErrorContext = struct {
    message: []const u8,
    detail: []const u8,
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

/// Field view (overlay) information for DBL record field semantics
/// When a store targets a field view, we need to use str_slice_store
/// to write through to the underlying buffer at the correct offset
const FieldViewInfo = struct {
    base_slot: u16, // Slot of the underlying buffer
    byte_offset: u16, // Byte offset into the buffer
    length: u16, // Length of this field in bytes
    is_global: bool, // Whether the base slot is a global
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
    pending_jumps: std.ArrayListUnmanaged(PendingJump),

    // IR value to stack slot mapping
    value_slots: std.AutoHashMap(u32, u16), // value_id -> stack_slot

    // IR value to constant pool index (for constants that can be loaded on demand)
    value_consts: std.AutoHashMap(u32, u16), // value_id -> constant_pool_index

    // Field view info for DBL record field overlays
    // Maps IR value ID (from ptr_offset result) to field view info
    // Used by emitStore to emit str_slice_store for field overlay write-through
    field_view_info: std.AutoHashMap(u32, FieldViewInfo),

    // Array pointer target tracking
    // When we store an array value into a pointer variable, record the array slot
    // so that array_load can use the correct slot when accessing through the pointer
    array_ptr_targets: std.AutoHashMap(u32, u16), // value_id -> array_slot

    // Current routine being emitted
    current_routine_start: u32,

    // Current function being emitted (for label lookup)
    current_func: ?*const ir.Function,

    // IR module being emitted (for looking up function signatures)
    ir_module: ?*const ir.Module,

    // Register allocator for register-based bytecode
    reg_alloc: RegisterAllocator,

    // Track the last computed intermediate result (for SSA value chains)
    // This avoids permanently allocating registers for temp results
    last_result_value: ?u32 = null,
    last_result_reg: u4 = 0,

    // Register spilling support
    // When all registers are in use, we spill a register to a stack slot
    spill_slot_base: u16 = 0, // First slot available for spilling
    next_spill_slot: u16 = 0, // Next available spill slot
    spilled_values: std.AutoHashMap(u32, u16), // value_id -> spill_slot
    max_spill_slots_used: u16 = 0, // Track maximum for local_count

    // Track allocated strings that need to be freed (e.g., qualified field names)
    allocated_strings: std.ArrayListUnmanaged([]const u8) = .{},

    // Error context for better error reporting
    last_error: ?ErrorContext = null,

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
            .field_view_info = std.AutoHashMap(u32, FieldViewInfo).init(allocator),
            .array_ptr_targets = std.AutoHashMap(u32, u16).init(allocator),
            .current_routine_start = 0,
            .current_func = null,
            .ir_module = null,
            .reg_alloc = RegisterAllocator.init(allocator),
            .spilled_values = std.AutoHashMap(u32, u16).init(allocator),
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
        self.field_view_info.deinit();
        self.array_ptr_targets.deinit();
        self.reg_alloc.deinit();
        self.spilled_values.deinit();
        // Free allocated strings (qualified field names, etc.)
        for (self.allocated_strings.items) |s| {
            self.allocator.free(s);
        }
        self.allocated_strings.deinit(self.allocator);
        // Free error context strings
        if (self.last_error) |err| {
            self.allocator.free(err.message);
            self.allocator.free(err.detail);
        }
    }

    /// Set error context for better error reporting
    pub fn setError(self: *Self, comptime message_fmt: []const u8, message_args: anytype, comptime detail_fmt: []const u8, detail_args: anytype) void {
        // Free previous error if any
        if (self.last_error) |err| {
            self.allocator.free(err.message);
            self.allocator.free(err.detail);
        }
        self.last_error = .{
            .message = std.fmt.allocPrint(self.allocator, message_fmt, message_args) catch "allocation failed",
            .detail = std.fmt.allocPrint(self.allocator, detail_fmt, detail_args) catch "",
        };
    }

    /// Get the last error context
    pub fn getLastError(self: *const Self) ?ErrorContext {
        return self.last_error;
    }

    /// Emit bytecode module from IR module
    pub fn emit(self: *Self, ir_module: *const ir.Module) EmitError!module.Module {
        debug.print(.emit, "emit() called for module with {d} functions", .{ir_module.functions.items.len});

        // Store IR module reference for looking up function signatures during emission
        self.ir_module = ir_module;

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
                // Track allocated string for cleanup
                self.allocated_strings.append(self.allocator, qualified_name) catch return EmitError.OutOfMemory;

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

        // Build vtables from IR module (duplicate strings so module owns them)
        var vtables_list: std.ArrayListUnmanaged(module.VTableDef) = .{};
        if (self.ir_module) |ir_mod| {
            for (ir_mod.vtables.items) |ir_vtable| {
                var methods_list: std.ArrayListUnmanaged(module.VTableMethod) = .{};
                for (ir_vtable.methods) |ir_method| {
                    try methods_list.append(self.allocator, .{
                        .method_name = try self.allocator.dupe(u8, ir_method.method_name),
                        .fn_name = try self.allocator.dupe(u8, ir_method.fn_name),
                    });
                }
                try vtables_list.append(self.allocator, .{
                    .trait_name = try self.allocator.dupe(u8, ir_vtable.trait_name),
                    .type_name = try self.allocator.dupe(u8, ir_vtable.type_name),
                    .methods = try methods_list.toOwnedSlice(self.allocator),
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
            .vtables = try vtables_list.toOwnedSlice(self.allocator),
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
                .precision = if (field.ty == .implied_decimal) field.ty.implied_decimal.scale else 0,
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
        self.resetSpillState(); // Reset spill state for new function

        self.current_routine_start = @intCast(self.code.items.len);

        // Allocate locals for parameters
        // For struct-typed parameters, flatten into individual field slots with qualified names
        for (func.signature.params) |param| {
            if (param.ty == .@"struct") {
                // Structure parameter - allocate slots for each field with qualified names
                const struct_type = param.ty.@"struct";
                const base_slot = self.local_count;

                // Also register the base parameter name pointing to the first field slot
                // This allows the alloca for this parameter to find the existing registration
                try self.locals.put(param.name, .{
                    .slot = base_slot,
                    .is_global = false,
                });

                for (struct_type.fields) |field| {
                    const qualified_name = std.fmt.allocPrint(
                        self.allocator,
                        "{s}.{s}",
                        .{ param.name, field.name },
                    ) catch return EmitError.OutOfMemory;
                    // Track allocated string for cleanup
                    self.allocated_strings.append(self.allocator, qualified_name) catch return EmitError.OutOfMemory;
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

        // Pre-pass: count all allocas in all blocks to know total local count
        // This ensures spill slots don't overlap with regular locals
        for (func.blocks.items) |block| {
            for (block.instructions.items) |inst| {
                if (inst == .alloca) {
                    const a = inst.alloca;
                    // For struct allocas, don't check globals - globals contains TYPE names,
                    // but struct instances should still get local slots
                    const is_struct = a.ty == .@"struct";
                    const skip_global_check = is_struct;
                    const in_globals = if (skip_global_check) false else self.globals.contains(a.name);
                    debug.print(.emit, "Pre-pass alloca: name='{s}' ty={any} global={} local={} skip_global={}", .{ a.name, a.ty, in_globals, self.locals.contains(a.name), skip_global_check });
                    if (!in_globals and !self.locals.contains(a.name)) {
                        if (is_struct) {
                            // Struct type: count FLATTENED slots for nested structs
                            const slot_count = emit_inst.getSlotCount(a.ty);
                            debug.print(.emit, "  -> counted as {d} slots for struct fields (flattened)", .{slot_count});
                            self.local_count += @intCast(slot_count);
                        } else if (a.ty == .array) {
                            // Array of u8 (DBL alpha/string buffer): single slot for the buffer value
                            // Other arrays: one slot per element (legacy behavior)
                            if (a.ty.array.element.* == .u8) {
                                debug.print(.emit, "  -> u8 array (buffer), counted as 1 slot, local_count now={d}", .{self.local_count + 1});
                                self.local_count += 1;
                            } else {
                                self.local_count += @intCast(a.ty.array.length);
                            }
                        } else {
                            // Primitive type: one slot
                            debug.print(.emit, "  -> counted as 1 slot, local_count now={d}", .{self.local_count + 1});
                            self.local_count += 1;
                        }
                    }
                }
            }
        }
        // Reset for actual emission (allocas will be processed again)
        const total_declared_locals = self.local_count;
        debug.print(.emit, "Pre-pass done: total_declared_locals={d}, spill_slot_base will be {d}", .{ total_declared_locals, total_declared_locals });
        // Count actual parameter slots (struct params expand to multiple slots)
        var param_slot_count: u16 = 0;
        for (func.signature.params) |param| {
            // Use flattened slot count for nested structs
            param_slot_count += @intCast(emit_inst.getSlotCount(param.ty));
        }
        self.local_count = param_slot_count;

        // Setup spill slots after ALL declared locals are accounted for
        self.spill_slot_base = total_declared_locals;
        self.next_spill_slot = total_declared_locals;

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

        // Build params array with ref info
        var params_list: std.ArrayListUnmanaged(module.ParamDef) = .empty;
        for (func.signature.params) |param| {
            const param_name_idx = try self.addIdentifier(param.name);
            try params_list.append(self.allocator, .{
                .name_index = param_name_idx,
                .data_type = irTypeToDataType(param.ty),
                .mode = if (param.is_ref) .ref else .val,
                .default_value = null,
            });
        }

        // Calculate total locals including any spill slots used
        const total_locals = self.local_count + self.max_spill_slots_used;

        try self.routines.append(self.allocator, .{
            .name_index = name_idx,
            .flags = if (func.linkage == .external) module.RoutineFlags{ .is_public = true } else module.RoutineFlags{},
            .code_offset = self.current_routine_start,
            .code_length = code_len,
            .param_count = @intCast(func.signature.params.len),
            .local_count = total_locals,
            .max_stack = 0, // Register-based: stack not used
            .params = try params_list.toOwnedSlice(self.allocator),
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
            // Memory operations
            .alloca => |a| try emit_inst.emitAlloca(self, a),
            .load => |l| try emit_inst.emitLoad(self, l),
            .store => |s| try emit_inst.emitStore(self, s),
            .field_ptr => |fp| try emit_inst.emitFieldPtr(self, fp),

            // Constants
            .iconst => |c| try emit_inst.emitIconst(self, c),
            .f32const => |c| try emit_inst.emitF32const(self, c),
            .f64const => |c| try emit_inst.emitF64const(self, c),
            .const_string => |c| try emit_inst.emitConstString(self, c),
            .const_null => {},

            // Arithmetic
            .iadd => |a| try emit_inst.emitBinaryArith(self, .add, a.lhs, a.rhs, a.result),
            .isub => |s| try emit_inst.emitBinaryArith(self, .sub, s.lhs, s.rhs, s.result),
            .imul => |m| try emit_inst.emitBinaryArith(self, .mul, m.lhs, m.rhs, m.result),
            .sdiv, .udiv => |d| try emit_inst.emitBinaryArith(self, .div, d.lhs, d.rhs, d.result),
            .srem, .urem => |m| try emit_inst.emitBinaryArith(self, .mod, m.lhs, m.rhs, m.result),
            .round => |r| try emit_inst.emitRound(self, r),
            .trunc => |t| try emit_inst.emitTrunc(self, t),
            .ineg => |n| try emit_inst.emitIneg(self, n),
            .icmp => |c| try emit_inst.emitIcmp(self, c),

            // Logical
            .log_and => |l| try emit_inst.emitBinaryArith(self, .log_and, l.lhs, l.rhs, l.result),
            .log_or => |l| try emit_inst.emitBinaryArith(self, .log_or, l.lhs, l.rhs, l.result),
            .log_not => |l| try emit_inst.emitLogNot(self, l),

            // Null/optional operations
            .is_null => |n| try emit_inst.emitIsNull(self, n),
            .select => |s| try emit_inst.emitSelect(self, s),

            // Pointer operations
            .ptr_offset => |p| try emit_inst.emitPtrOffset(self, p),

            // String operations
            .str_concat => |s| try emit_inst.emitStrConcat(self, s),
            .str_slice => |s| try emit_inst.emitStrSlice(self, s),
            .str_slice_store => |s| try emit_inst.emitStrSliceStore(self, s),
            .str_compare, .str_copy => return EmitError.InvalidInstruction,

            // Control flow
            .jump => |b| try emit_inst.emitJump(self, b),
            .brif => |c| try emit_inst.emitBrif(self, c),
            .return_ => |r| try emit_inst.emitReturn(self, r),

            // Function calls
            .call => |c| try emit_inst.emitCall(self, c),
            .call_indirect => |c| try emit_inst.emitCallIndirect(self, c),

            // I/O operations
            .io_open => |o| try emit_inst.emitIoOpen(self, o),
            .io_close => |c| try emit_inst.emitIoClose(self, c),
            .io_read => |r| try emit_inst.emitIoRead(self, r),
            .io_write => |w| try emit_inst.emitIoWrite(self, w),
            .io_delete => |d| try emit_inst.emitIoDelete(self, d),

            // Struct buffer operations
            .load_struct_buf => |sb| try emit_inst.emitLoadStructBuf(self, sb),
            .store_struct_buf => |sb| try emit_inst.emitStoreStructBuf(self, sb),

            // Debug
            .debug_line => |d| try emit_inst.emitDebugLine(self, d),

            // Exception handling
            .try_begin => |t| try emit_inst.emitTryBegin(self, t),
            .try_end => try emit_inst.emitTryEnd(self),
            .catch_begin => |c| try emit_inst.emitCatchBegin(self, c),
            .throw => |t| try emit_inst.emitThrow(self, t),

            // Array operations
            .array_load => |al| try emit_inst.emitArrayLoad(self, al),
            .array_store => |as| try emit_inst.emitArrayStore(self, as),
            .array_len => |al| try emit_inst.emitArrayLen(self, al),

            // Switch/branch table
            .br_table => |s| try emit_inst.emitBrTable(self, s),

            // Decimal formatting
            .format_decimal => |fd| try emit_inst.emitFormatDecimal(self, fd),
            .parse_decimal => |pd| try emit_inst.emitParseDecimal(self, pd),

            // Map operations
            .map_new => |mn| try emit_inst.emitMapNew(self, mn),
            .map_set => |ms| try emit_inst.emitMapSet(self, ms),
            .map_get => |mg| try emit_inst.emitMapGet(self, mg),
            .map_delete => |md| try emit_inst.emitMapDelete(self, md),
            .map_has => |mh| try emit_inst.emitMapHas(self, mh),
            .map_len => |ml| try emit_inst.emitMapLen(self, ml),
            .map_clear => |mc| try emit_inst.emitMapClear(self, mc),
            .map_keys => |mk| try emit_inst.emitMapKeys(self, mk),
            .map_values => |mv| try emit_inst.emitMapValues(self, mv),
            .map_key_at => |mk| try emit_inst.emitMapKeyAt(self, mk),

            // List operations
            .list_new => |ln| try emit_inst.emitListNew(self, ln),
            .list_push => |lp| try emit_inst.emitListPush(self, lp),
            .list_pop => |lp| try emit_inst.emitListPop(self, lp),
            .list_get => |lg| try emit_inst.emitListGet(self, lg),
            .list_set => |ls| try emit_inst.emitListSet(self, ls),
            .list_len => |ll| try emit_inst.emitListLen(self, ll),
            .list_clear => |lc| try emit_inst.emitListClear(self, lc),

            // Weak reference operations
            .weak_ref => |w| try emit_inst.emitWeakRef(self, w),
            .weak_load => |w| try emit_inst.emitWeakLoad(self, w),

            // ARC operations
            .arc_retain => |a| try emit_inst.emitArcRetain(self, a),
            .arc_release => |a| try emit_inst.emitArcRelease(self, a),
            .arc_move => |a| try emit_inst.emitArcMove(self, a),

            // Closure operations
            .make_closure => |c| try emit_inst.emitMakeClosure(self, c),

            // Trait object operations
            .make_trait_object => |m| try emit_inst.emitMakeTraitObject(self, m),
            .call_trait_method => |c| try emit_inst.emitCallTraitMethod(self, c),

            else => {
                // Other instructions not yet implemented
            },
        }
    }

    /// Emit store_record_buf with type_idx and base slot
    pub fn emitStoreRecordBuf(self: *Self, struct_name: []const u8, base_name: ?[]const u8) EmitError!void {
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

        // Calculate base slot from first field - check both locals and globals
        var base: u16 = 0;
        var is_global: bool = false;
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
                    // Check locals first
                    if (self.locals.get(field_name)) |loc| {
                        base = loc.slot;
                        is_global = false;
                    } else if (self.globals.get(field_name)) |glob| {
                        // Check globals
                        base = glob.slot;
                        is_global = true;
                    } else if (base_name) |bn| {
                        // Try qualified name: base_name.field_name (for struct-typed fields)
                        var qualified_buf: [256]u8 = undefined;
                        const qualified_name = std.fmt.bufPrint(&qualified_buf, "{s}.{s}", .{ bn, field_name }) catch "";
                        if (self.locals.get(qualified_name)) |loc| {
                            base = loc.slot;
                            is_global = false;
                        } else if (self.globals.get(qualified_name)) |glob| {
                            base = glob.slot;
                            is_global = true;
                        }
                    }
                }
            }
        }

        // Format: [rs:4|flags:4] [type_idx:16] [base:16]
        // flags: bit 0 = is_global
        // Source is in r0 (set by emitStoreStructBuf or call_dynamic)
        const flags: u4 = if (is_global) 1 else 0;
        try self.emitOpcode(.store_record_buf);
        try self.emitU8((@as(u8, 0) << 4) | flags); // r0 in upper nibble, flags in lower
        try self.emitU16(type_idx);
        try self.emitU16(base);
    }

    /// Emit opcode byte
    pub fn emitOpcode(self: *Self, op: Opcode) EmitError!void {
        try self.code.append(self.allocator, @intFromEnum(op));
    }

    /// Emit unsigned 8-bit value
    pub fn emitU8(self: *Self, val: u8) EmitError!void {
        try self.code.append(self.allocator, val);
    }

    /// Emit unsigned 16-bit value (little-endian)
    pub fn emitU16(self: *Self, val: u16) EmitError!void {
        try self.code.appendSlice(self.allocator, &std.mem.toBytes(val));
    }

    /// Emit signed 16-bit value (little-endian)
    pub fn emitI16(self: *Self, val: i16) EmitError!void {
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
    pub fn emitRegArith(self: *Self, op: Opcode, dest: u4, src1: u4, src2: u4) EmitError!void {
        try self.emitOpcode(op);
        try self.emitU8((@as(u8, dest) << 4) | @as(u8, src1));
        try self.emitU8(@as(u8, src2) << 4);
    }

    /// Emit register unary: op dest, src (neg, not, etc.)
    pub fn emitRegUnary(self: *Self, op: Opcode, dest: u4, src: u4) EmitError!void {
        try self.emitOpcode(op);
        try self.emitU8((@as(u8, dest) << 4) | @as(u8, src));
        try self.emitU8(0);
    }

    /// Emit register immediate arithmetic: op dest, src, imm8
    pub fn emitRegArithImm(self: *Self, op: Opcode, dest: u4, src: u4, imm: i8) EmitError!void {
        try self.emitOpcode(op);
        try self.emitU8((@as(u8, dest) << 4) | @as(u8, src));
        try self.emitU8(@bitCast(imm));
    }

    /// Emit register move: mov dest, src
    pub fn emitRegMov(self: *Self, dest: u4, src: u4) EmitError!void {
        try self.emitOpcode(.mov);
        try self.emitU8((@as(u8, dest) << 4) | @as(u8, src));
        try self.emitU8(0);
    }

    /// Emit register move immediate: movi dest, imm8
    pub fn emitRegMovImm(self: *Self, dest: u4, imm: i8) EmitError!void {
        try self.emitOpcode(.movi);
        try self.emitU8(@as(u8, dest) << 4);
        try self.emitU8(@bitCast(imm));
    }

    /// Emit register move immediate 16: movi16 dest, imm16
    pub fn emitRegMovImm16(self: *Self, dest: u4, imm: i16) EmitError!void {
        try self.emitOpcode(.movi16);
        try self.emitU8(@as(u8, dest) << 4);
        try self.emitI16(imm);
    }

    /// Emit register load constant: load_const dest, const_idx
    pub fn emitRegLoadConst(self: *Self, dest: u4, const_idx: u16) EmitError!void {
        try self.emitOpcode(.load_const);
        try self.emitU8(@as(u8, dest) << 4);
        try self.emitU16(const_idx);
    }

    /// Emit register load null/true/false
    pub fn emitRegLoadLiteral(self: *Self, op: Opcode, dest: u4) EmitError!void {
        try self.emitOpcode(op);
        try self.emitU8(@as(u8, dest) << 4);
        try self.emitU8(0);
    }

    /// Emit register load local: load_local dest, slot
    pub fn emitRegLoadLocal(self: *Self, dest: u4, slot: u8) EmitError!void {
        debug.print(.emit, "emitRegLoadLocal: dest=r{d} slot={d} op_byte=0x{x:0>2}", .{ dest, slot, @as(u8, dest) << 4 });
        try self.emitOpcode(.load_local);
        try self.emitU8(@as(u8, dest) << 4);
        try self.emitU8(slot);
    }

    /// Emit register store local: store_local src, slot
    pub fn emitRegStoreLocal(self: *Self, src: u4, slot: u8) EmitError!void {
        try self.emitOpcode(.store_local);
        try self.emitU8(@as(u8, src) << 4);
        try self.emitU8(slot);
    }

    /// Emit register load global: load_global dest, global_idx
    pub fn emitRegLoadGlobal(self: *Self, dest: u4, idx: u16) EmitError!void {
        try self.emitOpcode(.load_global);
        try self.emitU8(@as(u8, dest) << 4);
        try self.emitU16(idx);
    }

    /// Emit register store global: store_global src, global_idx
    pub fn emitRegStoreGlobal(self: *Self, src: u4, idx: u16) EmitError!void {
        try self.emitOpcode(.store_global);
        try self.emitU8(@as(u8, src) << 4);
        try self.emitU16(idx);
    }

    /// Emit register comparison: cmp dest, src1, src2
    pub fn emitRegCmp(self: *Self, op: Opcode, dest: u4, src1: u4, src2: u4) EmitError!void {
        try self.emitOpcode(op);
        try self.emitU8((@as(u8, dest) << 4) | @as(u8, src1));
        try self.emitU8(@as(u8, src2) << 4);
    }

    /// Emit register jump: jmp offset
    pub fn emitRegJmp(self: *Self, target: *const ir.Block) EmitError!void {
        try self.emitOpcode(.jmp);
        try self.emitU8(0); // Unused first byte
        try self.addPendingJump(target, false);
        try self.emitI16(0); // Placeholder
    }

    /// Emit register conditional jump: jz/jnz cond, offset
    pub fn emitRegCondJmp(self: *Self, op: Opcode, cond: u4, target: *const ir.Block) EmitError!void {
        try self.emitOpcode(op);
        try self.emitU8(@as(u8, cond) << 4);
        try self.addPendingJump(target, false);
        try self.emitI16(0); // Placeholder
    }

    /// Emit register call: call argc, routine_idx
    pub fn emitRegCall(self: *Self, argc: u4, routine_idx: u16) EmitError!void {
        try self.emitOpcode(.call);
        try self.emitU8(@as(u8, argc) << 4);
        try self.emitU16(routine_idx);
    }

    /// Emit register return: ret or ret_val src
    pub fn emitRegRet(self: *Self, src: ?u4) EmitError!void {
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
    pub fn getOrAllocReg(self: *Self, value: ir.Value) EmitError!u4 {
        // Check if value already has a register
        if (self.reg_alloc.getRegister(value.id)) |reg| {
            return reg;
        }

        // Allocate a new register
        if (self.reg_alloc.allocate(value.id)) |reg| {
            return reg;
        }

        // Out of registers - need to spill (fall back to stack-based for now)
        debug.print(.emit, "ERROR: Out of registers for value.id={d}, free_regs=0x{x:0>4}", .{ value.id, self.reg_alloc.free_regs });
        self.setError(
            "Ran out of CPU registers during code generation",
            .{},
            "The expression is too complex. Try breaking it into smaller statements.",
            .{},
        );
        return EmitError.TooManyLocals;
    }

    /// Emit a value into a specific register (for register-based mode)
    pub fn emitValueToReg(self: *Self, value: ir.Value, dest: u4) EmitError!void {
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
                    self.setError(
                        "Local variable storage exceeds 256-slot limit (slot {d})",
                        .{slot_info},
                        "DBL record buffers with large alpha fields may exceed this limit. Consider using smaller field sizes or fewer variables.",
                        .{},
                    );
                    return EmitError.TooManyLocals;
                }
            }
        }
    }

    /// Get or allocate a register for a value, or load it into a temp register if it's a constant/slot
    /// This version prefers using existing registers or loading on demand without permanent allocation
    pub fn getValueInReg(self: *Self, value: ir.Value, temp_reg: u4) EmitError!u4 {
        debug.print(.emit, "getValueInReg: value.id={d} temp_reg={d}", .{ value.id, temp_reg });

        // Check if this is the last computed result (SSA chaining)
        if (self.last_result_value) |last_id| {
            if (last_id == value.id) {
                debug.print(.emit, "  -> last_result path: returning r{d}", .{self.last_result_reg});
                return self.last_result_reg;
            }
        }

        // If value is already in a register, use that
        if (self.reg_alloc.getRegister(value.id)) |src| {
            debug.print(.emit, "  -> reg_alloc path: returning r{d}", .{src});
            return src;
        }

        // Before using temp_reg, check if it holds a value that needs to be preserved
        // If so, spill it first
        if (self.reg_alloc.reg_to_value[temp_reg]) |existing_value| {
            // Spill the existing value if not already spilled
            if (self.spilled_values.get(existing_value) == null) {
                const spill_slot = self.allocateSpillSlot();
                debug.print(.emit, "SPILL (pre-load): value_id={d} from r{d} to slot {d}", .{ existing_value, temp_reg, spill_slot });
                try self.emitSpillStore(temp_reg, spill_slot);
                try self.spilled_values.put(existing_value, spill_slot);
            }
            // If this was the last_result, invalidate it so subsequent lookups find it in spilled_values
            if (self.last_result_value) |last_id| {
                if (last_id == existing_value) {
                    self.last_result_value = null;
                }
            }
            // Remove from register allocator
            _ = self.reg_alloc.value_to_reg.remove(existing_value);
            self.reg_alloc.reg_to_value[temp_reg] = null;
            self.reg_alloc.free_regs |= @as(u16, 1) << temp_reg;
        }

        // Check if value was spilled - reload it into temp_reg
        if (self.spilled_values.get(value.id)) |spill_slot| {
            debug.print(.emit, "RELOAD: value_id={d} from slot {d} to r{d}", .{ value.id, spill_slot, temp_reg });
            try self.emitSpillLoad(temp_reg, spill_slot);
            // Note: we don't remove from spilled_values - the value might be needed again
            // and we'd need to reload it. The spill slot is stable for this function.
            return temp_reg;
        }

        // Check if value is a constant - load into temp register
        if (self.value_consts.get(value.id)) |const_idx| {
            debug.print(.emit, "  -> const path: const_idx={d} -> r{d}", .{ const_idx, temp_reg });
            try self.emitRegLoadConst(temp_reg, const_idx);
            return temp_reg;
        }

        // Check if value is in a slot - load into temp register
        if (self.value_slots.get(value.id)) |slot_info| {
            if (slot_info & 0x8000 != 0) {
                // Global variable
                const slot = slot_info & 0x7FFF;
                debug.print(.emit, "  -> slot path (global): slot={d} -> r{d}", .{ slot, temp_reg });
                try self.emitRegLoadGlobal(temp_reg, slot);
            } else {
                // Local variable
                if (slot_info < 256) {
                    debug.print(.emit, "  -> slot path (local): slot={d} -> r{d}", .{ slot_info, temp_reg });
                    try self.emitRegLoadLocal(temp_reg, @intCast(slot_info));
                } else {
                    self.setError(
                        "Local variable storage exceeds 256-slot limit (slot {d})",
                        .{slot_info},
                        "DBL record buffers with large alpha fields may exceed this limit. Consider using smaller field sizes or fewer variables.",
                        .{},
                    );
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
    /// Also registers the value with the register allocator so it can be found later
    pub fn setLastResult(self: *Self, value_id: u32, reg: u4) void {
        self.last_result_value = value_id;
        self.last_result_reg = reg;
        // Also track in register allocator so value can be retrieved later
        // (even after last_result is overwritten by subsequent computations)
        self.reg_alloc.assignRegister(value_id, reg) catch {};
    }

    // ============================================
    // Register Spilling Support
    // ============================================

    /// Allocate a register for a value, spilling if necessary.
    /// This is the main entry point for register allocation that handles spilling.
    pub fn allocateWithSpill(self: *Self, value_id: u32) EmitError!u4 {
        // First try normal allocation
        if (self.reg_alloc.allocate(value_id)) |reg| {
            return reg;
        }

        // No free registers - spill one
        const spill_reg = self.selectSpillCandidate();
        const spilled_value = self.reg_alloc.reg_to_value[spill_reg] orelse {
            // No value in this register (shouldn't happen, but handle gracefully)
            debug.print(.emit, "WARNING: spill candidate r{d} has no value", .{spill_reg});
            return spill_reg;
        };

        // Allocate a spill slot and emit store_local to save the value
        const spill_slot = self.allocateSpillSlot();
        debug.print(.emit, "SPILL: value_id={d} from r{d} to slot {d}", .{ spilled_value, spill_reg, spill_slot });
        try self.emitSpillStore(spill_reg, spill_slot);

        // Track the spilled value
        try self.spilled_values.put(spilled_value, spill_slot);

        // Remove from register allocator
        _ = self.reg_alloc.value_to_reg.remove(spilled_value);
        self.reg_alloc.reg_to_value[spill_reg] = null;

        // Now allocate the freed register for the new value
        self.reg_alloc.reg_to_value[spill_reg] = value_id;
        try self.reg_alloc.value_to_reg.put(value_id, spill_reg);

        return spill_reg;
    }

    /// Select a register to spill.
    /// Strategy: prefer caller-saved registers (r2-r7), skip r0-r1 (used for operand loading)
    fn selectSpillCandidate(self: *Self) u4 {
        // Start from r2 (r0, r1 are temp registers for operand loading)
        var reg: u4 = 2;
        while (reg < 14) : (reg += 1) {
            if (self.reg_alloc.reg_to_value[reg] != null) {
                return reg;
            }
        }
        // Fallback to r2 if nothing found (shouldn't happen)
        return 2;
    }

    /// Allocate a new spill slot
    fn allocateSpillSlot(self: *Self) u16 {
        const slot = self.next_spill_slot;
        self.next_spill_slot += 1;
        const slots_used = self.next_spill_slot - self.spill_slot_base;
        if (slots_used > self.max_spill_slots_used) {
            self.max_spill_slots_used = slots_used;
        }
        return slot;
    }

    /// Emit store_local for spilling a register to a slot
    fn emitSpillStore(self: *Self, reg: u4, slot: u16) EmitError!void {
        if (slot < 256) {
            try self.emitOpcode(.store_local);
            try self.emitU8(@as(u8, reg) << 4);
            try self.emitU8(@intCast(slot));
        } else {
            try self.emitOpcode(.store_local16);
            try self.emitU8(@as(u8, reg) << 4);
            try self.emitU16(slot);
        }
    }

    /// Emit load_local for reloading a spilled value
    fn emitSpillLoad(self: *Self, reg: u4, slot: u16) EmitError!void {
        if (slot < 256) {
            try self.emitOpcode(.load_local);
            try self.emitU8(@as(u8, reg) << 4);
            try self.emitU8(@intCast(slot));
        } else {
            try self.emitOpcode(.load_local16);
            try self.emitU8(@as(u8, reg) << 4);
            try self.emitU16(slot);
        }
    }

    /// Reset spill state for a new function
    fn resetSpillState(self: *Self) void {
        self.spilled_values.clearRetainingCapacity();
        self.spill_slot_base = 0;
        self.next_spill_slot = 0;
        self.max_spill_slots_used = 0;
    }

    /// Setup spill slots for a function (call after counting locals)
    fn setupSpillSlots(self: *Self) void {
        // Reserve spill slots starting after declared locals
        self.spill_slot_base = self.local_count;
        self.next_spill_slot = self.local_count;
        // Reserve 16 slots for spilling (will extend local_count later if needed)
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
    pub fn addPendingJump(self: *Self, target_block: *const ir.Block, is_wide: bool) EmitError!void {
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
    pub fn addConstant(self: *Self, constant: module.Constant) EmitError!u16 {
        const idx = self.constants.items.len;
        if (idx > 65535) return EmitError.TooManyConstants;
        try self.constants.append(self.allocator, constant);
        return @intCast(idx);
    }

    /// Add string constant with deduplication
    pub fn addString(self: *Self, str: []const u8) EmitError!u16 {
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
    pub fn addIdentifier(self: *Self, name: []const u8) EmitError!u16 {
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
        .isize => .int64, // Pointer-sized signed
        .usize => .int64, // Pointer-sized unsigned
        .f32 => .fixed_point,
        .f64 => .fixed_point,
        .string => .string,
        .implied_decimal => .decimal,
        .fixed_decimal => .decimal, // Both decimal types map to bytecode decimal
        .ptr => .int64, // Pointers are 64-bit
        .optional => .int64, // Optionals are pointer-sized
        .array => .string, // Arrays treated as string for now
        .slice => .string, // Slices treated as string for now
        .@"struct" => .structure, // Structs are structures
        .@"union" => .structure, // Unions are similar to structs
        .function => .int64, // Function pointers
        .map => .int64, // Map handles are pointer-sized
        .list => .int64, // List handles are pointer-sized
        .weak => .int64, // Weak references are pointer-sized
        .trait_object => .int64, // Trait objects are pointer-sized (vtable + data)
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
