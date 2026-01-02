//! Cot IR Lowering
//!
//! Converts the NodeStore-based AST into the IR representation.
//! This is the bridge between parsing and code generation.
//!
//! The lowering process:
//! 1. Collects struct definitions and creates IR struct types
//! 2. Creates IR functions from function definitions
//! 3. Lowers statements to IR instructions
//! 4. Lowers expressions to IR values

const std = @import("std");
const ast = @import("../ast/mod.zig");
const ir = @import("ir.zig");
const struct_serial = @import("struct_serialization.zig");
const scope_stack = @import("scope_stack.zig");
const verify = @import("verify.zig");
const cot_runtime = @import("cot_runtime");
const debug = cot_runtime.debug;

// Scoped logging (Ghostty pattern) - enable with std_options or runtime filter
const log = std.log.scoped(.@"ir-lower");

// Import extracted types (following Ghostty pattern)
const lower_types = @import("lower_types.zig");
pub const LowerError = lower_types.LowerError;
pub const GenericDef = lower_types.GenericDef;
pub const InstantiationKey = lower_types.InstantiationKey;
pub const TraitMethodSig = lower_types.TraitMethodSig;
pub const TraitDef = lower_types.TraitDef;
pub const ImplKey = lower_types.ImplKey;
pub const MethodImpl = lower_types.MethodImpl;
const ImplKeyContext = lower_types.ImplKeyContext;

// Scope management
const ScopeStack = scope_stack.ScopeStack;

// Struct serialization helpers
const StructHelper = struct_serial.StructHelper;
const StructTypeInfo = struct_serial.StructTypeInfo;

// NodeStore types
const NodeStore = ast.NodeStore;
const StringInterner = ast.StringInterner;
const StringId = ast.StringId;
const StmtIdx = ast.StmtIdx;
const ExprIdx = ast.ExprIdx;
const TypeIdx = ast.TypeIdx;
const StatementTag = ast.StatementTag;
const ExpressionTag = ast.ExpressionTag;
const BinaryOp = ast.BinaryOp;
const UnaryOp = ast.UnaryOp;
const TypeTag = ast.TypeTag;
const NodeData = ast.NodeData;
const SourceLoc = ast.SourceLoc;

const Allocator = std.mem.Allocator;

/// Lowering context that tracks state during AST to IR conversion
pub const Lowerer = struct {
    allocator: Allocator,
    module: *ir.Module,
    current_func: ?*ir.Function,
    current_block: ?*ir.Block,

    /// NodeStore-based AST (not owned)
    store: *const NodeStore,

    /// String interner (not owned)
    strings: *const StringInterner,

    /// Scope stack for variable management
    /// Provides hierarchical scope lookup with push/pop for blocks
    scopes: ScopeStack,

    /// Map global variable names to their IR values (for module-level globals like DBL common blocks)
    global_variables: std.StringHashMap(ir.Value),

    /// Map struct names to their IR types
    struct_types: std.StringHashMap(*const ir.StructType),

    /// Map union names to their IR types
    union_types: std.StringHashMap(*const ir.UnionType),

    /// Generic struct definitions (templates for instantiation)
    generic_struct_defs: std.StringHashMap(GenericDef),

    /// Current type parameter substitutions (for generic instantiation)
    type_param_substitutions: std.StringHashMap(ir.Type),

    /// Cache of instantiated generic structs
    instantiated_structs: std.StringHashMap(*const ir.StructType),

    /// Allocated Type pointers that need to be freed on deinit
    allocated_types: std.ArrayList(*ir.Type),

    /// Current loop's exit block (for break statements)
    loop_exit_block: ?*ir.Block,

    /// Current loop's continue block (for continue statements)
    loop_continue_block: ?*ir.Block,

    /// Warnings collected during lowering
    warnings: std.ArrayList(Warning),

    /// Context of the last error that occurred (for better error messages)
    last_error: ?ErrorContext,

    /// Counter for generating unique lambda names
    lambda_counter: u32,

    /// Function parameter defaults (function name -> list of default ExprIdx per param)
    /// If ExprIdx is null (0), the param is required
    fn_param_defaults: std.StringHashMap([]const u32),

    /// Trait definitions (trait name -> TraitDef)
    trait_defs: std.StringHashMap(TraitDef),

    /// Trait implementations (ImplKey -> method implementations)
    impl_methods: ImplMethodsMap,

    const ImplMethodsMap = std.HashMap(ImplKey, []const MethodImpl, ImplKeyContext, std.hash_map.default_max_load_percentage);

    /// Known builtin function names (lowercase)
    const known_builtins = std.StaticStringMap(void).initComptime(.{
        .{ "trim", {} },
        .{ "len", {} },
        .{ "string", {} },
        .{ "integer", {} },
        .{ "decimal", {} },
        .{ "size", {} },
        .{ "instr", {} },
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
        .{ "date", {} },
        .{ "time", {} },
        .{ "mem", {} },
        .{ "error", {} },
        .{ "chr", {} },
        .{ "asc", {} },
        .{ "upper", {} },
        .{ "lower", {} },
        .{ "atrim", {} },
        .{ "print", {} },
        .{ "println", {} },
    });

    pub const Warning = struct {
        message: []const u8,
        line: u32,
        column: u32,
    };

    /// Context for the last error that occurred during lowering
    pub const ErrorContext = struct {
        kind: LowerError,
        message: []const u8,
        line: u32,
        column: u32,
        context: []const u8, // What we were trying to do (e.g., "lowering assignment to 'x'")

        pub fn format(self: ErrorContext, writer: anytype) !void {
            try writer.print("{d}:{d}: error: {s}\n", .{ self.line, self.column, self.message });
            if (self.context.len > 0) {
                try writer.print("  while {s}\n", .{self.context});
            }
        }
    };

    const Self = @This();

    /// Check if a type is a map type (directly or through a pointer)
    fn isMapType(ty: ir.Type) bool {
        return switch (ty) {
            .map => true,
            .ptr => |p| p.* == .map,
            else => false,
        };
    }

    /// Convert AST SourceLoc to IR SourceLoc
    fn astLocToIrLoc(loc: SourceLoc) ?ir.SourceLoc {
        if (loc.line == 0 and loc.column == 0) {
            return null;
        }
        return .{ .line = loc.line, .column = loc.column };
    }

    pub fn init(allocator: Allocator, store: *const NodeStore, strings: *const StringInterner, module_name: []const u8) !Self {
        const module = try allocator.create(ir.Module);
        module.* = ir.Module.init(allocator, module_name);

        return .{
            .allocator = allocator,
            .module = module,
            .current_func = null,
            .current_block = null,
            .store = store,
            .strings = strings,
            .scopes = try ScopeStack.init(allocator),
            .global_variables = std.StringHashMap(ir.Value).init(allocator),
            .struct_types = std.StringHashMap(*const ir.StructType).init(allocator),
            .union_types = std.StringHashMap(*const ir.UnionType).init(allocator),
            .generic_struct_defs = std.StringHashMap(GenericDef).init(allocator),
            .type_param_substitutions = std.StringHashMap(ir.Type).init(allocator),
            .instantiated_structs = std.StringHashMap(*const ir.StructType).init(allocator),
            .allocated_types = .{},
            .loop_exit_block = null,
            .loop_continue_block = null,
            .warnings = .{},
            .last_error = null,
            .lambda_counter = 0,
            .fn_param_defaults = std.StringHashMap([]const u32).init(allocator),
            .trait_defs = std.StringHashMap(TraitDef).init(allocator),
            .impl_methods = ImplMethodsMap.init(allocator),
        };
    }

    /// Check if a function name is a known builtin
    fn isKnownBuiltin(name: []const u8) bool {
        return known_builtins.has(name);
    }

    /// Add a warning
    fn addWarning(self: *Self, comptime fmt: []const u8, args: anytype, line: u32, column: u32) void {
        const message = std.fmt.allocPrint(self.allocator, fmt, args) catch return;
        self.warnings.append(self.allocator, .{
            .message = message,
            .line = line,
            .column = column,
        }) catch {};
    }

    /// Get collected warnings
    pub fn getWarnings(self: *Self) []const Warning {
        return self.warnings.items;
    }

    /// Set error context when an error occurs (for better error messages)
    fn setErrorContext(
        self: *Self,
        err: LowerError,
        comptime message_fmt: []const u8,
        message_args: anytype,
        loc: SourceLoc,
        comptime context_fmt: []const u8,
        context_args: anytype,
    ) void {
        const message = std.fmt.allocPrint(self.allocator, message_fmt, message_args) catch "allocation failed";
        const context = std.fmt.allocPrint(self.allocator, context_fmt, context_args) catch "";
        self.last_error = .{
            .kind = err,
            .message = message,
            .line = loc.line,
            .column = loc.column,
            .context = context,
        };
    }

    /// Get the last error context (if any)
    pub fn getLastError(self: *const Self) ?ErrorContext {
        return self.last_error;
    }

    pub fn deinit(self: *Self) void {
        self.scopes.deinit();
        self.global_variables.deinit();
        self.struct_types.deinit();
        self.union_types.deinit();
        self.generic_struct_defs.deinit();
        self.type_param_substitutions.deinit();
        self.instantiated_structs.deinit();
        self.trait_defs.deinit();
        self.impl_methods.deinit();

        // Free function param defaults slices
        var defaults_iter = self.fn_param_defaults.valueIterator();
        while (defaults_iter.next()) |defaults| {
            self.allocator.free(defaults.*);
        }
        self.fn_param_defaults.deinit();

        // NOTE: allocated_types ownership was transferred to the Module in lowerProgram.
        // No cleanup needed here.

        for (self.warnings.items) |w| {
            self.allocator.free(w.message);
        }
        self.warnings.deinit(self.allocator);
    }

    /// Emit an instruction to the current block
    fn emit(self: *Self, inst: ir.Instruction) LowerError!void {
        const block = self.current_block orelse return LowerError.OutOfMemory;
        block.instructions.append(self.allocator, inst) catch return LowerError.OutOfMemory;
    }

    /// Lower a program (list of top-level statements)
    pub fn lowerProgram(self: *Self, top_level: []const StmtIdx) LowerError!*ir.Module {
        log.debug("Lowering program with {d} top-level statements", .{top_level.len});
        debug.print(.ir, "Lowering program with {d} top-level statements", .{top_level.len});

        // First pass: collect struct and union definitions
        for (top_level) |stmt_idx| {
            const tag = self.store.stmtTag(stmt_idx);
            if (tag == .struct_def) {
                try self.lowerStructDef(stmt_idx);
            } else if (tag == .union_def) {
                try self.lowerUnionDef(stmt_idx);
            }
        }

        // Second pass: collect trait definitions
        for (top_level) |stmt_idx| {
            const tag = self.store.stmtTag(stmt_idx);
            if (tag == .trait_def) {
                try self.lowerTraitDef(stmt_idx);
            }
        }

        // Third pass: collect impl blocks
        for (top_level) |stmt_idx| {
            const tag = self.store.stmtTag(stmt_idx);
            if (tag == .impl_block) {
                try self.lowerImplBlock(stmt_idx);
            }
        }

        // Fourth pass: register top-level let declarations as globals
        // (These come from DBL common blocks and need to be visible in all functions)
        // Must run BEFORE function lowering so functions can reference globals
        for (top_level) |stmt_idx| {
            const tag = self.store.stmtTag(stmt_idx);
            if (tag == .let_decl) {
                try self.lowerGlobalLetDecl(stmt_idx);
            }
        }

        // Fifth pass: collect function SIGNATURES (params/defaults only, no body)
        // This must happen before body lowering so call sites can use defaults
        for (top_level) |stmt_idx| {
            const tag = self.store.stmtTag(stmt_idx);
            if (tag == .fn_def) {
                try self.collectFnSignature(stmt_idx);
            }
        }

        // Sixth pass: lower function BODIES
        for (top_level) |stmt_idx| {
            const tag = self.store.stmtTag(stmt_idx);
            if (tag == .fn_def) {
                try self.lowerFnDef(stmt_idx);
            }
        }

        // Transfer ownership of allocated types to the module
        // The module will free them in its deinit
        // Convert managed ArrayList to unmanaged by taking the internal slice
        self.module.allocated_types = .{
            .items = self.allocated_types.items,
            .capacity = self.allocated_types.capacity,
        };
        // Clear the Lowerer's list to prevent double-free
        self.allocated_types = .{};

        return self.module;
    }

    /// Lower a struct definition
    fn lowerStructDef(self: *Self, stmt_idx: StmtIdx) LowerError!void {
        const data = self.store.stmtData(stmt_idx);
        const name_id = data.getName();
        const name = self.strings.get(name_id);
        if (name.len == 0) return LowerError.UndefinedType;

        log.debug("Lowering struct: {s}", .{name});
        debug.print(.ir, "Lowering struct: {s}", .{name});

        // Get fields and type params from extra_data
        // Layout: [field_count, type_param_count, type_params_start, ...fields...]
        const extra_start = data.getParamsStart();
        const field_count = self.store.getExtra(extra_start);
        const type_param_count = self.store.extra_data.items[extra_start.toInt() + 1];
        const type_params_start = self.store.extra_data.items[extra_start.toInt() + 2];

        // If this is a generic struct, store it as a template instead of lowering
        if (type_param_count > 0) {
            debug.print(.ir, "  -> Generic struct with {d} type params, storing as template", .{type_param_count});

            // Extract type parameter names
            var type_param_names: std.ArrayListUnmanaged(StringId) = .{};
            defer type_param_names.deinit(self.allocator);

            for (0..type_param_count) |i| {
                const param_name_id: StringId = @enumFromInt(self.store.extra_data.items[type_params_start + i]);
                try type_param_names.append(self.allocator, param_name_id);
            }

            const generic_def = GenericDef{
                .stmt_idx = stmt_idx,
                .type_param_count = @intCast(type_param_count),
                .type_param_names = try type_param_names.toOwnedSlice(self.allocator),
            };

            try self.generic_struct_defs.put(name, generic_def);
            return;
        }

        // Non-generic struct - lower immediately
        var fields: std.ArrayListUnmanaged(ir.StructType.Field) = .{};
        defer fields.deinit(self.allocator);

        // Fields start after [field_count, type_param_count, type_params_start]
        var extra_idx = extra_start.toInt() + 3;
        var current_offset: u32 = 0;
        for (0..field_count) |_| {
            const field_name_id: StringId = @enumFromInt(self.store.extra_data.items[extra_idx]);
            const field_type_idx: TypeIdx = @enumFromInt(self.store.extra_data.items[extra_idx + 1]);
            extra_idx += 2;

            const field_name = self.strings.get(field_name_id);
            if (field_name.len == 0) continue;
            const field_type = try self.lowerTypeIdx(field_type_idx);
            const field_size = field_type.sizeInBytes();

            try fields.append(self.allocator, .{
                .name = field_name,
                .ty = field_type,
                .offset = current_offset,
            });
            current_offset += field_size;
            debug.print(.ir, "  field '{s}': offset={d} size={d}", .{ field_name, current_offset - field_size, field_size });
        }

        const struct_type = try self.allocator.create(ir.StructType);
        struct_type.* = .{
            .name = name,
            .fields = try fields.toOwnedSlice(self.allocator),
            .size = current_offset, // Total size of all fields
            .alignment = 8,
        };
        debug.print(.ir, "  struct '{s}' total size: {d}", .{ name, current_offset });

        try self.module.addStruct(struct_type);
        try self.struct_types.put(name, struct_type);
    }

    /// Lower a union definition
    fn lowerUnionDef(self: *Self, stmt_idx: StmtIdx) LowerError!void {
        const data = self.store.stmtData(stmt_idx);
        const name_id: StringId = @enumFromInt(data.a);
        const name = self.strings.get(name_id);
        if (name.len == 0) return LowerError.UndefinedType;

        log.debug("Lowering union: {s}", .{name});
        debug.print(.ir, "Lowering union: {s}", .{name});

        // Get variants from extra_data
        // Layout: [variant_count, [name, type]...]
        const extra_start = data.b;
        const variant_count = self.store.extra_data.items[extra_start];

        var variants: std.ArrayListUnmanaged(ir.UnionType.Variant) = .{};
        defer variants.deinit(self.allocator);

        var max_size: u32 = 0;
        const max_alignment: u32 = 8;
        var extra_idx = extra_start + 1;
        for (0..variant_count) |_| {
            const variant_name_id: StringId = @enumFromInt(self.store.extra_data.items[extra_idx]);
            const variant_type_idx: TypeIdx = @enumFromInt(self.store.extra_data.items[extra_idx + 1]);
            extra_idx += 2;

            const variant_name = self.strings.get(variant_name_id);
            if (variant_name.len == 0) continue;
            const variant_type = try self.lowerTypeIdx(variant_type_idx);
            const variant_size = variant_type.sizeInBytes();

            try variants.append(self.allocator, .{
                .name = variant_name,
                .ty = variant_type,
            });

            // Union size is max of all variant sizes
            if (variant_size > max_size) {
                max_size = variant_size;
            }
            debug.print(.ir, "  variant '{s}': size={d}", .{ variant_name, variant_size });
        }

        const union_type = try self.allocator.create(ir.UnionType);
        union_type.* = .{
            .name = name,
            .variants = try variants.toOwnedSlice(self.allocator),
            .size = max_size,
            .alignment = max_alignment,
        };
        debug.print(.ir, "  union '{s}' total size: {d}", .{ name, max_size });

        try self.module.addUnion(union_type);
        try self.union_types.put(name, union_type);
    }

    /// Instantiate a generic struct with concrete type arguments
    fn instantiateGenericStruct(self: *Self, base_name: []const u8, type_args: []const ir.Type) LowerError!?*const ir.StructType {
        // Check if we have a generic definition for this name
        const generic_def = self.generic_struct_defs.get(base_name) orelse return null;

        // Verify argument count
        if (type_args.len != generic_def.type_param_count) {
            return null;
        }

        // Generate a unique name for this instantiation
        var mangled_name_buf: [256]u8 = undefined;
        var stream = std.io.fixedBufferStream(&mangled_name_buf);
        const writer = stream.writer();
        writer.writeAll(base_name) catch return null;
        writer.writeAll("<") catch return null;
        for (type_args, 0..) |arg, i| {
            if (i > 0) writer.writeAll(", ") catch return null;
            // Write type name
            switch (arg) {
                .i8 => writer.writeAll("i8") catch return null,
                .i16 => writer.writeAll("i16") catch return null,
                .i32 => writer.writeAll("i32") catch return null,
                .i64 => writer.writeAll("i64") catch return null,
                .u8 => writer.writeAll("u8") catch return null,
                .u16 => writer.writeAll("u16") catch return null,
                .u32 => writer.writeAll("u32") catch return null,
                .u64 => writer.writeAll("u64") catch return null,
                .f32 => writer.writeAll("f32") catch return null,
                .f64 => writer.writeAll("f64") catch return null,
                .bool => writer.writeAll("bool") catch return null,
                .string => writer.writeAll("string") catch return null,
                .void => writer.writeAll("void") catch return null,
                else => writer.writeAll("?") catch return null,
            }
        }
        writer.writeAll(">") catch return null;

        const mangled_name = stream.getWritten();

        // Check if we already instantiated this
        if (self.instantiated_structs.get(mangled_name)) |existing| {
            return existing;
        }

        debug.print(.ir, "Instantiating generic struct: {s}", .{mangled_name});

        // Set up type parameter substitutions
        self.type_param_substitutions.clearRetainingCapacity();
        for (generic_def.type_param_names, type_args) |param_name_id, arg_type| {
            const param_name = self.strings.get(param_name_id);
            self.type_param_substitutions.put(param_name, arg_type) catch return null;
        }

        // Re-lower the struct fields with substitutions
        const data = self.store.stmtData(generic_def.stmt_idx);
        const extra_start = data.getParamsStart();
        const field_count = self.store.getExtra(extra_start);

        var fields: std.ArrayListUnmanaged(ir.StructType.Field) = .{};
        defer fields.deinit(self.allocator);

        // Fields start after [field_count, type_param_count, type_params_start]
        var extra_idx = extra_start.toInt() + 3;
        var current_offset: u32 = 0;
        for (0..field_count) |_| {
            const field_name_id: StringId = @enumFromInt(self.store.extra_data.items[extra_idx]);
            const field_type_idx: TypeIdx = @enumFromInt(self.store.extra_data.items[extra_idx + 1]);
            extra_idx += 2;

            const field_name = self.strings.get(field_name_id);
            if (field_name.len == 0) continue;

            // Lower type with substitutions active
            const field_type = try self.lowerTypeIdx(field_type_idx);
            const field_size = field_type.sizeInBytes();

            try fields.append(self.allocator, .{
                .name = field_name,
                .ty = field_type,
                .offset = current_offset,
            });
            current_offset += field_size;
        }

        // Clear substitutions
        self.type_param_substitutions.clearRetainingCapacity();

        // Create the instantiated struct
        // Need to dupe the mangled name since it's in a stack buffer
        const name_copy = self.allocator.dupe(u8, mangled_name) catch return null;

        const struct_type = try self.allocator.create(ir.StructType);
        struct_type.* = .{
            .name = name_copy,
            .fields = try fields.toOwnedSlice(self.allocator),
            .size = current_offset, // Total size of all fields
            .alignment = 8,
        };

        try self.module.addStruct(struct_type);
        try self.instantiated_structs.put(name_copy, struct_type);
        try self.struct_types.put(name_copy, struct_type);

        return struct_type;
    }

    /// Lower a trait definition - stores trait method signatures for later lookup
    fn lowerTraitDef(self: *Self, stmt_idx: StmtIdx) LowerError!void {
        const data = self.store.stmtData(stmt_idx);
        const name_id: StringId = @enumFromInt(data.a);
        const name = self.strings.get(name_id);
        const methods_start = data.b;

        debug.print(.ir, "Lowering trait: {s}", .{name});

        // Parse extra_data: [method_count, type_param_count, type_params_start, ...method_data...]
        const method_count = self.store.extra_data.items[methods_start];
        const type_param_count: u16 = @intCast(self.store.extra_data.items[methods_start + 1]);
        // const type_params_start = self.store.extra_data.items[methods_start + 2]; // unused for now

        // Parse method signatures
        // Layout: [method_name, param_count, return_type, param_pairs...] for each method
        var methods: std.ArrayListUnmanaged(TraitMethodSig) = .{};
        defer methods.deinit(self.allocator);

        var offset: u32 = 3; // Skip method_count, type_param_count, type_params_start
        for (0..method_count) |_| {
            // Read method header: [method_name, param_count, return_type]
            const method_name_id: StringId = @enumFromInt(self.store.extra_data.items[methods_start + offset]);
            const method_name = self.strings.get(method_name_id);
            offset += 1;

            const param_count = self.store.extra_data.items[methods_start + offset];
            offset += 1;

            const return_type_idx: TypeIdx = @enumFromInt(self.store.extra_data.items[methods_start + offset]);
            offset += 1;

            // Skip param data (param_count * 2 entries for name + type)
            offset += param_count * 2;

            const return_type = if (return_type_idx != .null)
                self.lowerTypeIdx(return_type_idx) catch .void
            else
                .void;

            try methods.append(self.allocator, .{
                .name = method_name,
                .param_count = param_count,
                .return_type = return_type,
            });

            debug.print(.ir, "  Method: {s}({d} params) -> {}", .{ method_name, param_count, return_type });
        }

        // Store the trait definition
        try self.trait_defs.put(name, .{
            .name = name,
            .type_param_count = type_param_count,
            .methods = try methods.toOwnedSlice(self.allocator),
        });
    }

    /// Lower an impl block - registers method implementations for a trait+type pair
    fn lowerImplBlock(self: *Self, stmt_idx: StmtIdx) LowerError!void {
        const data = self.store.stmtData(stmt_idx);
        const methods_start = data.b;

        // Parse extra_data: [method_count, trait_type, target_type, method_stmt_idx...]
        const method_count = self.store.extra_data.items[methods_start];
        const trait_type_idx: TypeIdx = @enumFromInt(self.store.extra_data.items[methods_start + 1]);
        const target_type_idx: TypeIdx = @enumFromInt(self.store.extra_data.items[methods_start + 2]);

        // Get trait and target type names
        const trait_name = self.getTypeName(trait_type_idx) orelse {
            debug.print(.ir, "Warning: impl block has invalid trait type", .{});
            return;
        };

        // target_type is optional (for inherent impls, it's null)
        const target_name = if (target_type_idx != .null)
            self.getTypeName(target_type_idx) orelse trait_name // If no target, use trait as type
        else
            trait_name;

        debug.print(.ir, "Lowering impl {s} for {s} with {d} methods", .{ trait_name, target_name, method_count });

        // Collect method implementations
        var methods: std.ArrayListUnmanaged(MethodImpl) = .{};
        defer methods.deinit(self.allocator);

        for (0..method_count) |i| {
            const method_stmt_raw = self.store.extra_data.items[methods_start + 3 + i];
            const method_stmt_idx: StmtIdx = @enumFromInt(method_stmt_raw);

            // Get the method name from the fn_def statement
            const fn_data = self.store.stmtData(method_stmt_idx);
            const fn_name_id = fn_data.getName();
            const fn_name = self.strings.get(fn_name_id);

            debug.print(.ir, "  Impl method: {s}", .{fn_name});

            try methods.append(self.allocator, .{
                .method_name = fn_name,
                .fn_stmt_idx = method_stmt_idx,
            });

            // Also lower the method as a regular function (for now - later we may want mangled names)
            try self.lowerFnDef(method_stmt_idx);
        }

        // Store the implementation mapping
        const key = ImplKey{
            .trait_name = trait_name,
            .type_name = target_name,
        };
        try self.impl_methods.put(key, try methods.toOwnedSlice(self.allocator));
    }

    /// Get the name of a type from its TypeIdx
    fn getTypeName(self: *Self, type_idx: TypeIdx) ?[]const u8 {
        if (type_idx == .null) return null;

        const tag = self.store.typeTag(type_idx);
        switch (tag) {
            .named => {
                const type_data = self.store.typeData(type_idx);
                const name_id: StringId = @enumFromInt(type_data.a);
                return self.strings.get(name_id);
            },
            else => return null,
        }
    }

    /// Look up a trait method implementation for a specific type
    pub fn lookupTraitMethod(self: *Self, trait_name: []const u8, type_name: []const u8, method_name: []const u8) ?StmtIdx {
        const key = ImplKey{
            .trait_name = trait_name,
            .type_name = type_name,
        };

        if (self.impl_methods.get(key)) |methods| {
            for (methods) |m| {
                if (std.mem.eql(u8, m.method_name, method_name)) {
                    return m.fn_stmt_idx;
                }
            }
        }
        return null;
    }

    /// Collect function signature (params and defaults only, no body lowering)
    /// This runs in an earlier pass so defaults are available at call sites
    fn collectFnSignature(self: *Self, stmt_idx: StmtIdx) LowerError!void {
        const data = self.store.stmtData(stmt_idx);
        const name_id = data.getName();
        const name = self.strings.get(name_id);
        if (name.len == 0) return LowerError.UndefinedVariable;

        // Parse extra data: [param_count, param1_name, param1_type, param1_is_ref, param1_default, ..., return_type, body]
        const extra_start = data.getParamsStart();
        const param_count = self.store.getExtra(extra_start);

        // Collect default expressions for each parameter
        var param_defaults: std.ArrayListUnmanaged(u32) = .{};
        defer param_defaults.deinit(self.allocator);

        var extra_idx = extra_start.toInt() + 1;
        for (0..param_count) |_| {
            // Skip name, type, is_ref; just collect default
            const default_expr_raw: u32 = self.store.extra_data.items[extra_idx + 3];
            extra_idx += 4; // 4 values per param

            param_defaults.append(self.allocator, default_expr_raw) catch return LowerError.OutOfMemory;
        }

        // Store param defaults for this function (for call-site defaulting)
        if (param_count > 0) {
            const defaults_slice = param_defaults.toOwnedSlice(self.allocator) catch return LowerError.OutOfMemory;
            self.fn_param_defaults.put(name, defaults_slice) catch return LowerError.OutOfMemory;
        }
    }

    /// Check if a type implements a trait
    pub fn implementsTrait(self: *Self, type_name: []const u8, trait_name: []const u8) bool {
        const key = ImplKey{
            .trait_name = trait_name,
            .type_name = type_name,
        };
        return self.impl_methods.contains(key);
    }

    /// Lower a function definition
    fn lowerFnDef(self: *Self, stmt_idx: StmtIdx) LowerError!void {
        const data = self.store.stmtData(stmt_idx);
        const name_id = data.getName();
        const name = self.strings.get(name_id);
        if (name.len == 0) return LowerError.UndefinedVariable;

        debug.print(.ir, "Lowering function: {s}", .{name});

        // Parse extra data: [param_count, param1_name, param1_type, ..., return_type, body]
        const extra_start = data.getParamsStart();
        const param_count = self.store.getExtra(extra_start);

        var params: std.ArrayListUnmanaged(ir.FunctionType.Param) = .{};
        defer params.deinit(self.allocator);

        var extra_idx = extra_start.toInt() + 1;
        for (0..param_count) |_| {
            const param_name_id: StringId = @enumFromInt(self.store.extra_data.items[extra_idx]);
            const param_type_idx: TypeIdx = @enumFromInt(self.store.extra_data.items[extra_idx + 1]);
            const param_direction_raw: u32 = self.store.extra_data.items[extra_idx + 2];
            // default_expr at [extra_idx + 3] already collected in collectFnSignature
            extra_idx += 4; // 4 values per param: name, type, is_ref, default_value

            const param_name = self.strings.get(param_name_id);
            if (param_name.len == 0) continue;
            const param_type = try self.lowerTypeIdx(param_type_idx);

            // is_ref: 0 = by value, non-zero = by reference
            const is_ref = param_direction_raw != 0;

            try params.append(self.allocator, .{
                .name = param_name,
                .ty = param_type,
                .is_ref = is_ref,
            });
        }

        const return_type_idx: TypeIdx = @enumFromInt(self.store.extra_data.items[extra_idx]);
        const body_idx: StmtIdx = @enumFromInt(self.store.extra_data.items[extra_idx + 1]);

        const return_type = try self.lowerTypeIdx(return_type_idx);

        // Create function type (stack variable - copied into Function.signature)
        const func_type = ir.FunctionType{
            .params = try params.toOwnedSlice(self.allocator),
            .return_type = return_type,
            .is_variadic = false,
        };

        // Create function
        const func = try ir.Function.init(self.allocator, name, func_type);

        self.current_func = func;

        // Use the entry block created by Function.init
        self.current_block = func.entry;

        // Clear variables for new function scope
        self.scopes.reset();

        // Add parameters as local variables
        for (func_type.params) |param| {
            const ty_ptr = try self.allocator.create(ir.Type);
            ty_ptr.* = param.ty;
            try self.allocated_types.append(self.allocator, ty_ptr);

            const alloca_result = func.newValue(.{ .ptr = ty_ptr });
            try self.emit(.{
                .alloca = .{
                    .ty = param.ty,
                    .name = param.name,
                    .result = alloca_result,
                },
            });
            try self.scopes.put(param.name, alloca_result);
        }

        // Lower function body
        try self.lowerStatement(body_idx);

        // Add implicit return if not terminated
        if (!self.current_block.?.isTerminated()) {
            try self.emit(.{ .return_ = null });
        }

        try self.module.addFunction(func);
    }

    /// Emit a debug_line instruction if location is valid
    fn emitDebugLine(self: *Self, loc: SourceLoc) LowerError!void {
        if (loc.line > 0) {
            try self.emit(.{ .debug_line = .{ .line = loc.line, .column = loc.column } });
        }
    }

    /// Lower a single statement to IR
    fn lowerStatement(self: *Self, stmt_idx: StmtIdx) LowerError!void {
        const tag = self.store.stmtTag(stmt_idx);
        const loc = self.store.stmtLoc(stmt_idx);
        const data = self.store.stmtData(stmt_idx);

        log.debug("lowerStatement: {s} at line {d}", .{ @tagName(tag), loc.line });

        switch (tag) {
            .assignment => {
                try self.emitDebugLine(loc);
                try self.lowerAssignment(data);
            },
            .if_stmt => {
                try self.emitDebugLine(loc);
                try self.lowerIf(stmt_idx, data);
            },
            .return_stmt => {
                try self.emitDebugLine(loc);
                try self.lowerReturn(data);
            },
            .block => try self.lowerBlock(data),
            .while_stmt => {
                try self.emitDebugLine(loc);
                try self.lowerWhile(data);
            },
            .for_stmt => {
                try self.emitDebugLine(loc);
                try self.lowerFor(stmt_idx, data);
            },
            .loop_stmt => {
                try self.emitDebugLine(loc);
                try self.lowerLoop(data);
            },
            .break_stmt => {
                try self.emitDebugLine(loc);
                try self.lowerBreak();
            },
            .continue_stmt => {
                try self.emitDebugLine(loc);
                try self.lowerContinue();
            },
            .expression => {
                try self.emitDebugLine(loc);
                try self.lowerExpressionStmt(data);
            },
            .let_decl => {
                try self.emitDebugLine(loc);
                try self.lowerLetDecl(data);
            },
            .const_decl => {
                // Compile-time constants are handled by the comptime evaluator.
                // They don't generate runtime code.
            },
            .io_open => {
                try self.emitDebugLine(loc);
                try self.lowerIoOpen(stmt_idx);
            },
            .io_close => {
                try self.emitDebugLine(loc);
                try self.lowerIoClose(stmt_idx);
            },
            .io_read => {
                try self.emitDebugLine(loc);
                try self.lowerIoRead(stmt_idx);
            },
            .io_write => {
                try self.emitDebugLine(loc);
                try self.lowerIoWrite(stmt_idx);
            },
            .io_store => {
                try self.emitDebugLine(loc);
                try self.lowerIoStore(stmt_idx);
            },
            .io_delete => {
                try self.emitDebugLine(loc);
                try self.lowerIoDelete(stmt_idx);
            },
            .try_stmt => {
                try self.emitDebugLine(loc);
                try self.lowerTry(stmt_idx);
            },
            .throw_stmt => {
                try self.emitDebugLine(loc);
                try self.lowerThrow();
            },
            .match_stmt => {
                try self.emitDebugLine(loc);
                try self.lowerMatch(stmt_idx);
            },
            .field_view => {
                try self.emitDebugLine(loc);
                try self.lowerFieldView(stmt_idx);
            },
            .import_stmt, .fn_def, .struct_def, .union_def, .enum_def, .type_alias, .trait_def, .impl_block => {
                // Handled in earlier passes or no runtime code
            },
            .comptime_if, .comptime_block => {
                // Should be resolved before IR lowering
                return LowerError.UnsupportedFeature;
            },
        }
    }

    // ============================================================
    // Statement Handlers
    // ============================================================

    /// Lower a block statement (sequence of statements)
    fn lowerBlock(self: *Self, data: NodeData) LowerError!void {
        const span = data.getSpan();
        const stmt_indices = self.store.getStmtSpan(span);
        for (stmt_indices) |idx| {
            try self.lowerStatement(@enumFromInt(idx));
        }
    }

    /// Lower a break statement
    fn lowerBreak(self: *Self) LowerError!void {
        if (self.loop_exit_block) |exit_block| {
            try self.emit(.{ .jump = .{ .target = exit_block } });
        } else {
            return LowerError.UnsupportedFeature;
        }
    }

    /// Lower a continue statement
    fn lowerContinue(self: *Self) LowerError!void {
        if (self.loop_continue_block) |continue_block| {
            try self.emit(.{ .jump = .{ .target = continue_block } });
        } else {
            return LowerError.UnsupportedFeature;
        }
    }

    /// Lower an expression statement (evaluate for side effects)
    fn lowerExpressionStmt(self: *Self, data: NodeData) LowerError!void {
        const expr_idx = data.getExpr();
        _ = try self.lowerExpression(expr_idx);
    }

    /// Lower a throw statement
    fn lowerThrow(self: *Self) LowerError!void {
        // Throw generates a return for now (proper exceptions TBD)
        try self.emit(.{ .return_ = null });
    }

    /// Lower an assignment statement
    fn lowerAssignment(self: *Self, data: NodeData) LowerError!void {
        const target_idx = data.getTarget();
        const value_idx = data.getValue();

        // Check if target is a map index expression: m["key"] = value
        const target_tag = self.store.exprTag(target_idx);
        if (target_tag == .index) {
            const target_data = self.store.exprData(target_idx);
            const object_idx = target_data.getObject();

            // Try to lower the object to see if it's a map
            const object_val = try self.lowerExpression(object_idx);
            if (isMapType(object_val.ty)) {
                // This is a map index assignment: m["key"] = value
                const key_idx = target_data.getIndex();
                const key_val = try self.lowerExpression(key_idx);
                const value = try self.lowerExpression(value_idx);
                try self.emit(.{
                    .map_set = .{
                        .map = object_val,
                        .key = key_val,
                        .value = value,
                        .loc = null,
                    },
                });
                return;
            }
        }

        // Check if target is a struct-typed variable (for db_read buffer unpacking)
        // Uses StructHelper for centralized struct detection
        if (StructHelper.detectStructTypeFromExpr(&self.scopes, self.store, self.strings, target_idx)) |info| {
            // This is an assignment to a struct variable (e.g., cust = db_read(...))
            // Lower the value (call expression), then emit store_struct_buf
            log.debug("lowerAssignment: struct target '{s}' type '{s}'", .{ info.base_name, info.structName() });
            debug.print(.ir, "lowerAssignment: target '{s}' is struct type '{s}', emitting store_struct_buf", .{ info.base_name, info.structName() });

            const value = try self.lowerExpression(value_idx);
            const inst = StructHelper.makeStoreStructBufInst(info, value);
            try self.emit(inst);
            return;
        }

        // Normal assignment - lower both sides and emit store
        var value = try self.lowerExpression(value_idx);
        const target = try self.lowerLValue(target_idx);

        // DBL compatibility: when storing a numeric value to a string_fixed field,
        // format it with zero-padding to the field width
        const target_type = switch (target.ty) {
            .ptr => |p| p.*,
            else => target.ty,
        };

        // Decimal type validation and conversion
        if (target_type == .decimal) {
            if (!value.isNumeric()) {
                // Non-numeric value (string) being assigned to decimal field
                // Emit parse_decimal to validate and convert at runtime
                // This will raise "bad digit" error if string contains non-numeric chars
                debug.print(.ir, "DBL parse_decimal: string value to decimal, emitting parse_decimal for runtime validation", .{});

                const func = self.current_func orelse return LowerError.OutOfMemory;
                const parsed = func.newValue(.i64);
                try self.emit(.{
                    .parse_decimal = .{
                        .value = value,
                        .result = parsed,
                    },
                });
                value = parsed;
            }
        }

        if (target_type == .string_fixed and value.isNumeric()) {
            const width = target_type.string_fixed;
            debug.print(.ir, "DBL decimal format: numeric value to string_fixed({d}), emitting format_decimal", .{width});

            // Emit format_decimal instruction to convert integer to zero-padded string
            const func = self.current_func orelse return LowerError.OutOfMemory;
            const formatted = func.newValue(.{ .string_fixed = width });
            try self.emit(.{
                .format_decimal = .{
                    .value = value,
                    .width = width,
                    .result = formatted,
                },
            });
            value = formatted;
        }

        try self.emit(.{
            .store = .{
                .ptr = target,
                .value = value,
            },
        });
    }

    /// Lower an if statement
    fn lowerIf(self: *Self, _: StmtIdx, data: NodeData) LowerError!void {
        const func = self.current_func orelse return LowerError.OutOfMemory;

        const cond_idx = data.getCondition();
        const then_body = data.getThenBody();

        // Get else body from extra_data
        // The data packing puts else_body index in extra_data
        const extra_idx_raw = data.b & 0xFFFF;
        const else_body_raw = self.store.extra_data.items[extra_idx_raw];
        const else_body: StmtIdx = @enumFromInt(else_body_raw);

        // Evaluate condition
        const cond_val = try self.lowerExpression(cond_idx);

        // Create blocks
        const then_block = try func.createBlock("if.then");
        const else_block = try func.createBlock("if.else");
        const merge_block = try func.createBlock("if.merge");

        // Branch based on condition
        try self.emit(.{
            .brif = .{
                .condition = cond_val,
                .then_block = then_block,
                .else_block = else_block,
            },
        });

        // Then block
        self.current_block = then_block;
        try self.lowerStatement(then_body);
        if (!self.current_block.?.isTerminated()) {
            try self.emit(.{ .jump = .{ .target = merge_block } });
        }

        // Else block
        self.current_block = else_block;
        if (else_body != .null) {
            try self.lowerStatement(else_body);
        }
        if (!self.current_block.?.isTerminated()) {
            try self.emit(.{ .jump = .{ .target = merge_block } });
        }

        // Continue at merge block
        self.current_block = merge_block;
    }

    /// Lower a return statement
    fn lowerReturn(self: *Self, data: NodeData) LowerError!void {
        const value_idx = data.getReturnValue();

        if (value_idx != .null) {
            const value = try self.lowerExpression(value_idx);
            try self.emit(.{ .return_ = value });
        } else {
            try self.emit(.{ .return_ = null });
        }
    }

    /// Lower a while loop
    fn lowerWhile(self: *Self, data: NodeData) LowerError!void {
        const func = self.current_func orelse return LowerError.OutOfMemory;

        const cond_idx = data.getCondition();
        const body_idx = data.getBody();

        // Create blocks
        const cond_block = try func.createBlock("while.cond");
        const body_block = try func.createBlock("while.body");
        const exit_block = try func.createBlock("while.exit");

        // Save and set loop context
        const prev_exit = self.loop_exit_block;
        const prev_continue = self.loop_continue_block;
        self.loop_exit_block = exit_block;
        self.loop_continue_block = cond_block;

        // Jump to condition
        try self.emit(.{ .jump = .{ .target = cond_block } });

        // Condition block
        self.current_block = cond_block;
        const cond_val = try self.lowerExpression(cond_idx);
        try self.emit(.{
            .brif = .{
                .condition = cond_val,
                .then_block = body_block,
                .else_block = exit_block,
            },
        });

        // Body block
        self.current_block = body_block;
        try self.lowerStatement(body_idx);
        if (!self.current_block.?.isTerminated()) {
            try self.emit(.{ .jump = .{ .target = cond_block } });
        }

        // Restore loop context
        self.loop_exit_block = prev_exit;
        self.loop_continue_block = prev_continue;

        // Continue at exit block
        self.current_block = exit_block;
    }

    /// Lower a for loop
    fn lowerFor(self: *Self, stmt_idx: StmtIdx, data: NodeData) LowerError!void {
        const func = self.current_func orelse return LowerError.OutOfMemory;

        const binding_id = data.getBinding();
        const iterable_idx = data.getIterable();

        // Get body from extra_data
        const extra_idx_raw = data.b & 0xFFFF;
        const body_idx: StmtIdx = @enumFromInt(self.store.extra_data.items[extra_idx_raw]);

        const binding_name = self.strings.get(binding_id);
        if (binding_name.len == 0) return LowerError.UndefinedVariable;

        // For now, treat for loops as simple iteration over a range
        // Create loop variable
        const ty_ptr = try self.allocator.create(ir.Type);
        ty_ptr.* = .i64;
        try self.allocated_types.append(self.allocator, ty_ptr);

        const loop_var = func.newValue(.{ .ptr = ty_ptr });
        try self.emit(.{
            .alloca = .{
                .ty = .i64,
                .name = binding_name,
                .result = loop_var,
            },
        });
        try self.scopes.put(binding_name, loop_var);

        // Create blocks
        const cond_block = try func.createBlock("for.cond");
        const body_block = try func.createBlock("for.body");
        const incr_block = try func.createBlock("for.incr");
        const exit_block = try func.createBlock("for.exit");

        _ = stmt_idx;
        _ = iterable_idx;

        // Save and set loop context
        const prev_exit = self.loop_exit_block;
        const prev_continue = self.loop_continue_block;
        self.loop_exit_block = exit_block;
        self.loop_continue_block = incr_block;

        // TODO: Initialize from range start and check against range end
        try self.emit(.{ .jump = .{ .target = cond_block } });

        // Condition block - always true for now (infinite loop protection needed)
        self.current_block = cond_block;
        try self.emit(.{ .jump = .{ .target = body_block } });

        // Body block
        self.current_block = body_block;
        try self.lowerStatement(body_idx);
        if (!self.current_block.?.isTerminated()) {
            try self.emit(.{ .jump = .{ .target = incr_block } });
        }

        // Increment block
        self.current_block = incr_block;
        try self.emit(.{ .jump = .{ .target = cond_block } });

        // Restore loop context
        self.loop_exit_block = prev_exit;
        self.loop_continue_block = prev_continue;

        // Continue at exit block
        self.current_block = exit_block;
    }

    /// Lower an infinite loop
    fn lowerLoop(self: *Self, data: NodeData) LowerError!void {
        const func = self.current_func orelse return LowerError.OutOfMemory;

        const body_idx: StmtIdx = @enumFromInt(data.a);

        // Create blocks
        const body_block = try func.createBlock("loop.body");
        const exit_block = try func.createBlock("loop.exit");

        // Save and set loop context
        const prev_exit = self.loop_exit_block;
        const prev_continue = self.loop_continue_block;
        self.loop_exit_block = exit_block;
        self.loop_continue_block = body_block;

        // Jump to body
        try self.emit(.{ .jump = .{ .target = body_block } });

        // Body block
        self.current_block = body_block;
        try self.lowerStatement(body_idx);
        if (!self.current_block.?.isTerminated()) {
            try self.emit(.{ .jump = .{ .target = body_block } });
        }

        // Restore loop context
        self.loop_exit_block = prev_exit;
        self.loop_continue_block = prev_continue;

        // Continue at exit block
        self.current_block = exit_block;
    }

    /// Lower a let declaration
    fn lowerLetDecl(self: *Self, data: NodeData) LowerError!void {
        const func = self.current_func orelse return LowerError.OutOfMemory;

        const name_id = data.getName();
        const name = self.strings.get(name_id);
        if (name.len == 0) return LowerError.UndefinedVariable;

        // Get type and init from packed data
        const type_raw = (data.b >> 16) & 0xFFFF;
        // Check for 16-bit null (0xFFFF) since TypeIdx.null is 32-bit
        const type_idx: TypeIdx = if (type_raw == 0xFFFF) .null else @enumFromInt(type_raw);
        const extra_idx = data.b & 0xFFFF;
        const init_idx: ExprIdx = @enumFromInt(self.store.extra_data.items[extra_idx]);

        // Determine the variable type - either explicit or inferred from init
        var var_type: ir.Type = undefined;
        var init_val: ?ir.Value = null;

        if (init_idx != .null) {
            // Lower init expression first to get its type for inference
            init_val = try self.lowerExpression(init_idx);
        }

        if (type_idx != .null) {
            // Explicit type annotation
            var_type = try self.lowerTypeIdx(type_idx);
        } else if (init_val) |val| {
            // Infer type from init expression
            var_type = val.ty;
        } else {
            // No type and no init - default to void (error will be caught later)
            var_type = .void;
        }

        // Allocate variable
        const ty_ptr = try self.allocator.create(ir.Type);
        ty_ptr.* = var_type;
        try self.allocated_types.append(self.allocator, ty_ptr);

        const alloca_result = func.newValue(.{ .ptr = ty_ptr });
        try self.emit(.{
            .alloca = .{
                .ty = var_type,
                .name = name,
                .result = alloca_result,
            },
        });
        try self.scopes.put(name, alloca_result);

        // Store init value if present
        if (init_val) |val| {
            try self.emit(.{
                .store = .{
                    .ptr = alloca_result,
                    .value = val,
                },
            });
        }
    }

    /// Lower a field view (overlay) declaration
    /// A field_view creates an alias to another field's memory, optionally with an offset
    fn lowerFieldView(self: *Self, stmt_idx: StmtIdx) LowerError!void {
        const func = self.current_func orelse return LowerError.OutOfMemory;
        const data = self.store.stmtData(stmt_idx);

        // Get view name from data.a
        const name_id: StringId = @enumFromInt(data.a);
        const name = self.strings.get(name_id);
        if (name.len == 0) return LowerError.UndefinedVariable;

        // Get type and extra_start from data.b
        const type_raw = (data.b >> 16) & 0xFFFF;
        const type_idx: TypeIdx = if (type_raw == 0xFFFF) .null else @enumFromInt(type_raw);
        const extra_start = data.b & 0xFFFF;

        // Get base_field and offset from extra_data
        const base_field_id: StringId = @enumFromInt(self.store.extra_data.items[extra_start]);
        const offset: i32 = @bitCast(self.store.extra_data.items[extra_start + 1]);
        const base_field_name = self.strings.get(base_field_id);

        // Determine the view type
        var view_type: ir.Type = undefined;
        if (type_idx != .null) {
            view_type = try self.lowerTypeIdx(type_idx);
        } else {
            view_type = .{ .string_fixed = 1 }; // Default to a1
        }

        // Look up the base field's memory location
        if (self.scopes.get(base_field_name)) |base_ptr| {
            // Field overlay: view shares the same memory as base_ptr (plus offset)
            // For offset 0, the view is simply an alias to the same slot
            // For non-zero offsets, we need byte-level access (not yet supported)

            if (offset == 0) {
                // Simple alias - the view name maps to the exact same storage slot as the base
                // No instruction needed - we just register the view name pointing to base_ptr
                try self.scopes.put(name, base_ptr);
                debug.print(.ir, "field_view '{s}' aliased to '{s}' (offset 0)", .{ name, base_field_name });
            } else {
                // Offset access requires byte-level memory addressing
                // For now, we log a warning and create a separate variable
                // TODO: Implement proper byte-level substring views
                debug.print(.ir, "WARNING: field_view '{s}' with offset {d} not fully supported, creating separate variable", .{ name, offset });

                const ty_ptr = try self.allocator.create(ir.Type);
                ty_ptr.* = view_type;
                try self.allocated_types.append(self.allocator, ty_ptr);

                const alloca_result = func.newValue(.{ .ptr = ty_ptr });
                try self.emit(.{
                    .alloca = .{
                        .ty = view_type,
                        .name = name,
                        .result = alloca_result,
                    },
                });
                try self.scopes.put(name, alloca_result);
            }
        } else {
            // Base field not found - create a regular alloca as fallback
            // This allows code to compile even if the base field declaration comes later
            const ty_ptr = try self.allocator.create(ir.Type);
            ty_ptr.* = view_type;
            try self.allocated_types.append(self.allocator, ty_ptr);

            const alloca_result = func.newValue(.{ .ptr = ty_ptr });
            try self.emit(.{
                .alloca = .{
                    .ty = view_type,
                    .name = name,
                    .result = alloca_result,
                },
            });
            try self.scopes.put(name, alloca_result);
        }
    }

    /// Lower a top-level let declaration as a global variable
    /// This is used for DBL common blocks where globals need to be visible in all functions
    fn lowerGlobalLetDecl(self: *Self, stmt_idx: StmtIdx) LowerError!void {
        const data = self.store.stmtData(stmt_idx);
        const name_id = data.getName();
        const name = self.strings.get(name_id);
        if (name.len == 0) return LowerError.UndefinedVariable;

        // Get type from packed data
        const type_raw = (data.b >> 16) & 0xFFFF;
        const type_idx: TypeIdx = if (type_raw == 0xFFFF) .null else @enumFromInt(type_raw);

        // Determine the variable type
        var var_type: ir.Type = undefined;
        if (type_idx != .null) {
            var_type = try self.lowerTypeIdx(type_idx);
        } else {
            // Default to void if no type (shouldn't happen for common blocks)
            var_type = .void;
        }

        // Create a placeholder value for the global
        // When functions access this, they'll look it up by name
        const ty_ptr = try self.allocator.create(ir.Type);
        ty_ptr.* = var_type;
        try self.allocated_types.append(self.allocator, ty_ptr);

        // Create a synthetic value for the global
        // In the actual implementation, globals would be allocated at module level
        // For now, we just register the name so functions can find it
        const global_val = ir.Value{
            .id = 0xFFFFFF, // Special ID for globals
            .ty = .{ .ptr = ty_ptr },
        };

        try self.global_variables.put(name, global_val);
        debug.print(.ir, "Registered global: {s}", .{name});
    }

    /// Lower a buffer expression, handling struct types specially by emitting load_struct_buf
    /// to serialize all struct fields into a contiguous buffer for I/O operations.
    /// Uses StructHelper for centralized struct detection.
    fn lowerStructBufferOrExpression(self: *Self, buffer_idx: ExprIdx) LowerError!ir.Value {
        const func = self.current_func orelse return LowerError.OutOfMemory;

        // Check if buffer expression is a struct type using centralized detection
        if (StructHelper.detectStructTypeFromExpr(&self.scopes, self.store, self.strings, buffer_idx)) |info| {
            // Emit load_struct_buf to serialize the struct
            log.debug("lowerStructBufferOrExpression: serializing '{s}' type '{s}'", .{ info.base_name, info.structName() });
            debug.print(.ir, "lowerStructBufferOrExpression: emitting load_struct_buf for '{s}' type '{s}'", .{ info.base_name, info.structName() });

            const result = StructHelper.makeResultValue(func, info);
            const inst = StructHelper.makeLoadStructBufInst(info, result);
            try self.emit(inst);
            return result;
        }

        // Not a struct type variable - use normal expression lowering
        return try self.lowerExpression(buffer_idx);
    }

    /// Lower I/O open statement
    fn lowerIoOpen(self: *Self, stmt_idx: StmtIdx) LowerError!void {
        const func = self.current_func orelse return LowerError.OutOfMemory;
        _ = func;
        const data = self.store.stmtData(stmt_idx);

        // data.a is an ExprIdx for the channel expression, not a literal number
        const channel_idx: ExprIdx = @enumFromInt(data.a);
        const path_idx: ExprIdx = @enumFromInt(data.b);

        // Lower the channel expression (could be a literal, variable, or struct field like app.ch_cust)
        const channel_val = try self.lowerExpression(channel_idx);
        const path_val = try self.lowerExpression(path_idx);

        try self.emit(.{
            .io_open = .{
                .channel = channel_val,
                .filename = path_val,
                .mode = .update,
            },
        });
    }

    /// Lower I/O close statement
    fn lowerIoClose(self: *Self, stmt_idx: StmtIdx) LowerError!void {
        const func = self.current_func orelse return LowerError.OutOfMemory;
        _ = func;
        const data = self.store.stmtData(stmt_idx);

        // data.a is an ExprIdx for the channel expression
        const channel_idx: ExprIdx = @enumFromInt(data.a);
        const channel_val = try self.lowerExpression(channel_idx);

        try self.emit(.{
            .io_close = .{
                .channel = channel_val,
            },
        });
    }

    /// Lower I/O read statement
    fn lowerIoRead(self: *Self, stmt_idx: StmtIdx) LowerError!void {
        const func = self.current_func orelse return LowerError.OutOfMemory;
        _ = func;
        const data = self.store.stmtData(stmt_idx);

        // data.a is an ExprIdx for the channel expression, not a literal number
        const channel_idx: ExprIdx = @enumFromInt(data.a);
        const buffer_idx: ExprIdx = @enumFromInt(data.b);

        // Lower the channel expression (could be a literal, variable, or struct field like app.ch_cust)
        const channel_val = try self.lowerExpression(channel_idx);

        const buffer_val = try self.lowerLValueFromExpr(buffer_idx);

        // Check if buffer is a struct-typed variable - if so, we need to unpack after read
        var struct_name: ?[]const u8 = null;
        var base_name: ?[]const u8 = null;
        const expr_tag = self.store.exprTag(buffer_idx);
        if (expr_tag == .identifier) {
            const expr_data = self.store.exprData(buffer_idx);
            const name_id = expr_data.getName();
            const name = self.strings.get(name_id);

            if (self.scopes.get(name)) |var_val| {
                if (var_val.ty == .ptr) {
                    const pointee_type = var_val.ty.ptr.*;
                    if (pointee_type == .@"struct") {
                        struct_name = pointee_type.@"struct".name;
                        base_name = name; // Capture the variable name for slot lookup
                        debug.print(.ir, "lowerIoRead: buffer '{s}' is struct type '{s}'", .{ name, struct_name.? });
                    }
                }
            }
        }

        try self.emit(.{
            .io_read = .{
                .channel = channel_val,
                .buffer = buffer_val,
                .key = null,
                .qualifiers = .{},
                .struct_name = struct_name,
                .base_name = base_name,
            },
        });
    }

    /// Lower I/O write statement
    fn lowerIoWrite(self: *Self, stmt_idx: StmtIdx) LowerError!void {
        const func = self.current_func orelse return LowerError.OutOfMemory;
        _ = func;
        const data = self.store.stmtData(stmt_idx);

        // data.a is an ExprIdx for the channel expression
        const channel_idx: ExprIdx = @enumFromInt(data.a);
        const buffer_idx: ExprIdx = @enumFromInt(data.b);

        // Lower the channel expression (could be a literal, variable, or struct field like app.ch_cust)
        const channel_val = try self.lowerExpression(channel_idx);

        // Check if buffer is a struct type variable - if so, we need to serialize it
        const buffer_val = try self.lowerStructBufferOrExpression(buffer_idx);

        try self.emit(.{
            .io_write = .{
                .channel = channel_val,
                .buffer = buffer_val,
                .is_insert = false,
            },
        });
    }

    /// Lower I/O store statement
    fn lowerIoStore(self: *Self, stmt_idx: StmtIdx) LowerError!void {
        debug.print(.ir, ">>> lowerIoStore called", .{});
        const func = self.current_func orelse return LowerError.OutOfMemory;
        _ = func;
        const data = self.store.stmtData(stmt_idx);

        // data.a is an ExprIdx for the channel expression
        const channel_idx: ExprIdx = @enumFromInt(data.a);
        const buffer_idx: ExprIdx = @enumFromInt(data.b);
        debug.print(.ir, "lowerIoStore: channel_idx={d} buffer_idx={d}", .{ @intFromEnum(channel_idx), @intFromEnum(buffer_idx) });

        // Lower the channel expression (could be a literal, variable, or struct field like app.ch_cust)
        const channel_val = try self.lowerExpression(channel_idx);

        // Check if buffer is a struct type variable - if so, we need to serialize it
        const buffer_val = try self.lowerStructBufferOrExpression(buffer_idx);

        // Store is like write but for ISAM insert
        try self.emit(.{
            .io_write = .{
                .channel = channel_val,
                .buffer = buffer_val,
                .is_insert = true,
            },
        });
    }

    /// Lower I/O delete statement
    fn lowerIoDelete(self: *Self, stmt_idx: StmtIdx) LowerError!void {
        const func = self.current_func orelse return LowerError.OutOfMemory;
        _ = func;
        const data = self.store.stmtData(stmt_idx);

        // data.a is an ExprIdx for the channel expression
        const channel_idx: ExprIdx = @enumFromInt(data.a);
        const channel_val = try self.lowerExpression(channel_idx);

        try self.emit(.{
            .io_delete = .{
                .channel = channel_val,
            },
        });
    }

    /// Lower try/catch statement
    fn lowerTry(self: *Self, stmt_idx: StmtIdx) LowerError!void {
        // For now, just lower the try body - proper exception handling TBD
        const data = self.store.stmtData(stmt_idx);
        const body_idx: StmtIdx = @enumFromInt(data.a);

        try self.lowerStatement(body_idx);
    }

    /// Lower match statement
    fn lowerMatch(self: *Self, stmt_idx: StmtIdx) LowerError!void {
        const func = self.current_func orelse return LowerError.OutOfMemory;
        const data = self.store.stmtData(stmt_idx);

        // data.a = scrutinee expr, data.b = arms_start in extra_data
        const scrutinee_idx = data.getExpr();
        const arms_start: ast.ExtraIdx = @enumFromInt(data.b);

        const scrutinee_val = try self.lowerExpression(scrutinee_idx);

        // Get arm count from extra_data
        const arm_count = self.store.getExtra(arms_start);

        if (arm_count == 0) {
            // Empty match - nothing to do
            return;
        }

        // Create exit block
        const exit_block = try func.createBlock("match.exit");

        // Create blocks for each arm body and next check
        var arm_blocks: std.ArrayListUnmanaged(*ir.Block) = .{};
        defer arm_blocks.deinit(self.allocator);
        var check_blocks: std.ArrayListUnmanaged(*ir.Block) = .{};
        defer check_blocks.deinit(self.allocator);

        var i: u32 = 0;
        while (i < arm_count) : (i += 1) {
            const arm_block = try func.createBlock("match.arm");
            arm_blocks.append(self.allocator, arm_block) catch return LowerError.OutOfMemory;

            if (i < arm_count - 1) {
                const check_block = try func.createBlock("match.check");
                check_blocks.append(self.allocator, check_block) catch return LowerError.OutOfMemory;
            }
        }

        // Generate code for each arm
        i = 0;
        while (i < arm_count) : (i += 1) {
            // Get pattern and body from extra_data
            // Each arm is stored as (pattern_expr_idx, body_stmt_idx)
            const pattern_raw = self.store.getExtra(@enumFromInt(@intFromEnum(arms_start) + 1 + i * 2));
            const body_raw = self.store.getExtra(@enumFromInt(@intFromEnum(arms_start) + 2 + i * 2));

            const pattern_idx: ExprIdx = @enumFromInt(pattern_raw);
            const body_idx: StmtIdx = @enumFromInt(body_raw);

            // Lower pattern expression
            const pattern_val = try self.lowerExpression(pattern_idx);

            // Compare scrutinee with pattern
            const cmp_result = func.newValue(.bool);
            try self.emit(.{
                .icmp = .{
                    .cond = .eq,
                    .lhs = scrutinee_val,
                    .rhs = pattern_val,
                    .result = cmp_result,
                },
            });

            // Branch based on comparison
            const arm_block = arm_blocks.items[i];
            const else_block = if (i < arm_count - 1)
                check_blocks.items[i]
            else
                exit_block; // Last arm falls through to exit if no match

            try self.emit(.{
                .brif = .{
                    .condition = cmp_result,
                    .then_block = arm_block,
                    .else_block = else_block,
                },
            });

            // Generate arm body
            self.current_block = arm_block;
            try self.lowerStatement(body_idx);

            // Jump to exit after arm body
            if (!self.current_block.?.isTerminated()) {
                try self.emit(.{ .jump = .{ .target = exit_block } });
            }

            // Move to next check block if there is one
            if (i < arm_count - 1) {
                self.current_block = check_blocks.items[i];
            }
        }

        // Continue with exit block
        self.current_block = exit_block;
    }

    /// Lower an expression to get its address (for assignment targets)
    fn lowerLValue(self: *Self, expr_idx: ExprIdx) LowerError!ir.Value {
        const tag = self.store.exprTag(expr_idx);
        const data = self.store.exprData(expr_idx);

        switch (tag) {
            .identifier => {
                const name_id = data.getName();
                const name = self.strings.get(name_id);
                if (name.len == 0) return LowerError.UndefinedVariable;

                // Check local variables first (via scopes), then global variables
                if (self.scopes.get(name)) |ptr| {
                    return ptr;
                }
                if (self.global_variables.get(name)) |global_ptr| {
                    // For global variables, emit an alloca so the emitter can track the value ID
                    const func = self.current_func orelse return LowerError.OutOfMemory;

                    // Get the underlying type from the pointer
                    const var_type = switch (global_ptr.ty) {
                        .ptr => |p| p.*,
                        else => global_ptr.ty,
                    };

                    // Create a new value with a proper ID
                    const result = func.newValue(.{ .ptr = switch (global_ptr.ty) {
                        .ptr => |p| p,
                        else => blk: {
                            const ty_ptr = self.allocator.create(ir.Type) catch return LowerError.OutOfMemory;
                            ty_ptr.* = global_ptr.ty;
                            self.allocated_types.append(self.allocator, ty_ptr) catch return LowerError.OutOfMemory;
                            break :blk ty_ptr;
                        },
                    } });

                    // Emit alloca for the global variable
                    try self.emit(.{
                        .alloca = .{
                            .name = name,
                            .ty = var_type,
                            .result = result,
                        },
                    });

                    return result;
                }
                return LowerError.UndefinedVariable;
            },
            .member => {
                // Member access as lvalue
                return self.lowerMemberPtr(expr_idx);
            },
            .index => {
                // Index access as lvalue
                return self.lowerIndexPtr(expr_idx);
            },
            else => return LowerError.InvalidExpression,
        }
    }

    /// Helper for lowerLValue that takes an ExprIdx directly
    fn lowerLValueFromExpr(self: *Self, expr_idx: ExprIdx) LowerError!ir.Value {
        return self.lowerLValue(expr_idx);
    }

    /// Lower a member access to get a pointer
    fn lowerMemberPtr(self: *Self, expr_idx: ExprIdx) LowerError!ir.Value {
        const data = self.store.exprData(expr_idx);
        const object_idx = data.getObject();
        const field_id = data.getField();

        const object_ptr = try self.lowerLValue(object_idx);
        const field_name = self.strings.get(field_id);
        if (field_name.len == 0) return LowerError.UndefinedVariable;

        // Look up field in struct type
        const struct_type = switch (object_ptr.ty) {
            .ptr => |p| switch (p.*) {
                .@"struct" => |s| s,
                else => return LowerError.TypeMismatch,
            },
            else => return LowerError.TypeMismatch,
        };

        var field_idx: ?u32 = null;
        for (struct_type.fields, 0..) |f, i| {
            if (std.mem.eql(u8, f.name, field_name)) {
                field_idx = @intCast(i);
                break;
            }
        }

        if (field_idx) |idx| {
            const func = self.current_func orelse return LowerError.OutOfMemory;
            const field = struct_type.fields[idx];

            const ty_ptr = try self.allocator.create(ir.Type);
            ty_ptr.* = field.ty;
            try self.allocated_types.append(self.allocator, ty_ptr);

            const result = func.newValue(.{ .ptr = ty_ptr });
            try self.emit(.{
                .field_ptr = .{
                    .struct_ptr = object_ptr,
                    .field_index = idx,
                    .result = result,
                },
            });
            return result;
        }

        return LowerError.UndefinedVariable;
    }

    /// Lower an index access to get a pointer
    fn lowerIndexPtr(self: *Self, expr_idx: ExprIdx) LowerError!ir.Value {
        const func = self.current_func orelse return LowerError.OutOfMemory;
        const data = self.store.exprData(expr_idx);

        const object_idx = data.getObject();
        const index_idx = data.getIndex();

        const object_ptr = try self.lowerLValue(object_idx);
        const index_val = try self.lowerExpression(index_idx);

        // Get element type from array/slice type
        const elem_type = switch (object_ptr.ty) {
            .ptr => |p| switch (p.*) {
                .array => |a| a.element.*,
                .slice => |s| s.*,
                else => return LowerError.TypeMismatch,
            },
            else => return LowerError.TypeMismatch,
        };

        const ty_ptr = try self.allocator.create(ir.Type);
        ty_ptr.* = elem_type;
        try self.allocated_types.append(self.allocator, ty_ptr);

        // Use array_load since IR doesn't have get_element_ptr
        // Note: This returns a value, not a pointer - mutable array access needs IR extension
        const result = func.newValue(.{ .ptr = ty_ptr });
        try self.emit(.{
            .array_load = .{
                .array_ptr = object_ptr,
                .index = index_val,
                .result = result,
            },
        });
        return result;
    }

    /// Lower an expression to an IR value
    fn lowerExpression(self: *Self, expr_idx: ExprIdx) LowerError!ir.Value {
        const func = self.current_func orelse return LowerError.UnsupportedFeature;
        const tag = self.store.exprTag(expr_idx);
        const data = self.store.exprData(expr_idx);

        switch (tag) {
            .int_literal => return self.lowerIntLiteral(func, data),
            .float_literal => return self.lowerFloatLiteral(func, data),
            .string_literal => return self.lowerStringLiteral(func, data),
            .bool_literal => return self.lowerBoolLiteral(func, data),
            .null_literal => return self.lowerNullLiteral(func),
            .identifier => return self.lowerIdentifier(func, data),
            .binary => return self.lowerBinary(func, data, expr_idx),
            .unary => return self.lowerUnary(func, data, expr_idx),
            .grouping => return self.lowerExpression(@enumFromInt(data.a)),
            .call => return self.lowerCall(expr_idx),
            .method_call => return self.lowerMethodCall(expr_idx),
            .member => return self.lowerMember(func, expr_idx),
            .index => return self.lowerIndex(func, expr_idx),
            .array_init => return self.lowerArrayInit(expr_idx),
            .struct_init => return self.lowerStructInit(expr_idx),
            .lambda => return self.lowerLambda(expr_idx),
            .range => {
                // Range expressions are lowered by for loop handling, not as standalone values
                // If we get here, a range was used outside of a for loop context
                return LowerError.UnsupportedFeature;
            },
            .comptime_builtin => {
                // Comptime builtins should be evaluated at compile time by the comptime evaluator
                // If we get here, a comptime builtin wasn't resolved during constant folding
                return LowerError.UnsupportedFeature;
            },
            .if_expr, .match_expr, .block_expr => {
                // Expression forms (if/match/block as expressions) not yet supported
                // These would require SSA phi nodes or temporary variables
                return LowerError.UnsupportedFeature;
            },
        }
    }

    // ============================================================
    // Expression Handlers
    // ============================================================

    /// Lower an integer literal
    fn lowerIntLiteral(self: *Self, func: *ir.Function, data: NodeData) LowerError!ir.Value {
        const value = data.getIntValue();
        const result = func.newValue(.i64);
        try self.emit(.{
            .iconst = .{
                .ty = .i64,
                .value = value,
                .result = result,
            },
        });
        return result;
    }

    /// Lower a float literal
    fn lowerFloatLiteral(self: *Self, func: *ir.Function, data: NodeData) LowerError!ir.Value {
        const bits: u64 = (@as(u64, data.b) << 32) | data.a;
        const value: f64 = @bitCast(bits);
        const result = func.newValue(.f64);
        try self.emit(.{
            .f64const = .{
                .value = value,
                .result = result,
            },
        });
        return result;
    }

    /// Lower a string literal
    fn lowerStringLiteral(self: *Self, func: *ir.Function, data: NodeData) LowerError!ir.Value {
        const str_id = data.getName();
        const str = self.strings.get(str_id);
        const result = func.newValue(.{ .string_fixed = @intCast(str.len) });
        try self.emit(.{
            .const_string = .{
                .value = str,
                .result = result,
            },
        });
        return result;
    }

    /// Lower a boolean literal (represented as iconst 0 or 1 to match Cranelift)
    fn lowerBoolLiteral(self: *Self, func: *ir.Function, data: NodeData) LowerError!ir.Value {
        const value: i64 = if (data.a != 0) 1 else 0;
        const result = func.newValue(.bool);
        try self.emit(.{
            .iconst = .{
                .ty = .bool,
                .value = value,
                .result = result,
            },
        });
        return result;
    }

    /// Lower a null literal
    fn lowerNullLiteral(self: *Self, func: *ir.Function) LowerError!ir.Value {
        const ty_ptr = try self.allocator.create(ir.Type);
        ty_ptr.* = .void;
        try self.allocated_types.append(self.allocator, ty_ptr);
        const result = func.newValue(.{ .optional = ty_ptr });
        try self.emit(.{
            .const_null = .{
                .ty = .{ .optional = ty_ptr },
                .result = result,
            },
        });
        return result;
    }

    /// Lower an identifier expression (variable load)
    fn lowerIdentifier(self: *Self, func: *ir.Function, data: NodeData) LowerError!ir.Value {
        const name_id = data.getName();
        const name = self.strings.get(name_id);
        if (name.len == 0) return LowerError.UndefinedVariable;

        // Check local variables first (via scopes), then global variables
        const ptr = self.scopes.get(name) orelse
            self.global_variables.get(name) orelse
            return LowerError.UndefinedVariable;

        const result = func.newValue(ptr.ty);
        try self.emit(.{
            .load = .{
                .ptr = ptr,
                .result = result,
            },
        });
        return result;
    }

    /// Lower a binary expression
    fn lowerBinary(self: *Self, func: *ir.Function, data: NodeData, expr_idx: ExprIdx) LowerError!ir.Value {
        _ = expr_idx; // Used for error location, currently unused with auto-coercion
        const lhs_idx = data.getLhs();
        const rhs_idx = data.getRhs();
        const op = data.getBinaryOp();

        const lhs = try self.lowerExpression(lhs_idx);
        const rhs = try self.lowerExpression(rhs_idx);

        // Determine the result type based on the operation
        const result_ty: ir.Type = switch (op) {
            // Comparison operations always produce bool
            .eq, .ne, .lt, .le, .gt, .ge => .bool,
            // Logical operations always produce bool
            .@"and", .@"or" => .bool,
            // Arithmetic and bitwise operations inherit operand type
            .add, .sub, .mul, .div, .mod, .bit_and, .bit_or, .bit_xor, .shl, .shr => lhs.ty,
            .range, .range_inclusive => lhs.ty,
        };

        const result = func.newValue(result_ty);

        const ir_op: ir.Instruction = switch (op) {
            .add => blk: {
                const lhs_is_string = (lhs.ty == .string or lhs.ty == .string_fixed);
                const rhs_is_string = (rhs.ty == .string or rhs.ty == .string_fixed);

                debug.print(.ir, "binary add: lhs.ty={s} rhs.ty={s} lhs_is_string={} rhs_is_string={}", .{
                    @tagName(lhs.ty),
                    @tagName(rhs.ty),
                    lhs_is_string,
                    rhs_is_string,
                });

                // Auto-coercion: if either operand is a string, use string concatenation
                // The VM will auto-convert integers/decimals to their string representation
                const is_concat = lhs_is_string or rhs_is_string;
                debug.print(.ir, "binary add decision: {s}", .{if (is_concat) "str_concat (auto-coerce)" else "iadd"});

                break :blk if (is_concat)
                    .{ .str_concat = .{ .lhs = lhs, .rhs = rhs, .result = result } }
                else
                    .{ .iadd = .{ .lhs = lhs, .rhs = rhs, .result = result } };
            },
            .sub => .{ .isub = .{ .lhs = lhs, .rhs = rhs, .result = result } },
            .mul => .{ .imul = .{ .lhs = lhs, .rhs = rhs, .result = result } },
            .div => .{ .sdiv = .{ .lhs = lhs, .rhs = rhs, .result = result } },
            .mod => .{ .srem = .{ .lhs = lhs, .rhs = rhs, .result = result } },
            .eq => .{ .icmp = .{ .cond = .eq, .lhs = lhs, .rhs = rhs, .result = result } },
            .ne => .{ .icmp = .{ .cond = .ne, .lhs = lhs, .rhs = rhs, .result = result } },
            .lt => .{ .icmp = .{ .cond = .slt, .lhs = lhs, .rhs = rhs, .result = result } },
            .le => .{ .icmp = .{ .cond = .sle, .lhs = lhs, .rhs = rhs, .result = result } },
            .gt => .{ .icmp = .{ .cond = .sgt, .lhs = lhs, .rhs = rhs, .result = result } },
            .ge => .{ .icmp = .{ .cond = .sge, .lhs = lhs, .rhs = rhs, .result = result } },
            .@"and" => .{ .log_and = .{ .lhs = lhs, .rhs = rhs, .result = result } },
            .@"or" => .{ .log_or = .{ .lhs = lhs, .rhs = rhs, .result = result } },
            .bit_and => .{ .band = .{ .lhs = lhs, .rhs = rhs, .result = result } },
            .bit_or => .{ .bor = .{ .lhs = lhs, .rhs = rhs, .result = result } },
            .bit_xor => .{ .bxor = .{ .lhs = lhs, .rhs = rhs, .result = result } },
            .shl => .{ .ishl = .{ .lhs = lhs, .rhs = rhs, .result = result } },
            .shr => .{ .sshr = .{ .lhs = lhs, .rhs = rhs, .result = result } },
            .range, .range_inclusive => return LowerError.UnsupportedFeature,
        };

        try self.emit(ir_op);
        return result;
    }

    /// Lower a unary expression
    fn lowerUnary(self: *Self, func: *ir.Function, data: NodeData, expr_idx: ExprIdx) LowerError!ir.Value {
        _ = expr_idx;
        const operand_idx = data.getOperand();
        const op = data.getUnaryOp();

        const operand = try self.lowerExpression(operand_idx);

        switch (op) {
            .neg => {
                const result = func.newValue(operand.ty);
                try self.emit(.{ .ineg = .{ .operand = operand, .result = result } });
                return result;
            },
            .not => {
                // Logical not always produces bool
                const result = func.newValue(.bool);
                try self.emit(.{ .log_not = .{ .operand = operand, .result = result } });
                return result;
            },
            .bit_not => {
                const result = func.newValue(operand.ty);
                try self.emit(.{ .bnot = .{ .operand = operand, .result = result } });
                return result;
            },
            .addr_of => {
                // Get address of operand - use lowerLValue to get pointer
                const ptr = try self.lowerLValue(operand_idx);
                return ptr;
            },
            .deref => {
                // Dereference pointer - operand should be a pointer, result is pointee type
                const pointee_ty = if (operand.ty == .ptr) operand.ty.ptr.* else operand.ty;
                const deref_result = func.newValue(pointee_ty);
                try self.emit(.{
                    .load = .{
                        .ptr = operand,
                        .result = deref_result,
                    },
                });
                return deref_result;
            },
        }
    }

    /// Lower a member access expression
    fn lowerMember(self: *Self, func: *ir.Function, expr_idx: ExprIdx) LowerError!ir.Value {
        const ptr = try self.lowerMemberPtr(expr_idx);
        const result = func.newValue(ptr.ty);
        try self.emit(.{
            .load = .{
                .ptr = ptr,
                .result = result,
            },
        });
        return result;
    }

    /// Lower an index expression
    fn lowerIndex(self: *Self, func: *ir.Function, expr_idx: ExprIdx) LowerError!ir.Value {
        const data = self.store.exprData(expr_idx);
        const object_idx = data.getObject();
        const index_idx = data.getIndex();

        // First, try to determine if the object is a map
        const object_val = try self.lowerExpression(object_idx);

        // Check if object is a map type - emit map_get instruction
        if (isMapType(object_val.ty)) {
            const key_val = try self.lowerExpression(index_idx);
            const result = func.newValue(.string); // Map values are strings by default
            try self.emit(.{
                .map_get = .{
                    .map = object_val,
                    .key = key_val,
                    .result = result,
                    .loc = null,
                },
            });
            return result;
        }

        // Fallback to array/slice indexing via pointer
        const ptr = try self.lowerIndexPtr(expr_idx);
        const result = func.newValue(ptr.ty);
        try self.emit(.{
            .load = .{
                .ptr = ptr,
                .result = result,
            },
        });
        return result;
    }

    /// Lower a function call expression
    fn lowerCall(self: *Self, expr_idx: ExprIdx) LowerError!ir.Value {
        const func = self.current_func orelse return LowerError.OutOfMemory;
        const data = self.store.exprData(expr_idx);

        const callee_idx = data.getCallee();
        const args_count = data.b >> 16;
        const args_start = data.b & 0xFFFF;

        // Get callee name
        const callee_tag = self.store.exprTag(callee_idx);

        // Check for Map.new() static method call OR map instance method calls
        if (callee_tag == .member) {
            const callee_data = self.store.exprData(callee_idx);
            const object_idx = callee_data.getObject();
            const field_id = callee_data.getField();
            const field_name = self.strings.get(field_id);

            // Check if object is "Map" identifier (static method)
            const object_tag = self.store.exprTag(object_idx);
            if (object_tag == .identifier) {
                const object_data = self.store.exprData(object_idx);
                const object_name_id = object_data.getName();
                const object_name = self.strings.get(object_name_id);

                if (std.mem.eql(u8, object_name, "Map") and std.mem.eql(u8, field_name, "new")) {
                    // Map.new() - emit map_new instruction
                    // Default flags: case_sensitive=true, preserve_spaces=true
                    const flags: u8 = 0x03; // bit 0 = case_sensitive, bit 1 = preserve_spaces

                    const result = func.newValue(.{ .map = undefined });
                    try self.emit(.{
                        .map_new = .{
                            .flags = flags,
                            .result = result,
                            .loc = null,
                        },
                    });
                    return result;
                }
            }

            // Check if this is a method call on a map instance: m.set(), m.get(), etc.
            const object_val = try self.lowerExpression(object_idx);
            if (isMapType(object_val.ty)) {
                // Lower arguments
                var method_args: std.ArrayListUnmanaged(ir.Value) = .{};
                defer method_args.deinit(self.allocator);
                for (0..args_count) |i| {
                    const arg_idx: ExprIdx = @enumFromInt(self.store.extra_data.items[args_start + i]);
                    const arg_val = try self.lowerExpression(arg_idx);
                    try method_args.append(self.allocator, arg_val);
                }

                // Dispatch to appropriate map operation
                if (std.mem.eql(u8, field_name, "set") and method_args.items.len >= 2) {
                    try self.emit(.{
                        .map_set = .{
                            .map = object_val,
                            .key = method_args.items[0],
                            .value = method_args.items[1],
                            .loc = null,
                        },
                    });
                    return func.newValue(.void);
                } else if (std.mem.eql(u8, field_name, "get") and method_args.items.len >= 1) {
                    const result = func.newValue(.string);
                    try self.emit(.{
                        .map_get = .{
                            .map = object_val,
                            .key = method_args.items[0],
                            .result = result,
                            .loc = null,
                        },
                    });
                    return result;
                } else if (std.mem.eql(u8, field_name, "has") and method_args.items.len >= 1) {
                    const result = func.newValue(.bool);
                    try self.emit(.{
                        .map_has = .{
                            .map = object_val,
                            .key = method_args.items[0],
                            .result = result,
                            .loc = null,
                        },
                    });
                    return result;
                } else if (std.mem.eql(u8, field_name, "delete") and method_args.items.len >= 1) {
                    try self.emit(.{
                        .map_delete = .{
                            .map = object_val,
                            .key = method_args.items[0],
                            .loc = null,
                        },
                    });
                    return func.newValue(.void);
                } else if (std.mem.eql(u8, field_name, "len")) {
                    const result = func.newValue(.i64);
                    try self.emit(.{
                        .map_len = .{
                            .map = object_val,
                            .result = result,
                            .loc = null,
                        },
                    });
                    return result;
                } else if (std.mem.eql(u8, field_name, "clear")) {
                    try self.emit(.{
                        .map_clear = .{
                            .map = object_val,
                            .loc = null,
                        },
                    });
                    return func.newValue(.void);
                } else if (std.mem.eql(u8, field_name, "keys")) {
                    const result = func.newValue(.string);
                    try self.emit(.{
                        .map_keys = .{
                            .map = object_val,
                            .result = result,
                            .loc = null,
                        },
                    });
                    return result;
                } else if (std.mem.eql(u8, field_name, "values")) {
                    const result = func.newValue(.string);
                    try self.emit(.{
                        .map_values = .{
                            .map = object_val,
                            .result = result,
                            .loc = null,
                        },
                    });
                    return result;
                }
            }
        }

        const callee_name = if (callee_tag == .identifier) blk: {
            const callee_data = self.store.exprData(callee_idx);
            const name_id = callee_data.getName();
            break :blk self.strings.get(name_id);
        } else "";

        // Check if this is a database I/O call that needs structure serialization
        const is_db_write = std.mem.eql(u8, callee_name, "db_store") or std.mem.eql(u8, callee_name, "db_write");

        // Lower arguments
        var args: std.ArrayListUnmanaged(ir.Value) = .{};
        defer args.deinit(self.allocator);

        for (0..args_count) |i| {
            const arg_idx: ExprIdx = @enumFromInt(self.store.extra_data.items[args_start + i]);

            // For db_store/db_write, the second argument (index 1) is the record
            // If it's a struct-type variable, we need to serialize it
            if (is_db_write and i == 1) {
                const arg_val = try self.lowerStructBufferOrExpression(arg_idx);
                try args.append(self.allocator, arg_val);
            } else {
                const arg_val = try self.lowerExpression(arg_idx);
                try args.append(self.allocator, arg_val);
            }
        }

        // Check if we need to fill in default values for missing arguments
        if (self.fn_param_defaults.get(callee_name)) |defaults| {
            const provided_args = args.items.len;
            const total_params = defaults.len;

            // If fewer args provided than params, fill in defaults
            if (provided_args < total_params) {
                for (provided_args..total_params) |i| {
                    const default_expr_raw = defaults[i];
                    if (default_expr_raw != 0) {
                        // Lower the default expression
                        const default_expr_idx: ExprIdx = @enumFromInt(default_expr_raw);
                        const default_val = try self.lowerExpression(default_expr_idx);
                        args.append(self.allocator, default_val) catch return LowerError.OutOfMemory;
                    } else {
                        // Required param missing - compile-time error
                        const loc = self.store.exprLoc(expr_idx);
                        self.setErrorContext(
                            LowerError.MissingRequiredParam,
                            "missing required parameter {d} in call to '{s}'",
                            .{ i + 1, callee_name },
                            loc,
                            "checking function call arguments",
                            .{},
                        );
                        return LowerError.MissingRequiredParam;
                    }
                }
            }
        }

        const args_slice = try args.toOwnedSlice(self.allocator);
        // NOTE: args_slice is freed by Block.deinit when the call instruction is cleaned up

        // Determine result type based on builtin function
        const result_type = getBuiltinReturnType(callee_name, args_slice);
        const result = func.newValue(result_type);

        try self.emit(.{
            .call = .{
                .callee = callee_name,
                .args = args_slice,
                .result = result,
            },
        });

        return result;
    }

    /// Lower a method call expression
    fn lowerMethodCall(self: *Self, expr_idx: ExprIdx) LowerError!ir.Value {
        const func = self.current_func orelse return LowerError.OutOfMemory;
        const data = self.store.exprData(expr_idx);

        // Method calls have object in a, method name + args in extra
        const object_idx: ExprIdx = @enumFromInt(data.a);
        const extra_start = data.b;

        const method_id: StringId = @enumFromInt(self.store.extra_data.items[extra_start]);
        const args_count = self.store.extra_data.items[extra_start + 1];

        const method_name = self.strings.get(method_id);

        // Get object value
        const object_val = try self.lowerExpression(object_idx);

        // Check if object is a map type - emit map instructions
        if (isMapType(object_val.ty)) {
            // Lower arguments
            var method_args: std.ArrayListUnmanaged(ir.Value) = .{};
            defer method_args.deinit(self.allocator);
            for (0..args_count) |i| {
                const arg_idx: ExprIdx = @enumFromInt(self.store.extra_data.items[extra_start + 2 + i]);
                const arg_val = try self.lowerExpression(arg_idx);
                try method_args.append(self.allocator, arg_val);
            }

            // Dispatch to appropriate map operation
            if (std.mem.eql(u8, method_name, "set") and method_args.items.len >= 2) {
                try self.emit(.{
                    .map_set = .{
                        .map = object_val,
                        .key = method_args.items[0],
                        .value = method_args.items[1],
                        .loc = null,
                    },
                });
                return func.newValue(.void);
            } else if (std.mem.eql(u8, method_name, "get") and method_args.items.len >= 1) {
                const result = func.newValue(.string); // Map values are strings by default
                try self.emit(.{
                    .map_get = .{
                        .map = object_val,
                        .key = method_args.items[0],
                        .result = result,
                        .loc = null,
                    },
                });
                return result;
            } else if (std.mem.eql(u8, method_name, "has") and method_args.items.len >= 1) {
                const result = func.newValue(.bool);
                try self.emit(.{
                    .map_has = .{
                        .map = object_val,
                        .key = method_args.items[0],
                        .result = result,
                        .loc = null,
                    },
                });
                return result;
            } else if (std.mem.eql(u8, method_name, "delete") and method_args.items.len >= 1) {
                try self.emit(.{
                    .map_delete = .{
                        .map = object_val,
                        .key = method_args.items[0],
                        .loc = null,
                    },
                });
                return func.newValue(.void);
            } else if (std.mem.eql(u8, method_name, "len")) {
                const result = func.newValue(.i64);
                try self.emit(.{
                    .map_len = .{
                        .map = object_val,
                        .result = result,
                        .loc = null,
                    },
                });
                return result;
            } else if (std.mem.eql(u8, method_name, "clear")) {
                try self.emit(.{
                    .map_clear = .{
                        .map = object_val,
                        .loc = null,
                    },
                });
                return func.newValue(.void);
            } else if (std.mem.eql(u8, method_name, "keys")) {
                const result = func.newValue(.string); // Returns comma-separated keys
                try self.emit(.{
                    .map_keys = .{
                        .map = object_val,
                        .result = result,
                        .loc = null,
                    },
                });
                return result;
            } else if (std.mem.eql(u8, method_name, "values")) {
                const result = func.newValue(.string); // Returns comma-separated values
                try self.emit(.{
                    .map_values = .{
                        .map = object_val,
                        .result = result,
                        .loc = null,
                    },
                });
                return result;
            }
        }

        // Fallback to regular method call
        var args: std.ArrayListUnmanaged(ir.Value) = .{};
        defer args.deinit(self.allocator);
        try args.append(self.allocator, object_val);

        for (0..args_count) |i| {
            const arg_idx: ExprIdx = @enumFromInt(self.store.extra_data.items[extra_start + 2 + i]);
            const arg_val = try self.lowerExpression(arg_idx);
            try args.append(self.allocator, arg_val);
        }

        const args_slice = try args.toOwnedSlice(self.allocator);
        // NOTE: args_slice is freed by Block.deinit when the call instruction is cleaned up

        const result_type = getBuiltinReturnType(method_name, args_slice);
        const result = func.newValue(result_type);

        try self.emit(.{
            .call = .{
                .callee = method_name,
                .args = args_slice,
                .result = result,
            },
        });

        return result;
    }

    /// Lower an array initialization expression
    fn lowerArrayInit(self: *Self, expr_idx: ExprIdx) LowerError!ir.Value {
        const func = self.current_func orelse return LowerError.OutOfMemory;
        const data = self.store.exprData(expr_idx);

        // data.a = start index in extra_data (where count is stored)
        // data.b = element count
        const extra_start: ast.ExtraIdx = @enumFromInt(data.a);
        const elem_count = data.b;

        if (elem_count == 0) {
            // Empty array - return null pointer for now
            const result = func.newValue(.void);
            return result;
        }

        // Lower all elements to get their values and infer element type
        var elem_values: std.ArrayListUnmanaged(ir.Value) = .{};
        defer elem_values.deinit(self.allocator);

        var i: u32 = 0;
        while (i < elem_count) : (i += 1) {
            const elem_idx_raw = self.store.getExtra(@enumFromInt(@intFromEnum(extra_start) + 1 + i));
            const elem_idx: ExprIdx = @enumFromInt(elem_idx_raw);
            const elem_val = try self.lowerExpression(elem_idx);
            elem_values.append(self.allocator, elem_val) catch return LowerError.OutOfMemory;
        }

        // Use first element's type as array element type
        const elem_type = elem_values.items[0].ty;
        const elem_type_ptr = try self.allocator.create(ir.Type);
        elem_type_ptr.* = elem_type;
        try self.allocated_types.append(self.allocator, elem_type_ptr);

        // Create array type
        const array_type = ir.Type{ .array = .{ .element = elem_type_ptr, .length = elem_count } };
        const array_type_ptr = try self.allocator.create(ir.Type);
        array_type_ptr.* = array_type;
        try self.allocated_types.append(self.allocator, array_type_ptr);

        // Allocate stack space for array
        const array_ptr = func.newValue(.{ .ptr = array_type_ptr });
        try self.emit(.{
            .alloca = .{
                .ty = array_type,
                .name = "_array_init",
                .result = array_ptr,
            },
        });

        // Store each element into the array
        for (elem_values.items, 0..) |elem_val, idx| {
            // Create index constant
            const idx_val = func.newValue(.i64);
            try self.emit(.{
                .iconst = .{ .ty = .i64, .value = @intCast(idx), .result = idx_val },
            });

            // Store element at index
            try self.emit(.{
                .array_store = .{
                    .array_ptr = array_ptr,
                    .index = idx_val,
                    .value = elem_val,
                },
            });
        }

        // Return pointer to array
        return array_ptr;
    }

    /// Lower a struct initialization expression
    fn lowerStructInit(self: *Self, expr_idx: ExprIdx) LowerError!ir.Value {
        const func = self.current_func orelse return LowerError.OutOfMemory;
        const data = self.store.exprData(expr_idx);

        // data.a = type_name (StringId)
        // data.b = start index in extra_data
        const type_name_id: StringId = @enumFromInt(data.a);
        const type_name = self.strings.get(type_name_id);
        if (type_name.len == 0) return LowerError.UndefinedType;

        // Look up struct type
        const struct_type = self.struct_types.get(type_name) orelse return LowerError.UndefinedType;

        // Get field count and field data from extra
        const extra_start: ast.ExtraIdx = @enumFromInt(data.b);
        const field_count = self.store.getExtra(extra_start);

        // Create struct type for allocation
        const struct_ir_type = ir.Type{ .@"struct" = struct_type };
        const struct_type_ptr = try self.allocator.create(ir.Type);
        struct_type_ptr.* = struct_ir_type;
        try self.allocated_types.append(self.allocator, struct_type_ptr);

        // Allocate stack space for struct
        const struct_ptr = func.newValue(.{ .ptr = struct_type_ptr });
        try self.emit(.{
            .alloca = .{
                .ty = struct_ir_type,
                .name = type_name,
                .result = struct_ptr,
            },
        });

        // Store each field
        var i: u32 = 0;
        while (i < field_count) : (i += 1) {
            // Each field has: (field_name_id, expr_idx) in extra_data
            const field_name_id_raw = self.store.getExtra(@enumFromInt(@intFromEnum(extra_start) + 1 + i * 2));
            const field_expr_raw = self.store.getExtra(@enumFromInt(@intFromEnum(extra_start) + 2 + i * 2));

            const field_name_id: StringId = @enumFromInt(field_name_id_raw);
            const field_name = self.strings.get(field_name_id);
            const field_expr_idx: ExprIdx = @enumFromInt(field_expr_raw);

            // Find field index in struct type
            var field_index: ?u32 = null;
            for (struct_type.fields, 0..) |field, idx| {
                if (std.mem.eql(u8, field.name, field_name)) {
                    field_index = @intCast(idx);
                    break;
                }
            }

            if (field_index) |idx| {
                // Lower the field value
                const field_val = try self.lowerExpression(field_expr_idx);

                // Get field type
                const field_type = struct_type.fields[idx].ty;
                const field_type_ptr = try self.allocator.create(ir.Type);
                field_type_ptr.* = field_type;
                try self.allocated_types.append(self.allocator, field_type_ptr);

                // Get pointer to field
                const field_ptr = func.newValue(.{ .ptr = field_type_ptr });
                try self.emit(.{
                    .field_ptr = .{
                        .struct_ptr = struct_ptr,
                        .field_index = idx,
                        .result = field_ptr,
                    },
                });

                // Store value to field
                try self.emit(.{
                    .store = .{
                        .ptr = field_ptr,
                        .value = field_val,
                    },
                });
            }
            // Skip unknown fields silently (could add warning)
        }

        // Return pointer to struct
        return struct_ptr;
    }

    /// Lower a lambda expression
    fn lowerLambda(self: *Self, expr_idx: ExprIdx) LowerError!ir.Value {
        const outer_func = self.current_func orelse return LowerError.OutOfMemory;
        const data = self.store.exprData(expr_idx);

        // data.a = params_start in extra_data
        // extra_data layout: [param_count, param1_name, param1_type, ..., body_stmt]
        const params_start: ast.ExtraIdx = @enumFromInt(data.a);
        const param_count = self.store.getExtra(params_start);

        // Generate unique lambda name
        const lambda_name = std.fmt.allocPrint(self.allocator, "_lambda_{d}", .{self.lambda_counter}) catch return LowerError.OutOfMemory;
        self.lambda_counter += 1;

        // Parse parameters
        var params: std.ArrayListUnmanaged(ir.FunctionType.Param) = .{};
        defer params.deinit(self.allocator);

        var extra_idx = @intFromEnum(params_start) + 1;
        for (0..param_count) |_| {
            const param_name_id: StringId = @enumFromInt(self.store.extra_data.items[extra_idx]);
            const param_type_idx: TypeIdx = @enumFromInt(self.store.extra_data.items[extra_idx + 1]);
            const param_direction_raw: u32 = self.store.extra_data.items[extra_idx + 2];
            const default_expr_raw: u32 = self.store.extra_data.items[extra_idx + 3];
            extra_idx += 4; // 4 values per param: name, type, is_ref, default_value

            const param_name = self.strings.get(param_name_id);
            if (param_name.len == 0) continue;

            // Use type if specified, otherwise default to void (inferred later)
            const param_type = if (param_type_idx != .null)
                try self.lowerTypeIdx(param_type_idx)
            else
                ir.Type.void;

            // is_ref: 0 = by value, non-zero = by reference
            const is_ref = param_direction_raw != 0;

            // default_value: handled at call time
            _ = default_expr_raw;

            params.append(self.allocator, .{
                .name = param_name,
                .ty = param_type,
                .is_ref = is_ref,
            }) catch return LowerError.OutOfMemory;
        }

        // Get body statement index
        const body_idx: StmtIdx = @enumFromInt(self.store.extra_data.items[extra_idx]);

        // Create function type (return type will be inferred as void for now)
        const func_type = self.allocator.create(ir.FunctionType) catch return LowerError.OutOfMemory;
        func_type.* = .{
            .params = params.toOwnedSlice(self.allocator) catch return LowerError.OutOfMemory,
            .return_type = .void,
            .is_variadic = false,
        };

        // Create lambda function
        const lambda_func = ir.Function.init(self.allocator, lambda_name, func_type.*) catch return LowerError.OutOfMemory;

        // Save current context
        const saved_func = self.current_func;
        const saved_block = self.current_block;

        // Switch to lambda context
        self.current_func = lambda_func;
        self.current_block = lambda_func.entry;

        // Push new scope for lambda parameters/locals
        try self.scopes.push();

        // Add parameters as local variables
        for (func_type.params) |param| {
            const ty_ptr = self.allocator.create(ir.Type) catch return LowerError.OutOfMemory;
            ty_ptr.* = param.ty;
            self.allocated_types.append(self.allocator, ty_ptr) catch return LowerError.OutOfMemory;

            const alloca_result = lambda_func.newValue(.{ .ptr = ty_ptr });
            try self.emit(.{
                .alloca = .{
                    .ty = param.ty,
                    .name = param.name,
                    .result = alloca_result,
                },
            });
            self.scopes.put(param.name, alloca_result) catch return LowerError.OutOfMemory;
        }

        // Lower lambda body
        try self.lowerStatement(body_idx);

        // Add implicit return if not terminated
        if (!self.current_block.?.isTerminated()) {
            try self.emit(.{ .return_ = null });
        }

        // Add lambda function to module
        self.module.addFunction(lambda_func) catch return LowerError.OutOfMemory;

        // Restore context
        self.scopes.pop();
        self.current_func = saved_func;
        self.current_block = saved_block;

        // Return function pointer value
        const func_type_ptr = self.allocator.create(ir.FunctionType) catch return LowerError.OutOfMemory;
        func_type_ptr.* = func_type.*;
        const fn_ptr_type = ir.Type{ .function = func_type_ptr };
        const fn_ptr_type_ptr = self.allocator.create(ir.Type) catch return LowerError.OutOfMemory;
        fn_ptr_type_ptr.* = fn_ptr_type;
        self.allocated_types.append(self.allocator, fn_ptr_type_ptr) catch return LowerError.OutOfMemory;

        // Create a value representing the function pointer
        const result = outer_func.newValue(fn_ptr_type);
        return result;
    }

    /// Lower a type index to an IR type
    fn lowerTypeIdx(self: *Self, type_idx: TypeIdx) LowerError!ir.Type {
        if (type_idx == .null) {
            return .void;
        }

        const tag = self.store.typeTag(type_idx);
        const data = self.store.typeData(type_idx);

        return switch (tag) {
            .void => .void,
            .bool => .bool,
            .i8 => .i8,
            .i16 => .i16,
            .i32 => .i32,
            .i64 => .i64,
            .u8 => .u8,
            .u16 => .u16,
            .u32 => .u32,
            .u64 => .u64,
            .f32 => .f32,
            .f64 => .f64,
            .string => .string,
            .string_fixed => .{ .string_fixed = data.a },
            .decimal => .{ .decimal = .{ .precision = @intCast(data.a), .scale = @intCast(data.b) } },
            .array => blk: {
                const elem_type_idx: TypeIdx = @enumFromInt(data.a);
                const length = data.b;
                const elem_type = try self.lowerTypeIdx(elem_type_idx);
                const elem_ptr = try self.allocator.create(ir.Type);
                elem_ptr.* = elem_type;
                try self.allocated_types.append(self.allocator, elem_ptr);
                break :blk .{ .array = .{ .element = elem_ptr, .length = length } };
            },
            .slice => blk: {
                const elem_type_idx: TypeIdx = @enumFromInt(data.a);
                const elem_type = try self.lowerTypeIdx(elem_type_idx);
                const elem_ptr = try self.allocator.create(ir.Type);
                elem_ptr.* = elem_type;
                try self.allocated_types.append(self.allocator, elem_ptr);
                break :blk .{ .slice = elem_ptr };
            },
            .optional => blk: {
                const inner_type_idx: TypeIdx = @enumFromInt(data.a);
                const inner_type = try self.lowerTypeIdx(inner_type_idx);
                const inner_ptr = try self.allocator.create(ir.Type);
                inner_ptr.* = inner_type;
                try self.allocated_types.append(self.allocator, inner_ptr);
                break :blk .{ .optional = inner_ptr };
            },
            .pointer => blk: {
                const pointee_type_idx: TypeIdx = @enumFromInt(data.a);
                const pointee_type = try self.lowerTypeIdx(pointee_type_idx);
                const pointee_ptr = try self.allocator.create(ir.Type);
                pointee_ptr.* = pointee_type;
                try self.allocated_types.append(self.allocator, pointee_ptr);
                break :blk .{ .ptr = pointee_ptr };
            },
            .named => blk: {
                const name_id: StringId = @enumFromInt(data.a);
                const name = self.strings.get(name_id);
                if (name.len == 0) return LowerError.UndefinedType;
                if (self.struct_types.get(name)) |struct_type| {
                    break :blk .{ .@"struct" = struct_type };
                }
                return LowerError.UndefinedType;
            },
            // Generic type parameter - look up in substitution map
            .type_param => blk: {
                const name_id: StringId = @enumFromInt(data.a);
                const name = self.strings.get(name_id);
                if (name.len == 0) break :blk .void;
                // Look up the type parameter in current substitutions
                if (self.type_param_substitutions.get(name)) |concrete_type| {
                    break :blk concrete_type;
                }
                // Type parameter not substituted - this is an error during instantiation
                break :blk .void;
            },
            // Generic type instantiation - instantiate with concrete type arguments
            .generic_instance => blk: {
                const base_type_idx = TypeIdx.fromInt(data.a);
                const args_start = data.b;

                // Get base type info
                const base_tag = self.store.typeTag(base_type_idx);
                if (base_tag != .named) break :blk .void;

                const base_data = self.store.typeData(base_type_idx);
                const base_name_id: StringId = @enumFromInt(base_data.a);
                const base_name = self.strings.get(base_name_id);
                if (base_name.len == 0) break :blk .void;

                // Get type arguments from extra_data
                const arg_count = self.store.extra_data.items[args_start];
                var type_args: std.ArrayListUnmanaged(ir.Type) = .{};
                defer type_args.deinit(self.allocator);

                for (0..arg_count) |i| {
                    const arg_type_idx = TypeIdx.fromInt(self.store.extra_data.items[args_start + 1 + i]);
                    const arg_type = try self.lowerTypeIdx(arg_type_idx);
                    try type_args.append(self.allocator, arg_type);
                }

                // Try to instantiate the generic type
                const instantiated = try self.instantiateGenericStruct(base_name, type_args.items);
                if (instantiated) |struct_type| {
                    break :blk .{ .@"struct" = struct_type };
                }

                // Fallback: check if it's already a non-generic struct
                if (self.struct_types.get(base_name)) |struct_type| {
                    break :blk .{ .@"struct" = struct_type };
                }

                break :blk .void;
            },
            .map => blk: {
                // Map<K, V> type - key type in data.a, value type in data.b
                const key_type_idx: TypeIdx = @enumFromInt(data.a);
                const value_type_idx: TypeIdx = @enumFromInt(data.b);
                const key_type = try self.lowerTypeIdx(key_type_idx);
                const value_type = try self.lowerTypeIdx(value_type_idx);

                // Allocate MapType struct
                const map_type = try self.allocator.create(ir.MapType);
                const key_ptr = try self.allocator.create(ir.Type);
                const value_ptr = try self.allocator.create(ir.Type);
                key_ptr.* = key_type;
                value_ptr.* = value_type;
                try self.allocated_types.append(self.allocator, key_ptr);
                try self.allocated_types.append(self.allocator, value_ptr);

                map_type.* = .{
                    .key_type = key_ptr,
                    .value_type = value_ptr,
                };

                break :blk .{ .map = map_type };
            },
            .function => .void, // Function types are handled separately
            .@"union" => .void, // Union types are handled via union_def statement
            // These types don't have IR equivalents yet
            .tuple, .error_union, .inferred, .any, .never => .void,
        };
    }
};

/// Determine return type of builtin functions
fn getBuiltinReturnType(name: []const u8, args: []const ir.Value) ir.Type {
    // String functions that return strings
    if (std.mem.eql(u8, name, "trim") or
        std.mem.eql(u8, name, "upper") or
        std.mem.eql(u8, name, "lower") or
        std.mem.eql(u8, name, "atrim") or
        std.mem.eql(u8, name, "chr") or
        std.mem.eql(u8, name, "string") or
        std.mem.eql(u8, name, "str_setchar") or
        std.mem.eql(u8, name, "substr") or
        std.mem.eql(u8, name, "replace") or
        std.mem.eql(u8, name, "format") or
        std.mem.eql(u8, name, "concat"))
    {
        return .string;
    }

    // Numeric functions that return integers
    if (std.mem.eql(u8, name, "len") or
        std.mem.eql(u8, name, "size") or
        std.mem.eql(u8, name, "asc") or
        std.mem.eql(u8, name, "instr") or
        std.mem.eql(u8, name, "integer") or
        std.mem.eql(u8, name, "pos") or
        std.mem.eql(u8, name, "index"))
    {
        return .i64;
    }

    // Math functions that return floats
    if (std.mem.eql(u8, name, "sqrt") or
        std.mem.eql(u8, name, "sin") or
        std.mem.eql(u8, name, "cos") or
        std.mem.eql(u8, name, "tan") or
        std.mem.eql(u8, name, "log") or
        std.mem.eql(u8, name, "log10") or
        std.mem.eql(u8, name, "exp") or
        std.mem.eql(u8, name, "abs") or
        std.mem.eql(u8, name, "floor") or
        std.mem.eql(u8, name, "ceil") or
        std.mem.eql(u8, name, "round") or
        std.mem.eql(u8, name, "pow"))
    {
        return .f64;
    }

    // Boolean functions
    if (std.mem.eql(u8, name, "file_exists") or
        std.mem.eql(u8, name, "dir_exists") or
        std.mem.eql(u8, name, "is_numeric") or
        std.mem.eql(u8, name, "is_alpha"))
    {
        return .bool;
    }

    // Void functions (side-effect only)
    if (std.mem.eql(u8, name, "print") or
        std.mem.eql(u8, name, "println") or
        std.mem.eql(u8, name, "display") or
        std.mem.eql(u8, name, "clear") or
        std.mem.eql(u8, name, "sleep") or
        std.mem.eql(u8, name, "exit") or
        std.mem.eql(u8, name, "close") or
        std.mem.startsWith(u8, name, "t_")) // TUI functions
    {
        return .void;
    }

    // Default: return type of first argument or void
    if (args.len > 0) {
        return args[0].ty;
    }
    return .void;
}

/// Options for lowering
pub const LowerOptions = struct {
    /// Run IR verification after lowering (catches bugs early)
    verify: bool = false,
};

/// Main entry point for lowering
pub fn lower(
    allocator: Allocator,
    store: *const NodeStore,
    strings: *const StringInterner,
    top_level: []const StmtIdx,
    module_name: []const u8,
) LowerError!*ir.Module {
    return lowerWithOptions(allocator, store, strings, top_level, module_name, .{});
}

/// Main entry point for lowering with options
pub fn lowerWithOptions(
    allocator: Allocator,
    store: *const NodeStore,
    strings: *const StringInterner,
    top_level: []const StmtIdx,
    module_name: []const u8,
    options: LowerOptions,
) LowerError!*ir.Module {
    var lowerer = try Lowerer.init(allocator, store, strings, module_name);
    defer lowerer.deinit();

    const module = try lowerer.lowerProgram(top_level);

    // Optional IR verification
    if (options.verify) {
        var verifier = verify.Verifier.init(allocator, module);
        defer verifier.deinit();

        verifier.verify() catch {
            // Log verification errors
            for (verifier.getErrors()) |e| {
                log.err("IR verification: {any}", .{e});
            }
            return LowerError.VerificationFailed;
        };
    }

    return module;
}

/// Semantic diagnostic information for LSP
pub const SemanticDiagnostic = struct {
    line: u32,
    column: u32,
    message: []const u8,
    severity: enum { @"error", warning },
};

/// Analyze source for semantic errors without completing lowering
/// Returns a list of diagnostics (errors and warnings) that can be used by LSP
pub fn analyzeSemantics(
    allocator: Allocator,
    store: *const NodeStore,
    strings: *const StringInterner,
    top_level: []const StmtIdx,
) ![]SemanticDiagnostic {
    var diagnostics: std.ArrayListUnmanaged(SemanticDiagnostic) = .empty;
    errdefer diagnostics.deinit(allocator);

    var lowerer = Lowerer.init(allocator, store, strings, "analysis") catch {
        // Allocation error - return empty diagnostics
        return diagnostics.toOwnedSlice(allocator);
    };
    defer lowerer.deinit();

    // Try to lower - if it fails, capture the error
    _ = lowerer.lowerProgram(top_level) catch {
        // Capture the error context as a diagnostic
        if (lowerer.getLastError()) |ctx| {
            try diagnostics.append(allocator, .{
                .line = ctx.line,
                .column = ctx.column,
                .message = try allocator.dupe(u8, ctx.message),
                .severity = .@"error",
            });
        }
    };

    // Add any warnings
    for (lowerer.getWarnings()) |warning| {
        try diagnostics.append(allocator, .{
            .line = warning.line,
            .column = warning.column,
            .message = try allocator.dupe(u8, warning.message),
            .severity = .warning,
        });
    }

    return diagnostics.toOwnedSlice(allocator);
}

// ============================================
// Inline Tests (Ghostty pattern)
// ============================================

const testing = std.testing;

test "lower: Lowerer initialization" {
    const allocator = testing.allocator;

    // Create minimal NodeStore for testing
    var interner = ast.StringInterner.init(allocator);
    defer interner.deinit();

    var store = ast.NodeStore.init(allocator, &interner);
    defer store.deinit();

    // Create lowerer
    var lowerer = Lowerer.init(allocator, &store, &interner, null) catch |err| {
        debug.print(.ir, "Lowerer init failed: {any}", .{err});
        return err;
    };
    defer lowerer.deinit();

    // Verify initial state
    try testing.expect(lowerer.module != null);
    try testing.expectEqual(@as(usize, 0), lowerer.scopes.depth());
}

test "lower: type name resolution" {
    const allocator = testing.allocator;

    var interner = ast.StringInterner.init(allocator);
    defer interner.deinit();

    var store = ast.NodeStore.init(allocator, &interner);
    defer store.deinit();

    var lowerer = try Lowerer.init(allocator, &store, &interner, null);
    defer lowerer.deinit();

    // Test primitive type resolution
    const i32_name = try interner.intern("i32");
    const i32_type = lowerer.resolveTypeName(i32_name);
    try testing.expectEqual(ir.Type.i32, i32_type);

    const bool_name = try interner.intern("bool");
    const bool_type = lowerer.resolveTypeName(bool_name);
    try testing.expectEqual(ir.Type.bool, bool_type);

    const str_name = try interner.intern("str");
    const str_type = lowerer.resolveTypeName(str_name);
    try testing.expectEqual(ir.Type.str, str_type);
}

test "lower: scope stack push/pop" {
    const allocator = testing.allocator;

    var interner = ast.StringInterner.init(allocator);
    defer interner.deinit();

    var store = ast.NodeStore.init(allocator, &interner);
    defer store.deinit();

    var lowerer = try Lowerer.init(allocator, &store, &interner, null);
    defer lowerer.deinit();

    // Initial depth should be 0
    try testing.expectEqual(@as(usize, 0), lowerer.scopes.depth());

    // Push a scope
    lowerer.scopes.push();
    try testing.expectEqual(@as(usize, 1), lowerer.scopes.depth());

    // Pop the scope
    lowerer.scopes.pop();
    try testing.expectEqual(@as(usize, 0), lowerer.scopes.depth());
}
