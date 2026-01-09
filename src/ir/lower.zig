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
const builtin = @import("builtin");
const ast = @import("../ast/mod.zig");
const ir = @import("ir.zig");
const struct_serial = @import("struct_serialization.zig");
const scope_stack = @import("scope_stack.zig");
const verify = @import("verify.zig");
const cot_runtime = @import("cot_runtime");
const debug = cot_runtime.debug;
const comptime_eval = @import("../comptime/comptime.zig");

// Scoped logging (Ghostty pattern) - enable with std_options or runtime filter
const log = std.log.scoped(.@"ir-lower");

// Import extracted types (following Ghostty pattern)
const lower_types = @import("lower_types.zig");
pub const LowerError = lower_types.LowerError;

// Expression lowering (extracted to reduce file size)
const lower_expr = @import("lower_expr.zig");

// Statement lowering (extracted to reduce file size)
const lower_stmt = @import("lower_stmt.zig");

// I/O lowering (extracted to reduce file size)
const lower_io = @import("lower_io.zig");
pub const GenericDef = lower_types.GenericDef;
pub const InstantiationKey = lower_types.InstantiationKey;
pub const TraitMethodSig = lower_types.TraitMethodSig;
pub const TraitDef = lower_types.TraitDef;
pub const AssociatedTypeDef = lower_types.AssociatedTypeDef;
pub const AssociatedTypeBinding = lower_types.AssociatedTypeBinding;
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
const EnumDefView = ast.EnumDefView;
const VariantPayloadKind = ast.VariantPayloadKind;

const Allocator = std.mem.Allocator;

/// Lowering context that tracks state during AST to IR conversion
pub const Lowerer = struct {
    allocator: Allocator,
    module: *ir.Module,
    current_func: ?*ir.Function,
    current_block: ?*ir.Block,

    /// NodeStore-based AST (not owned)
    store: *const NodeStore,

    /// String interner (not owned, mutable for interning generated strings)
    strings: *StringInterner,

    /// Scope stack for variable management
    /// Provides hierarchical scope lookup with push/pop for blocks
    scopes: ScopeStack,

    /// Map global variable names to their IR values (for module-level globals like DBL common blocks)
    global_variables: std.StringHashMap(ir.Value),

    /// Map struct names to their IR types
    struct_types: std.StringHashMap(*const ir.StructType),

    /// Map union names to their IR types
    union_types: std.StringHashMap(*const ir.UnionType),

    /// Map enum names to their variant values (enum name -> (variant name -> i64))
    enum_types: std.StringHashMap(std.StringHashMap(i64)),

    /// Generic struct definitions (templates for instantiation)
    generic_struct_defs: std.StringHashMap(GenericDef),

    /// Generic function definitions (templates for instantiation)
    generic_fn_defs: std.StringHashMap(GenericDef),

    /// Cache of instantiated generic functions (mangled name -> already lowered)
    instantiated_fns: std.StringHashMap(void),

    /// Current type parameter substitutions (for generic instantiation)
    type_param_substitutions: std.StringHashMap(ir.Type),

    /// Cache of instantiated generic structs
    instantiated_structs: std.StringHashMap(*const ir.StructType),

    /// Allocated Type pointers that need to be freed on deinit
    allocated_types: std.ArrayList(*ir.Type),

    /// Allocated ListType pointers that need to be freed on deinit
    allocated_list_types: std.ArrayList(*ir.ListType),

    /// Current loop's exit block (for break statements)
    loop_exit_block: ?*ir.Block,

    /// Current loop's continue block (for continue statements)
    loop_continue_block: ?*ir.Block,

    /// Stack of deferred statement bodies (to execute at scope exit)
    defers: std.ArrayListUnmanaged(StmtIdx),

    /// Stack of scope marks for defer (each entry is the defer stack length when scope was entered)
    defer_scope_marks: std.ArrayListUnmanaged(usize),

    /// Warnings collected during lowering
    warnings: std.ArrayList(Warning),

    /// Context of the last error that occurred (for better error messages)
    last_error: ?ErrorContext,

    /// Counter for generating unique lambda names
    lambda_counter: u32,

    /// Current lambda's captured variables (name -> outer scope value)
    /// Set during lowerLambda, used by identifier lowering to detect captures
    current_captures: ?std.StringHashMap(ir.Value),

    /// The closure environment variable in the current lambda (if any)
    /// Points to the map containing captured values
    closure_env_value: ?ir.Value,

    /// Function parameter defaults (function name -> list of default ExprIdx per param)
    /// If ExprIdx is null (0), the param is required
    fn_param_defaults: std.StringHashMap([]const u32),

    /// Function parameter types (function name -> list of param types)
    /// Used for type coercion during call lowering (e.g., struct to trait object)
    fn_param_types: std.StringHashMap([]const ir.Type),

    /// Owned strings that need to be freed (e.g., qualified method names)
    owned_strings: std.ArrayListUnmanaged([]const u8),

    /// Function return types (function name -> return type)
    /// Used to determine call expression result types for user-defined functions
    fn_return_types: std.StringHashMap(ir.Type),

    /// Trait definitions (trait name -> TraitDef)
    trait_defs: std.StringHashMap(TraitDef),

    /// Trait implementations (ImplKey -> method implementations)
    impl_methods: ImplMethodsMap,

    /// Associated type bindings for each impl (ImplKey -> associated type bindings)
    impl_assoc_types: ImplAssocTypesMap,

    /// Current impl block context (set during impl block lowering for Self.Item resolution)
    current_impl_key: ?ImplKey,

    /// Comptime evaluator for evaluating comptime conditions during lowering
    comptime_evaluator: comptime_eval.Evaluator,

    /// Imported namespaces (e.g., "std.math" from `import std.math`)
    /// Used to validate that non-prelude namespace functions have been imported
    imported_namespaces: std.StringHashMap(void),

    /// Whether to emit explicit ARC instructions (arc_retain/arc_release).
    /// When false, the VM handles ARC at runtime.
    emit_arc: bool,

    /// Current source location being processed (for error messages)
    current_loc: SourceLoc,

    /// Source file path (for @file() builtin and error messages)
    source_file: ?[]const u8,

    /// Narrowed member paths (for type narrowing after null checks on member expressions)
    /// Maps full member path (e.g., "l.current_scope") to the narrowed type
    /// Used when accessing a member that has been null-checked to use unwrapped type
    narrowed_members: std.StringHashMap(ir.Type),

    /// Stack of narrowed member save points (for scope management)
    /// Each entry stores the set of keys that were added at that scope level
    narrowed_member_scopes: std.ArrayListUnmanaged(std.StringHashMap(void)),

    const ImplMethodsMap = std.HashMap(ImplKey, []const MethodImpl, ImplKeyContext, std.hash_map.default_max_load_percentage);
    const ImplAssocTypesMap = std.HashMap(ImplKey, []const AssociatedTypeBinding, ImplKeyContext, std.hash_map.default_max_load_percentage);

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
        file_path: ?[]const u8, // Source file path (from FileId lookup)
        line: u32,
        column: u32,
        context: []const u8, // What we were trying to do (e.g., "lowering assignment to 'x'")

        pub fn format(self: ErrorContext, writer: anytype) !void {
            if (self.file_path) |path| {
                try writer.print("{s}:{d}:{d}: error: {s}\n", .{ path, self.line, self.column, self.message });
            } else {
                try writer.print("{d}:{d}: error: {s}\n", .{ self.line, self.column, self.message });
            }
            if (self.context.len > 0) {
                try writer.print("  while {s}\n", .{self.context});
            }
        }
    };

    const Self = @This();

    /// Check if a type is a map type (directly or through a pointer)
    pub fn isMapType(ty: ir.Type) bool {
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

    pub fn init(allocator: Allocator, store: *const NodeStore, strings: *StringInterner, module_name: []const u8, options: LowerOptions) !Self {
        const module = try allocator.create(ir.Module);
        module.* = ir.Module.init(allocator, module_name);

        var self = Self{
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
            .enum_types = std.StringHashMap(std.StringHashMap(i64)).init(allocator),
            .generic_struct_defs = std.StringHashMap(GenericDef).init(allocator),
            .generic_fn_defs = std.StringHashMap(GenericDef).init(allocator),
            .instantiated_fns = std.StringHashMap(void).init(allocator),
            .type_param_substitutions = std.StringHashMap(ir.Type).init(allocator),
            .instantiated_structs = std.StringHashMap(*const ir.StructType).init(allocator),
            .allocated_types = .{},
            .allocated_list_types = .{},
            .loop_exit_block = null,
            .loop_continue_block = null,
            .defers = .{},
            .defer_scope_marks = .{},
            .warnings = .{},
            .last_error = null,
            .lambda_counter = 0,
            .current_captures = null,
            .closure_env_value = null,
            .fn_param_defaults = std.StringHashMap([]const u32).init(allocator),
            .fn_param_types = std.StringHashMap([]const ir.Type).init(allocator),
            .owned_strings = .{},
            .fn_return_types = std.StringHashMap(ir.Type).init(allocator),
            .trait_defs = std.StringHashMap(TraitDef).init(allocator),
            .impl_methods = ImplMethodsMap.init(allocator),
            .impl_assoc_types = ImplAssocTypesMap.init(allocator),
            .current_impl_key = null,
            .comptime_evaluator = comptime_eval.Evaluator.init(allocator, store, strings),
            .imported_namespaces = std.StringHashMap(void).init(allocator),
            .emit_arc = options.emit_arc,
            .current_loc = .{ .line = 0, .column = 0 },
            .source_file = options.source_file,
            .narrowed_members = std.StringHashMap(ir.Type).init(allocator),
            .narrowed_member_scopes = .{},
        };

        // Pre-populate type maps from dependency context (for cross-package compilation)
        if (options.dependency_types) |dep_types| {
            // Import struct types from dependencies
            var struct_it = dep_types.struct_types.iterator();
            while (struct_it.next()) |entry| {
                self.struct_types.put(entry.key_ptr.*, entry.value_ptr.*) catch {};
            }
            // Import union types from dependencies
            var union_it = dep_types.union_types.iterator();
            while (union_it.next()) |entry| {
                self.union_types.put(entry.key_ptr.*, entry.value_ptr.*) catch {};
            }
            // Import enum types from dependencies
            var enum_it = dep_types.enum_types.iterator();
            while (enum_it.next()) |entry| {
                // Clone the variant map
                var variants = std.StringHashMap(i64).init(allocator);
                var var_it = entry.value_ptr.iterator();
                while (var_it.next()) |var_entry| {
                    variants.put(var_entry.key_ptr.*, var_entry.value_ptr.*) catch {};
                }
                self.enum_types.put(entry.key_ptr.*, variants) catch {};
            }
            // Import function return types from dependencies
            var fn_it = dep_types.fn_return_types.iterator();
            while (fn_it.next()) |entry| {
                self.fn_return_types.put(entry.key_ptr.*, entry.value_ptr.*) catch {};
            }
        }

        return self;
    }

    /// Check if a function name is a known builtin
    fn isKnownBuiltin(name: []const u8) bool {
        return known_builtins.has(name);
    }

    /// Track an import statement for namespace validation
    fn trackImport(self: *Self, stmt_idx: StmtIdx) LowerError!void {
        const data = self.store.stmtData(stmt_idx);
        const module_path_id = data.getName();
        const module_path = self.strings.get(module_path_id);

        // Store the imported namespace
        self.imported_namespaces.put(module_path, {}) catch return LowerError.OutOfMemory;

        log.debug("Tracked import: {s}", .{module_path});
    }

    /// Check if a namespace has been imported (or is in prelude)
    pub fn isNamespaceImported(self: *const Self, namespace: []const u8) bool {
        return self.imported_namespaces.contains(namespace);
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
    pub fn setErrorContext(
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
            .file_path = self.source_file,
            .line = loc.line,
            .column = loc.column,
            .context = context,
        };
    }

    /// Set error context with file_id lookup (for more precise file tracking)
    pub fn setErrorContextWithFileId(
        self: *Self,
        err: LowerError,
        comptime message_fmt: []const u8,
        message_args: anytype,
        file_id: ast.FileId,
        loc: SourceLoc,
        comptime context_fmt: []const u8,
        context_args: anytype,
    ) void {
        const message = std.fmt.allocPrint(self.allocator, message_fmt, message_args) catch "allocation failed";
        const context = std.fmt.allocPrint(self.allocator, context_fmt, context_args) catch "";
        // Look up file path from NodeStore
        const file_path = self.store.getFilePath(file_id);
        self.last_error = .{
            .kind = err,
            .message = message,
            .file_path = file_path,
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
        // Deinit inner maps first
        var enum_iter = self.enum_types.valueIterator();
        while (enum_iter.next()) |inner_map| {
            inner_map.deinit();
        }
        self.enum_types.deinit();
        // Free generic struct type_param_names and type_param_bounds slices
        var gen_struct_iter = self.generic_struct_defs.valueIterator();
        while (gen_struct_iter.next()) |def| {
            self.allocator.free(def.type_param_names);
            self.allocator.free(def.type_param_bounds);
        }
        self.generic_struct_defs.deinit();
        // Free generic function type_param_names and type_param_bounds slices
        var gen_fn_iter = self.generic_fn_defs.valueIterator();
        while (gen_fn_iter.next()) |def| {
            self.allocator.free(def.type_param_names);
            self.allocator.free(def.type_param_bounds);
        }
        self.generic_fn_defs.deinit();
        self.instantiated_fns.deinit();
        self.type_param_substitutions.deinit();
        self.instantiated_structs.deinit();
        self.trait_defs.deinit();
        // Free impl method slices
        var impl_iter = self.impl_methods.valueIterator();
        while (impl_iter.next()) |methods| {
            self.allocator.free(methods.*);
        }
        self.impl_methods.deinit();
        self.comptime_evaluator.deinit();
        self.imported_namespaces.deinit();

        // Free function param defaults slices
        var defaults_iter = self.fn_param_defaults.valueIterator();
        while (defaults_iter.next()) |defaults| {
            self.allocator.free(defaults.*);
        }
        self.fn_param_defaults.deinit();

        // Free function param types slices
        var param_types_iter = self.fn_param_types.valueIterator();
        while (param_types_iter.next()) |types| {
            self.allocator.free(types.*);
        }
        self.fn_param_types.deinit();

        self.fn_return_types.deinit();

        // Free impl associated types slices
        var assoc_iter = self.impl_assoc_types.valueIterator();
        while (assoc_iter.next()) |bindings| {
            self.allocator.free(bindings.*);
        }
        self.impl_assoc_types.deinit();

        // Free owned strings (qualified method names, etc.)
        for (self.owned_strings.items) |s| {
            self.allocator.free(s);
        }
        self.owned_strings.deinit(self.allocator);

        // NOTE: allocated_types ownership was transferred to the Module in lowerProgram.
        // No cleanup needed here.

        // Clean up defer tracking
        self.defers.deinit(self.allocator);
        self.defer_scope_marks.deinit(self.allocator);

        for (self.warnings.items) |w| {
            self.allocator.free(w.message);
        }
        self.warnings.deinit(self.allocator);

        // Clean up narrowed member tracking
        self.narrowed_members.deinit();
        for (self.narrowed_member_scopes.items) |*scope_keys| {
            scope_keys.deinit();
        }
        self.narrowed_member_scopes.deinit(self.allocator);
    }

    // ============================================================================
    // Narrowed Member Management
    // ============================================================================

    /// Push a new scope for narrowed member tracking.
    /// Called when entering a block or if-body where members may be narrowed.
    pub fn pushNarrowedMemberScope(self: *Self) !void {
        try self.narrowed_member_scopes.append(self.allocator, std.StringHashMap(void).init(self.allocator));
    }

    /// Pop the current scope for narrowed member tracking.
    /// Removes all member narrowings added in this scope.
    pub fn popNarrowedMemberScope(self: *Self) void {
        if (self.narrowed_member_scopes.items.len == 0) return;
        // Manually pop since ArrayListUnmanaged doesn't have popOrNull
        const last_idx = self.narrowed_member_scopes.items.len - 1;
        var scope_keys = self.narrowed_member_scopes.items[last_idx];
        self.narrowed_member_scopes.items.len = last_idx;
        // Remove all keys that were added in this scope
        var iter = scope_keys.keyIterator();
        while (iter.next()) |key| {
            _ = self.narrowed_members.remove(key.*);
        }
        scope_keys.deinit();
    }

    /// Add a narrowed member path with its narrowed type.
    /// For example, after `if (l.current_scope != null)`, call this with
    /// path="l.current_scope" and the unwrapped type.
    pub fn addNarrowedMember(self: *Self, path: []const u8, narrowed_ty: ir.Type) !void {
        try self.narrowed_members.put(path, narrowed_ty);
        // Track this key in the current scope for cleanup on pop
        if (self.narrowed_member_scopes.items.len > 0) {
            var current_scope = &self.narrowed_member_scopes.items[self.narrowed_member_scopes.items.len - 1];
            try current_scope.put(path, {});
        }
    }

    /// Get the narrowed type for a member path, if it has been narrowed.
    pub fn getNarrowedMemberType(self: *const Self, path: []const u8) ?ir.Type {
        return self.narrowed_members.get(path);
    }

    /// Emit an instruction to the current block
    pub fn emit(self: *Self, inst: ir.Instruction) LowerError!void {
        const block = self.current_block orelse return LowerError.OutOfMemory;
        block.instructions.append(self.allocator, inst) catch return LowerError.OutOfMemory;
    }

    /// Process a statement for type/function definitions, recursively handling blocks.
    /// This is used to find definitions inside namespaces (which are represented as blocks).
    fn processStatementForDefinitions(self: *Self, stmt_idx: StmtIdx, pass: enum { struct_names, structs, traits, fn_sigs, impl_sigs, globals, impl_bodies, fn_bodies, tests }) LowerError!void {
        const tag = self.store.stmtTag(stmt_idx);

        // Handle blocks by recursively processing their contents
        if (tag == .block) {
            const data = self.store.stmtData(stmt_idx);
            // Block data: a = start index in extra_data, b = count
            const start = data.a;
            const count = data.b;
            for (0..count) |i| {
                const child_idx: StmtIdx = @enumFromInt(self.store.extra_data.items[start + i]);
                try self.processStatementForDefinitions(child_idx, pass);
            }
            return;
        }

        // Process based on the current pass
        switch (pass) {
            .struct_names => {
                // First pass: register struct/union/enum names for forward references
                if (tag == .struct_def) {
                    debug.print(.ir, "Phase 1: found struct_def, calling registerStructName", .{});
                    try self.registerStructName(stmt_idx);
                } else if (tag == .union_def) {
                    try self.registerUnionName(stmt_idx);
                } else if (tag == .enum_def) {
                    try self.registerEnumName(stmt_idx);
                }
            },
            .structs => {
                // Second pass: fill in struct/union/enum fields (all names now known)
                if (tag == .struct_def) {
                    try self.lowerStructDef(stmt_idx);
                } else if (tag == .union_def) {
                    try self.lowerUnionDef(stmt_idx);
                } else if (tag == .enum_def) {
                    try self.lowerEnumDef(stmt_idx);
                }
            },
            .traits => {
                if (tag == .trait_def) {
                    try self.lowerTraitDef(stmt_idx);
                }
            },
            .impl_sigs => {
                debug.print(.ir, "impl_sigs pass: checking stmt tag={s}", .{@tagName(tag)});
                if (tag == .impl_block) {
                    debug.print(.ir, "impl_sigs pass: collecting signatures", .{});
                    try self.collectImplSignatures(stmt_idx);
                }
            },
            .impl_bodies => {
                debug.print(.ir, "impl_bodies pass: checking stmt tag={s}", .{@tagName(tag)});
                if (tag == .impl_block) {
                    debug.print(.ir, "impl_bodies pass: lowering bodies", .{});
                    try self.lowerImplBodies(stmt_idx);
                }
            },
            .globals => {
                // Handle both let and const declarations at module level
                if (tag == .let_decl or tag == .const_decl) {
                    debug.print(.ir, "globals pass: lowering {s} decl", .{@tagName(tag)});
                    try lower_stmt.lowerGlobalLetDecl(self, stmt_idx);
                }
            },
            .fn_sigs => {
                if (tag == .fn_def) {
                    try self.collectFnSignature(stmt_idx);
                }
            },
            .fn_bodies => {
                if (tag == .fn_def) {
                    try self.lowerFnDef(stmt_idx);
                }
            },
            .tests => {
                if (tag == .test_def) {
                    try self.lowerTestDef(stmt_idx);
                }
            },
        }
    }

    /// Lower a program (list of top-level statements)
    pub fn lowerProgram(self: *Self, top_level: []const StmtIdx) LowerError!*ir.Module {
        log.debug("Lowering program with {d} top-level statements", .{top_level.len});
        debug.print(.ir, "Lowering program with {d} top-level statements", .{top_level.len});

        // Process import statements first - they define namespaces used throughout
        for (top_level) |stmt_idx| {
            const tag = self.store.stmtTag(stmt_idx);
            if (tag == .import_stmt) {
                try self.trackImport(stmt_idx);
            }
        }

        // Register top-level const declarations with the comptime evaluator
        // This must happen first so comptime conditions can reference constants
        for (top_level) |stmt_idx| {
            const tag = self.store.stmtTag(stmt_idx);
            if (tag == .const_decl) {
                const view = ast.views.ConstDeclView.from(self.store, stmt_idx);
                if (view.init != .null) {
                    // Try to evaluate the init expression at comptime
                    if (self.comptime_evaluator.evaluateExpression(view.init)) |value| {
                        self.comptime_evaluator.constants.put(view.name, value) catch {};
                    } else |_| {
                        // Couldn't evaluate at comptime - that's okay, not all const inits are comptime
                    }
                }
            }
        }

        // First pass: register struct/union/enum NAMES for forward references
        // This enables self-referential types like: struct Node { next: ?*Node }
        debug.print(.ir, "=== Starting struct_names pass (Phase 1) with {d} statements ===", .{top_level.len});
        for (top_level) |stmt_idx| {
            try self.processStatementForDefinitions(stmt_idx, .struct_names);
        }
        debug.print(.ir, "=== Finished struct_names pass (Phase 1) ===", .{});

        // Second pass: fill in struct/union/enum FIELDS (all type names now known)
        for (top_level) |stmt_idx| {
            try self.processStatementForDefinitions(stmt_idx, .structs);
        }

        // Second pass: collect trait definitions
        for (top_level) |stmt_idx| {
            try self.processStatementForDefinitions(stmt_idx, .traits);
        }

        // Third pass: collect function SIGNATURES (params/defaults only, no body)
        // This must happen before impl method bodies can call standalone functions
        for (top_level) |stmt_idx| {
            try self.processStatementForDefinitions(stmt_idx, .fn_sigs);
        }

        // Fourth pass: collect impl method SIGNATURES
        // This must happen before any method bodies are lowered so methods can call each other
        debug.print(.ir, "=== Starting impl_sigs pass with {d} statements ===", .{top_level.len});
        for (top_level) |stmt_idx| {
            try self.processStatementForDefinitions(stmt_idx, .impl_sigs);
        }
        debug.print(.ir, "=== Finished impl_sigs pass ===", .{});

        // Fifth pass: register top-level let declarations as globals
        // (These come from DBL common blocks and need to be visible in all functions)
        // Must run BEFORE function lowering so functions can reference globals
        for (top_level) |stmt_idx| {
            try self.processStatementForDefinitions(stmt_idx, .globals);
        }

        // Sixth pass: lower function BODIES
        for (top_level) |stmt_idx| {
            try self.processStatementForDefinitions(stmt_idx, .fn_bodies);
        }

        // Seventh pass: lower impl method BODIES
        debug.print(.ir, "=== Starting impl_bodies pass with {d} statements ===", .{top_level.len});
        for (top_level) |stmt_idx| {
            try self.processStatementForDefinitions(stmt_idx, .impl_bodies);
        }
        debug.print(.ir, "=== Finished impl_bodies pass ===", .{});

        // Eighth pass: lower test definitions
        for (top_level) |stmt_idx| {
            try self.processStatementForDefinitions(stmt_idx, .tests);
        }

        // Build VTables from trait implementations
        try self.buildVTables();

        // Export trait definitions to module for method index lookup
        try self.exportTraitDefs();

        // Transfer ownership of allocated types to the module
        // The module will free them in its deinit
        // Convert managed ArrayList to unmanaged by taking the internal slice
        self.module.allocated_types = .{
            .items = self.allocated_types.items,
            .capacity = self.allocated_types.capacity,
        };
        // Clear the Lowerer's list to prevent double-free
        self.allocated_types = .{};

        // Transfer ownership of allocated list types to the module
        self.module.allocated_list_types = .{
            .items = self.allocated_list_types.items,
            .capacity = self.allocated_list_types.capacity,
        };
        self.allocated_list_types = .{};

        return self.module;
    }

    // ========================================================================
    // Forward Declaration Registration (Phase 1)
    // These functions register type names before processing fields, enabling
    // self-referential types like: struct Node { next: ?*Node }
    // ========================================================================

    /// Register a struct name with an empty (forward-declared) type
    fn registerStructName(self: *Self, stmt_idx: StmtIdx) LowerError!void {
        const data = self.store.stmtData(stmt_idx);
        const name_id = data.getName();
        const name = self.strings.get(name_id);
        if (name.len == 0) {
            debug.print(.ir, "registerStructName: empty name, skipping", .{});
            return LowerError.UndefinedType;
        }

        // Skip if already registered (shouldn't happen, but be safe)
        if (self.struct_types.contains(name)) {
            debug.print(.ir, "registerStructName: {s} already registered, skipping", .{name});
            return;
        }

        // Check if this is a generic struct - skip registration, handle in lowerStructDef
        const extra_start = data.getParamsStart();
        const type_param_count = self.store.extra_data.items[extra_start.toInt() + 1];
        if (type_param_count > 0) {
            debug.print(.ir, "registerStructName: {s} has {d} type params, skipping (generic)", .{ name, type_param_count });
            return;
        }

        debug.print(.ir, "Registering struct name (forward): {s}", .{name});

        // Create an empty struct type as a forward declaration
        const struct_type = try self.allocator.create(ir.StructType);
        struct_type.* = .{
            .name = name,
            .fields = &[_]ir.StructType.Field{}, // Empty for now, filled in lowerStructDef
            .size = 0,
            .alignment = 8,
        };

        try self.struct_types.put(name, struct_type);
    }

    /// Register a union name with an empty (forward-declared) type
    fn registerUnionName(self: *Self, stmt_idx: StmtIdx) LowerError!void {
        const data = self.store.stmtData(stmt_idx);
        const name_id: StringId = @enumFromInt(data.a);
        const name = self.strings.get(name_id);
        if (name.len == 0) return LowerError.UndefinedType;

        // Skip if already registered
        if (self.union_types.contains(name)) return;

        debug.print(.ir, "Registering union name (forward): {s}", .{name});

        // Create an empty union type as a forward declaration
        const union_type = try self.allocator.create(ir.UnionType);
        union_type.* = .{
            .name = name,
            .variants = &[_]ir.UnionType.Variant{},
            .size = 0,
            .alignment = 8,
        };

        try self.union_types.put(name, union_type);
    }

    /// Register an enum name with an empty (forward-declared) type
    fn registerEnumName(self: *Self, stmt_idx: StmtIdx) LowerError!void {
        const data = self.store.stmtData(stmt_idx);
        const name_id = data.getName();
        const name = self.strings.get(name_id);
        if (name.len == 0) return LowerError.UndefinedType;

        // Skip if already registered
        if (self.enum_types.contains(name)) return;

        debug.print(.ir, "Registering enum name (forward): {s}", .{name});

        // Create an empty variant map as a forward declaration
        // Will be populated by lowerEnumDef
        const empty_variants = std.StringHashMap(i64).init(self.allocator);
        try self.enum_types.put(name, empty_variants);
    }

    // ========================================================================
    // Type Definition Lowering (Phase 2)
    // These functions fill in the fields of forward-declared types
    // ========================================================================

    /// Lower a struct definition
    fn lowerStructDef(self: *Self, stmt_idx: StmtIdx) LowerError!void {
        const data = self.store.stmtData(stmt_idx);
        const loc = self.store.stmtLoc(stmt_idx);
        self.current_loc = loc; // Set location for error messages

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

            // Extract type parameter names and bounds
            // storeTypeParams format: [count, name0, bound0, name1, bound1, ...]
            var type_param_names: std.ArrayListUnmanaged(StringId) = .{};
            defer type_param_names.deinit(self.allocator);
            var type_param_bounds: std.ArrayListUnmanaged(ast.TypeIdx) = .{};
            defer type_param_bounds.deinit(self.allocator);

            for (0..type_param_count) |i| {
                // Names are at positions 1, 3, 5, ... (skip count at 0)
                const name_idx = type_params_start + 1 + @as(u32, @intCast(i)) * 2;
                const bound_idx = name_idx + 1;
                const param_name_id: StringId = @enumFromInt(self.store.extra_data.items[name_idx]);
                const param_bound: ast.TypeIdx = @enumFromInt(self.store.extra_data.items[bound_idx]);
                try type_param_names.append(self.allocator, param_name_id);
                try type_param_bounds.append(self.allocator, param_bound);
            }

            const generic_def = GenericDef{
                .stmt_idx = stmt_idx,
                .type_param_count = @intCast(type_param_count),
                .type_param_names = try type_param_names.toOwnedSlice(self.allocator),
                .type_param_bounds = try type_param_bounds.toOwnedSlice(self.allocator),
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

        const owned_fields = try fields.toOwnedSlice(self.allocator);

        // Check if we have a forward-declared struct to update in-place
        // This is critical for self-referential types: fields like ?*Node already
        // point to the forward-declared struct, so we must update it, not replace it
        if (self.struct_types.get(name)) |existing_const| {
            // Cast away const to update in-place (the memory was allocated mutable)
            const existing: *ir.StructType = @constCast(existing_const);
            existing.fields = owned_fields;
            existing.size = current_offset;
            debug.print(.ir, "  struct '{s}' updated in-place, total size: {d}", .{ name, current_offset });
            try self.module.addStruct(existing);
        } else {
            // No forward declaration - create new struct type
            const struct_type = try self.allocator.create(ir.StructType);
            struct_type.* = .{
                .name = name,
                .fields = owned_fields,
                .size = current_offset,
                .alignment = 8,
            };
            debug.print(.ir, "  struct '{s}' total size: {d}", .{ name, current_offset });

            try self.module.addStruct(struct_type);
            try self.struct_types.put(name, struct_type);
        }
    }

    /// Lower a union definition
    fn lowerUnionDef(self: *Self, stmt_idx: StmtIdx) LowerError!void {
        const data = self.store.stmtData(stmt_idx);
        const loc = self.store.stmtLoc(stmt_idx);
        self.current_loc = loc; // Set location for error messages

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

        const owned_variants = try variants.toOwnedSlice(self.allocator);

        // Check if we have a forward-declared union to update in-place
        if (self.union_types.get(name)) |existing_const| {
            const existing: *ir.UnionType = @constCast(existing_const);
            existing.variants = owned_variants;
            existing.size = max_size;
            debug.print(.ir, "  union '{s}' updated in-place, total size: {d}", .{ name, max_size });
            try self.module.addUnion(existing);
        } else {
            const union_type = try self.allocator.create(ir.UnionType);
            union_type.* = .{
                .name = name,
                .variants = owned_variants,
                .size = max_size,
                .alignment = max_alignment,
            };
            debug.print(.ir, "  union '{s}' total size: {d}", .{ name, max_size });

            try self.module.addUnion(union_type);
            try self.union_types.put(name, union_type);
        }
    }

    /// Lower an enum definition - extract variant names, values, and payloads (for sum types)
    /// New format in extra_data: [count, (name, value, payload_info)*]
    /// Each variant has 3 u32s: name, value, payload_info
    /// payload_info = (kind: 2 bits) | (field_count: 8 bits) | (fields_start: 22 bits)
    fn lowerEnumDef(self: *Self, stmt_idx: StmtIdx) LowerError!void {
        const loc = self.store.stmtLoc(stmt_idx);
        self.current_loc = loc; // Set location for error messages

        // Use the EnumDefView for structured access
        const view = EnumDefView.from(self.store, stmt_idx);
        const name = self.strings.get(view.name);
        if (name.len == 0) return LowerError.UndefinedType;

        log.debug("Lowering enum: {s}", .{name});
        debug.print(.ir, "Lowering enum: {s}", .{name});

        // Create inner map for this enum's variants (for backwards compatibility)
        var variants = std.StringHashMap(i64).init(self.allocator);
        errdefer variants.deinit();

        // Build extended variant definitions for sum type support
        var variant_defs_list: std.ArrayListUnmanaged(ir.Module.EnumVariantDef) = .empty;
        errdefer variant_defs_list.deinit(self.allocator);

        var is_sum_type = false;

        // Iterate through variants using the view
        var it = view.variantIterator(self.store);
        while (it.next()) |variant| {
            const variant_name = self.strings.get(variant.name);
            if (variant_name.len == 0) continue;

            try variants.put(variant_name, variant.value);

            // Convert AST payload kind to IR payload kind
            const ir_payload_kind: ir.Module.VariantPayloadKind = switch (variant.payload_kind) {
                .none => .none,
                .tuple => .tuple,
                .struct_like => .struct_like,
            };

            // Track if this is a sum type
            if (variant.payload_kind != .none) {
                is_sum_type = true;
            }

            // Build payload fields for this variant
            var payload_fields_list: std.ArrayListUnmanaged(ir.Module.VariantPayloadField) = .empty;
            errdefer payload_fields_list.deinit(self.allocator);

            if (variant.payload_field_count > 0) {
                var field_it = view.payloadFieldIterator(self.store, variant);
                while (field_it.next()) |field| {
                    const field_name: ?[]const u8 = if (field.name.isNull()) null else self.strings.get(field.name);
                    const type_tag = self.store.typeTag(field.type_idx);

                    // Get the type name from the type
                    const type_name = self.typeTagToName(type_tag, field.type_idx);

                    try payload_fields_list.append(self.allocator, .{
                        .name = field_name,
                        .type_name = type_name,
                    });
                }
            }

            const payload_fields = try payload_fields_list.toOwnedSlice(self.allocator);

            try variant_defs_list.append(self.allocator, .{
                .name = variant_name,
                .value = variant.value,
                .payload_kind = ir_payload_kind,
                .payload_fields = payload_fields,
            });

            if (variant.payload_kind != .none) {
                debug.print(.ir, "  variant '{s}' = {d} (payload: {s}, {d} fields)", .{
                    variant_name,
                    variant.value,
                    @tagName(variant.payload_kind),
                    variant.payload_field_count,
                });
            } else {
                debug.print(.ir, "  variant '{s}' = {d}", .{ variant_name, variant.value });
            }
        }

        const variant_defs = try variant_defs_list.toOwnedSlice(self.allocator);

        // Check if we have a forward-declared enum to clean up
        if (self.enum_types.fetchRemove(name)) |kv| {
            // Clean up the old empty map
            var old_variants = kv.value;
            old_variants.deinit();
        }

        try self.enum_types.put(name, variants);

        // Also store in module for cross-package export
        // Clone the variants map for the module (module owns its copy)
        var module_variants = std.StringHashMap(i64).init(self.allocator);
        var var_it = variants.iterator();
        while (var_it.next()) |entry| {
            module_variants.put(entry.key_ptr.*, entry.value_ptr.*) catch {};
        }
        self.module.enums.put(self.allocator, name, .{
            .name = name,
            .variants = module_variants,
            .variant_defs = variant_defs,
            .is_sum_type = is_sum_type,
        }) catch {};
    }

    /// Convert a type tag to a type name string
    fn typeTagToName(self: *Self, tag: TypeTag, type_idx: TypeIdx) []const u8 {
        return switch (tag) {
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
            .bool => "bool",
            .void => "void",
            .string => "string",
            .named => blk: {
                const data = self.store.typeData(type_idx);
                const name_id: StringId = @enumFromInt(data.a);
                break :blk self.strings.get(name_id);
            },
            else => "unknown",
        };
    }

    /// Instantiate a generic struct with concrete type arguments
    pub fn instantiateGenericStruct(self: *Self, base_name: []const u8, type_args: []const ir.Type) LowerError!?*const ir.StructType {
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

        // Validate type bounds
        for (generic_def.type_param_bounds, type_args, 0..) |bound_idx, arg_type, i| {
            if (!bound_idx.isNull()) {
                // There's a bound - check if the type implements the required trait
                const bound_name = self.resolveTypeIdxName(bound_idx);
                if (bound_name) |trait_name| {
                    if (!self.typeImplementsTrait(arg_type, trait_name)) {
                        const param_name = self.strings.get(generic_def.type_param_names[i]);
                        debug.print(.ir, "Type bound violation: {s} does not implement {s}", .{ @tagName(arg_type), trait_name });
                        log.err("Type '{s}' does not implement trait '{s}' required for type parameter '{s}'", .{ @tagName(arg_type), trait_name, param_name });
                        return null;
                    }
                }
            }
        }

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

    /// Instantiate a generic function with concrete type arguments
    /// Returns the mangled function name to call, or null if not a generic function
    pub fn instantiateGenericFn(self: *Self, base_name: []const u8, type_args: []const ir.Type) LowerError!?[]const u8 {
        // Check if we have a generic definition for this name
        const generic_def = self.generic_fn_defs.get(base_name) orelse return null;

        // Verify argument count
        if (type_args.len != generic_def.type_param_count) {
            debug.print(.ir, "Generic fn {s}: expected {d} type args, got {d}", .{ base_name, generic_def.type_param_count, type_args.len });
            return null;
        }

        // Generate a mangled name for this instantiation
        var mangled_name_buf: [256]u8 = undefined;
        var stream = std.io.fixedBufferStream(&mangled_name_buf);
        const writer = stream.writer();
        writer.writeAll(base_name) catch return null;
        writer.writeAll("<") catch return null;
        for (type_args, 0..) |arg, i| {
            if (i > 0) writer.writeAll(", ") catch return null;
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
                // Array of u8 is effectively a string type
                .array => |arr| {
                    if (arr.element.* == .u8) {
                        writer.writeAll("string") catch return null;
                    } else {
                        writer.writeAll("array") catch return null;
                    }
                },
                .@"struct" => |st| writer.writeAll(st.name) catch return null,
                .ptr => |p| {
                    writer.writeAll("*") catch return null;
                    if (p.* == .@"struct") {
                        writer.writeAll(p.@"struct".name) catch return null;
                    } else {
                        writer.writeAll("?") catch return null;
                    }
                },
                else => writer.writeAll("?") catch return null,
            }
        }
        writer.writeAll(">") catch return null;

        const mangled_name = stream.getWritten();

        // Check if we already instantiated this
        if (self.instantiated_fns.get(mangled_name)) |_| {
            // Already instantiated, return a dupe of the name
            return self.allocator.dupe(u8, mangled_name) catch return LowerError.OutOfMemory;
        }

        debug.print(.ir, "Instantiating generic function: {s}", .{mangled_name});

        // Validate type bounds
        for (generic_def.type_param_bounds, type_args, 0..) |bound_idx, arg_type, i| {
            if (!bound_idx.isNull()) {
                // There's a bound - check if the type implements the required trait
                const bound_name = self.resolveTypeIdxName(bound_idx);
                if (bound_name) |trait_name| {
                    if (!self.typeImplementsTrait(arg_type, trait_name)) {
                        const param_name = self.strings.get(generic_def.type_param_names[i]);
                        debug.print(.ir, "Type bound violation: {s} does not implement {s}", .{ @tagName(arg_type), trait_name });
                        log.err("Type '{s}' does not implement trait '{s}' required for type parameter '{s}'", .{ @tagName(arg_type), trait_name, param_name });
                        return LowerError.TypeMismatch;
                    }
                }
            }
        }

        // Set up type parameter substitutions
        self.type_param_substitutions.clearRetainingCapacity();
        for (generic_def.type_param_names, type_args) |param_name_id, arg_type| {
            const param_name = self.strings.get(param_name_id);
            self.type_param_substitutions.put(param_name, arg_type) catch return LowerError.OutOfMemory;
        }

        // Re-lower the function with substitutions active
        const stmt_idx = generic_def.stmt_idx;
        const data = self.store.stmtData(stmt_idx);

        // Allocate the mangled name
        const name_copy = self.allocator.dupe(u8, mangled_name) catch return LowerError.OutOfMemory;

        // Parse params from extra data: [param_count, type_param_count, type_params..., params..., return_type, body]
        const extra_start = data.getParamsStart();
        const param_count = self.store.getExtra(extra_start);
        const type_param_count = self.store.extra_data.items[extra_start.toInt() + 1];

        var params: std.ArrayListUnmanaged(ir.FunctionType.Param) = .{};
        defer params.deinit(self.allocator);

        // Skip past type params (each type param is 2 values: name, bound)
        var extra_idx = extra_start.toInt() + 2 + type_param_count * 2;
        for (0..param_count) |_| {
            const param_name_id: StringId = @enumFromInt(self.store.extra_data.items[extra_idx]);
            const param_type_idx: TypeIdx = @enumFromInt(self.store.extra_data.items[extra_idx + 1]);
            const param_direction_raw: u32 = self.store.extra_data.items[extra_idx + 2];
            extra_idx += 4; // 4 values per param

            const param_name = self.strings.get(param_name_id);
            if (param_name.len == 0) continue;

            // Lower type with substitutions active
            const param_type = try self.lowerTypeIdx(param_type_idx);

            // NOTE: We preserve the declared type (*struct(T)) for type checking.
            // The is_ref flag indicates bytecode should use copy-back semantics.
            // Only DBL-specific types (implied_decimal, fixed_decimal, array of u8) need this.
            const is_ref = param_direction_raw != 0 or ir.isDblRefType(param_type);

            try params.append(self.allocator, .{
                .name = param_name,
                .ty = param_type,
                .is_ref = is_ref,
            });
        }

        const return_type_idx: TypeIdx = @enumFromInt(self.store.extra_data.items[extra_idx]);
        const body_idx: StmtIdx = @enumFromInt(self.store.extra_data.items[extra_idx + 1]);

        // Lower return type with substitutions
        const return_type = try self.lowerTypeIdx(return_type_idx);

        // Clear substitutions before lowering body (we set them per-identifier lookup)
        // Actually keep them for the body lowering
        // self.type_param_substitutions.clearRetainingCapacity();

        // Create function type
        const func_type = ir.FunctionType{
            .params = try params.toOwnedSlice(self.allocator),
            .return_type = return_type,
            .is_variadic = false,
        };

        // Create function
        const func = try ir.Function.init(self.allocator, name_copy, func_type);

        // Save current context
        const saved_func = self.current_func;
        const saved_block = self.current_block;
        // Save current scope - reset() orphans but doesn't destroy scopes
        const saved_scope = self.scopes.current;

        self.current_func = func;
        self.current_block = func.entry;

        // Create fresh scope for the instantiated function
        self.scopes.reset();

        // Add parameters as local variables (same as lowerFnDef)
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
            self.scopes.put(param.name, alloca_result) catch return LowerError.OutOfMemory;
        }

        // Lower the body
        const body_tag = self.store.stmtTag(body_idx);
        if (body_tag == .block) {
            const body_data = self.store.stmtData(body_idx);
            try lower_stmt.lowerBlock(self, body_data);
        } else {
            try self.lowerStatement(body_idx);
        }

        // Add implicit return if not terminated
        if (!self.current_block.?.isTerminated()) {
            try self.emit(.{ .return_ = null });
        }

        // Clear substitutions
        self.type_param_substitutions.clearRetainingCapacity();

        // Restore context
        self.current_func = saved_func;
        self.current_block = saved_block;
        // Restore scope to continue lowering calling function
        self.scopes.current = saved_scope;

        // Add function to module
        try self.module.addFunction(func);

        // Store param types and return type for the instantiated function
        const param_types_slice = self.allocator.alloc(ir.Type, func_type.params.len) catch return LowerError.OutOfMemory;
        for (func_type.params, 0..) |p, i| {
            param_types_slice[i] = p.ty;
        }
        self.fn_param_types.put(name_copy, param_types_slice) catch return LowerError.OutOfMemory;
        self.fn_return_types.put(name_copy, return_type) catch return LowerError.OutOfMemory;

        // Mark as instantiated
        self.instantiated_fns.put(name_copy, {}) catch return LowerError.OutOfMemory;

        debug.print(.ir, "Finished instantiating generic function: {s} -> {s}", .{ base_name, name_copy });

        return name_copy;
    }

    /// Lower a trait definition - stores trait method signatures and associated types for later lookup
    fn lowerTraitDef(self: *Self, stmt_idx: StmtIdx) LowerError!void {
        const data = self.store.stmtData(stmt_idx);
        const loc = self.store.stmtLoc(stmt_idx);
        self.current_loc = loc; // Set location for error messages

        const name_id: StringId = @enumFromInt(data.a);
        const trait_name = self.strings.get(name_id);
        const extra_start = data.b;

        debug.print(.ir, "Lowering trait: {s}", .{trait_name});

        // Parse extra_data: [method_count, type_param_count, type_params_start, assoc_type_count, ...assoc_types..., ...method_data...]
        const method_count = self.store.extra_data.items[extra_start];
        const type_param_count: u16 = @intCast(self.store.extra_data.items[extra_start + 1]);
        // const type_params_start = self.store.extra_data.items[extra_start + 2]; // unused for now
        const assoc_type_count = self.store.extra_data.items[extra_start + 3];

        var offset: u32 = 4; // Skip method_count, type_param_count, type_params_start, assoc_type_count

        // Parse associated types: [name, bound] pairs
        var assoc_types: std.ArrayListUnmanaged(AssociatedTypeDef) = .{};
        defer assoc_types.deinit(self.allocator);

        for (0..assoc_type_count) |_| {
            const assoc_name_id: StringId = @enumFromInt(self.store.extra_data.items[extra_start + offset]);
            const assoc_name = self.strings.get(assoc_name_id);
            offset += 1;

            const bound_type_idx: TypeIdx = @enumFromInt(self.store.extra_data.items[extra_start + offset]);
            offset += 1;

            try assoc_types.append(self.allocator, .{
                .name = assoc_name,
                .bound_type_idx = bound_type_idx,
            });

            debug.print(.ir, "  Associated type: {s}", .{assoc_name});
        }

        // Parse method signatures
        // Layout: [method_name, param_count, return_type, has_default, default_body, param_quads...] for each method
        var methods: std.ArrayListUnmanaged(TraitMethodSig) = .{};
        defer methods.deinit(self.allocator);

        for (0..method_count) |_| {
            // Read method header: [method_name, param_count, return_type, has_default, default_body]
            const method_name_id: StringId = @enumFromInt(self.store.extra_data.items[extra_start + offset]);
            const method_name = self.strings.get(method_name_id);
            offset += 1;

            const param_count = self.store.extra_data.items[extra_start + offset];
            offset += 1;

            const return_type_idx: TypeIdx = @enumFromInt(self.store.extra_data.items[extra_start + offset]);
            offset += 1;

            const has_default = self.store.extra_data.items[extra_start + offset] != 0;
            offset += 1;

            const default_body_raw = self.store.extra_data.items[extra_start + offset];
            const default_body_idx: ?StmtIdx = if (has_default) @enumFromInt(default_body_raw) else null;
            offset += 1;

            // Store raw param data for default impl generation (don't resolve types yet)
            var raw_param_data: ?[]u32 = null;
            if (has_default and param_count > 0) {
                var param_data: std.ArrayListUnmanaged(u32) = .{};
                defer param_data.deinit(self.allocator);

                for (0..param_count) |_| {
                    // Store name and type as raw u32 values
                    try param_data.append(self.allocator, self.store.extra_data.items[extra_start + offset]); // name
                    try param_data.append(self.allocator, self.store.extra_data.items[extra_start + offset + 1]); // type
                    offset += 4; // Skip is_ref and default too
                }
                raw_param_data = try param_data.toOwnedSlice(self.allocator);
            } else {
                // Just skip the param data
                offset += param_count * 4;
            }

            try methods.append(self.allocator, .{
                .name = method_name,
                .param_count = param_count,
                .return_type_idx = return_type_idx,
                .has_default = has_default,
                .default_body_stmt_idx = default_body_idx,
                .raw_param_data = raw_param_data,
            });

            if (has_default) {
                debug.print(.ir, "  Method: {s}({d} params) [has default]", .{ method_name, param_count });
            } else {
                debug.print(.ir, "  Method: {s}({d} params)", .{ method_name, param_count });
            }
        }

        // Store the trait definition with associated types
        try self.trait_defs.put(trait_name, .{
            .name = trait_name,
            .type_param_count = type_param_count,
            .methods = try methods.toOwnedSlice(self.allocator),
            .associated_types = try assoc_types.toOwnedSlice(self.allocator),
        });
    }

    /// Lower an impl block - registers method implementations and associated type bindings for a trait+type pair
    fn lowerImplBlock(self: *Self, stmt_idx: StmtIdx) LowerError!void {
        const data = self.store.stmtData(stmt_idx);
        const extra_start = data.b;

        // Parse extra_data: [method_count, trait_type, target_type, assoc_binding_count, ...assoc_bindings..., method_stmt_idx...]
        const method_count = self.store.extra_data.items[extra_start];
        const trait_type_idx: TypeIdx = @enumFromInt(self.store.extra_data.items[extra_start + 1]);
        const target_type_idx: TypeIdx = @enumFromInt(self.store.extra_data.items[extra_start + 2]);
        const assoc_binding_count = self.store.extra_data.items[extra_start + 3];

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

        debug.print(.ir, "Lowering impl {s} for {s} with {d} methods, {d} associated types", .{ trait_name, target_name, method_count, assoc_binding_count });

        // Parse associated type bindings: [name, concrete_type] pairs
        var offset: u32 = 4; // Skip method_count, trait_type, target_type, assoc_binding_count

        var assoc_bindings: std.ArrayListUnmanaged(AssociatedTypeBinding) = .{};
        defer assoc_bindings.deinit(self.allocator);

        for (0..assoc_binding_count) |_| {
            const assoc_name_id: StringId = @enumFromInt(self.store.extra_data.items[extra_start + offset]);
            const assoc_name = self.strings.get(assoc_name_id);
            offset += 1;

            const concrete_type_idx: TypeIdx = @enumFromInt(self.store.extra_data.items[extra_start + offset]);
            offset += 1;

            try assoc_bindings.append(self.allocator, .{
                .name = assoc_name,
                .concrete_type_idx = concrete_type_idx,
            });

            debug.print(.ir, "  Associated type binding: {s}", .{assoc_name});
        }

        // Store associated type bindings for this impl (for later type resolution)
        const impl_key = ImplKey{ .trait_name = trait_name, .type_name = target_name };
        try self.impl_assoc_types.put(impl_key, try assoc_bindings.toOwnedSlice(self.allocator));

        // Set current impl context for Self.Item resolution during method lowering
        self.current_impl_key = impl_key;
        defer self.current_impl_key = null;

        // Collect method implementations
        var methods: std.ArrayListUnmanaged(MethodImpl) = .{};
        defer methods.deinit(self.allocator);

        for (0..method_count) |i| {
            const method_stmt_raw = self.store.extra_data.items[extra_start + offset + i];
            const method_stmt_idx: StmtIdx = @enumFromInt(method_stmt_raw);

            // Get the method name from the fn_def statement
            const fn_data = self.store.stmtData(method_stmt_idx);
            const fn_name_id = fn_data.getName();
            const fn_name = self.strings.get(fn_name_id);

            // Create qualified function name: TypeName.method_name
            var qualified_name_buf: [256]u8 = undefined;
            const qualified_name = std.fmt.bufPrint(&qualified_name_buf, "{s}.{s}", .{ target_name, fn_name }) catch {
                return LowerError.OutOfMemory;
            };
            const qualified_name_owned = try self.allocator.dupe(u8, qualified_name);

            debug.print(.ir, "  Impl method: {s} -> {s}", .{ fn_name, qualified_name_owned });

            try methods.append(self.allocator, .{
                .method_name = fn_name,
                .fn_stmt_idx = method_stmt_idx,
            });

            // Lower the method with the qualified name
            try self.lowerImplMethod(method_stmt_idx, qualified_name_owned);
        }

        // Validate trait requirements (associated types and methods)
        const trait_def = self.trait_defs.get(trait_name);
        if (trait_def) |td| {
            // Check for missing associated types
            for (td.associated_types) |assoc_type_def| {
                var found = false;
                for (assoc_bindings.items) |binding| {
                    if (std.mem.eql(u8, binding.name, assoc_type_def.name)) {
                        found = true;
                        break;
                    }
                }
                if (!found) {
                    debug.print(.ir, "Warning: impl {s} for {s} is missing associated type '{s}'", .{ trait_name, target_name, assoc_type_def.name });
                }
            }

            // Build a set of implemented method names
            var implemented = std.StringHashMap(void).init(self.allocator);
            defer implemented.deinit();
            for (methods.items) |m| {
                try implemented.put(m.method_name, {});
            }

            // Check each trait method for defaults
            for (td.methods) |trait_method| {
                if (!implemented.contains(trait_method.name)) {
                    // Method not implemented - check for default
                    if (trait_method.has_default and trait_method.default_body_stmt_idx != null) {
                        debug.print(.ir, "  Using default impl for {s}.{s}", .{ target_name, trait_method.name });

                        // Generate the qualified function name
                        var qualified_name_buf: [256]u8 = undefined;
                        const qualified_name = std.fmt.bufPrint(&qualified_name_buf, "{s}.{s}", .{ target_name, trait_method.name }) catch {
                            return LowerError.OutOfMemory;
                        };
                        const qualified_name_owned = try self.allocator.dupe(u8, qualified_name);

                        // Lower the default method implementation
                        try self.lowerDefaultTraitMethod(
                            trait_method,
                            target_name,
                            qualified_name_owned,
                        );

                        // Add to the methods list
                        try methods.append(self.allocator, .{
                            .method_name = trait_method.name,
                            .fn_stmt_idx = .null, // No AST stmt - generated from default
                        });
                    } else {
                        // Missing required method with no default
                        debug.print(.ir, "Warning: missing impl for {s}.{s} (no default)", .{ target_name, trait_method.name });
                    }
                }
            }
        }

        // Store the implementation mapping
        const key = ImplKey{
            .trait_name = trait_name,
            .type_name = target_name,
        };
        try self.impl_methods.put(key, try methods.toOwnedSlice(self.allocator));
    }

    /// Collect impl method SIGNATURES only (no body lowering)
    /// This runs before body lowering so methods can call each other and standalone functions
    fn collectImplSignatures(self: *Self, stmt_idx: StmtIdx) LowerError!void {
        const data = self.store.stmtData(stmt_idx);
        const extra_start = data.b;

        // Parse extra_data: [method_count, trait_type, target_type, assoc_binding_count, ...assoc_bindings..., method_stmt_idx...]
        const method_count = self.store.extra_data.items[extra_start];
        const trait_type_idx: TypeIdx = @enumFromInt(self.store.extra_data.items[extra_start + 1]);
        const target_type_idx: TypeIdx = @enumFromInt(self.store.extra_data.items[extra_start + 2]);
        const assoc_binding_count = self.store.extra_data.items[extra_start + 3];

        // Get trait and target type names
        const trait_name = self.getTypeName(trait_type_idx) orelse {
            debug.print(.ir, "Warning: impl block has invalid trait type", .{});
            return;
        };

        // target_type is optional (for inherent impls, it's null)
        const target_name = if (target_type_idx != .null)
            self.getTypeName(target_type_idx) orelse trait_name
        else
            trait_name;

        debug.print(.ir, "Collecting impl {s} for {s} signatures: {d} methods, {d} associated types", .{ trait_name, target_name, method_count, assoc_binding_count });

        // Parse associated type bindings: [name, concrete_type] pairs
        var offset: u32 = 4; // Skip method_count, trait_type, target_type, assoc_binding_count

        var assoc_bindings: std.ArrayListUnmanaged(AssociatedTypeBinding) = .{};
        defer assoc_bindings.deinit(self.allocator);

        for (0..assoc_binding_count) |_| {
            const assoc_name_id: StringId = @enumFromInt(self.store.extra_data.items[extra_start + offset]);
            const assoc_name = self.strings.get(assoc_name_id);
            offset += 1;

            const concrete_type_idx: TypeIdx = @enumFromInt(self.store.extra_data.items[extra_start + offset]);
            offset += 1;

            try assoc_bindings.append(self.allocator, .{
                .name = assoc_name,
                .concrete_type_idx = concrete_type_idx,
            });

            debug.print(.ir, "  Associated type binding: {s}", .{assoc_name});
        }

        // Store associated type bindings for this impl (for later type resolution)
        const impl_key = ImplKey{ .trait_name = trait_name, .type_name = target_name };
        try self.impl_assoc_types.put(impl_key, try assoc_bindings.toOwnedSlice(self.allocator));

        // Set current impl context for Self.Item resolution during signature collection
        self.current_impl_key = impl_key;
        defer self.current_impl_key = null;

        // Collect method signatures (no body lowering)
        var methods: std.ArrayListUnmanaged(MethodImpl) = .{};
        defer methods.deinit(self.allocator);

        for (0..method_count) |i| {
            const method_stmt_raw = self.store.extra_data.items[extra_start + offset + i];
            const method_stmt_idx: StmtIdx = @enumFromInt(method_stmt_raw);

            // Get the method name from the fn_def statement
            const fn_data = self.store.stmtData(method_stmt_idx);
            const fn_name_id = fn_data.getName();
            const fn_name = self.strings.get(fn_name_id);

            // Create qualified function name: TypeName.method_name
            var qualified_name_buf: [256]u8 = undefined;
            const qualified_name = std.fmt.bufPrint(&qualified_name_buf, "{s}.{s}", .{ target_name, fn_name }) catch {
                return LowerError.OutOfMemory;
            };
            const qualified_name_owned = try self.allocator.dupe(u8, qualified_name);
            self.owned_strings.append(self.allocator, qualified_name_owned) catch return LowerError.OutOfMemory;

            debug.print(.ir, "  Collecting signature: {s} -> {s}", .{ fn_name, qualified_name_owned });

            // Collect the method signature only (no body)
            try self.collectImplMethodSignature(method_stmt_idx, qualified_name_owned);

            try methods.append(self.allocator, .{
                .method_name = fn_name,
                .fn_stmt_idx = method_stmt_idx,
            });
        }

        // Store the implementation mapping (for method lookup)
        const key = ImplKey{
            .trait_name = trait_name,
            .type_name = target_name,
        };
        try self.impl_methods.put(key, try methods.toOwnedSlice(self.allocator));
    }

    /// Lower impl method BODIES only (signatures already collected)
    fn lowerImplBodies(self: *Self, stmt_idx: StmtIdx) LowerError!void {
        const data = self.store.stmtData(stmt_idx);
        const extra_start = data.b;

        // Parse extra_data: [method_count, trait_type, target_type, assoc_binding_count, ...assoc_bindings..., method_stmt_idx...]
        const method_count = self.store.extra_data.items[extra_start];
        const trait_type_idx: TypeIdx = @enumFromInt(self.store.extra_data.items[extra_start + 1]);
        const target_type_idx: TypeIdx = @enumFromInt(self.store.extra_data.items[extra_start + 2]);
        const assoc_binding_count = self.store.extra_data.items[extra_start + 3];

        // Get trait and target type names
        const trait_name = self.getTypeName(trait_type_idx) orelse {
            return;
        };

        const target_name = if (target_type_idx != .null)
            self.getTypeName(target_type_idx) orelse trait_name
        else
            trait_name;

        debug.print(.ir, "Lowering impl {s} for {s} bodies: {d} methods", .{ trait_name, target_name, method_count });

        // Skip associated type bindings
        const offset: u32 = 4 + assoc_binding_count * 2;

        // Set current impl context
        const impl_key = ImplKey{ .trait_name = trait_name, .type_name = target_name };
        self.current_impl_key = impl_key;
        defer self.current_impl_key = null;

        // Lower method bodies
        for (0..method_count) |i| {
            const method_stmt_raw = self.store.extra_data.items[extra_start + offset + i];
            const method_stmt_idx: StmtIdx = @enumFromInt(method_stmt_raw);

            const fn_data = self.store.stmtData(method_stmt_idx);
            const fn_name_id = fn_data.getName();
            const fn_name = self.strings.get(fn_name_id);

            var qualified_name_buf: [256]u8 = undefined;
            const qualified_name = std.fmt.bufPrint(&qualified_name_buf, "{s}.{s}", .{ target_name, fn_name }) catch {
                return LowerError.OutOfMemory;
            };
            const qualified_name_owned = try self.allocator.dupe(u8, qualified_name);
            self.owned_strings.append(self.allocator, qualified_name_owned) catch return LowerError.OutOfMemory;

            debug.print(.ir, "  Lowering body: {s}", .{qualified_name_owned});

            // Lower the method body (signature already registered)
            try self.lowerImplMethodBody(method_stmt_idx, qualified_name_owned);
        }

        // Handle default trait method implementations
        const trait_def = self.trait_defs.get(trait_name);
        if (trait_def) |td| {
            // Get the implemented methods from impl_methods
            if (self.impl_methods.get(impl_key)) |methods| {
                // Build a set of implemented method names
                var implemented = std.StringHashMap(void).init(self.allocator);
                defer implemented.deinit();
                for (methods) |m| {
                    try implemented.put(m.method_name, {});
                }

                // Check each trait method for defaults
                for (td.methods) |trait_method| {
                    if (!implemented.contains(trait_method.name)) {
                        // Method not implemented - check for default
                        if (trait_method.has_default and trait_method.default_body_stmt_idx != null) {
                            debug.print(.ir, "  Using default impl for {s}.{s}", .{ target_name, trait_method.name });

                            // Generate the qualified function name
                            var qualified_name_buf: [256]u8 = undefined;
                            const qualified_name = std.fmt.bufPrint(&qualified_name_buf, "{s}.{s}", .{ target_name, trait_method.name }) catch {
                                return LowerError.OutOfMemory;
                            };
                            const qualified_name_owned = try self.allocator.dupe(u8, qualified_name);

                            // Lower the default method implementation
                            try self.lowerDefaultTraitMethod(
                                trait_method,
                                target_name,
                                qualified_name_owned,
                            );
                        } else {
                            debug.print(.ir, "Warning: missing impl for {s}.{s} (no default)", .{ target_name, trait_method.name });
                        }
                    }
                }
            }
        }
    }

    /// Collect signature for an impl method (no body lowering)
    fn collectImplMethodSignature(self: *Self, stmt_idx: StmtIdx, qualified_name: []const u8) LowerError!void {
        const data = self.store.stmtData(stmt_idx);

        debug.print(.ir, "Collecting impl method signature: {s}", .{qualified_name});

        // Parse extra data: [param_count, type_param_count, type_params..., param1_name, param1_type, ..., return_type, body]
        const extra_start = data.getParamsStart();
        const param_count = self.store.getExtra(extra_start);
        const type_param_count = self.store.extra_data.items[extra_start.toInt() + 1];

        var params: std.ArrayListUnmanaged(ir.FunctionType.Param) = .{};
        defer params.deinit(self.allocator);

        // Skip past type params: each type param is 2 values (name, bound)
        var extra_idx = extra_start.toInt() + 2 + type_param_count * 2;
        for (0..param_count) |_| {
            const param_name_id: StringId = @enumFromInt(self.store.extra_data.items[extra_idx]);
            const param_type_idx: TypeIdx = @enumFromInt(self.store.extra_data.items[extra_idx + 1]);
            const param_direction_raw: u32 = self.store.extra_data.items[extra_idx + 2];
            extra_idx += 4; // 4 values per param: name, type, is_ref, default_value

            const param_name = self.strings.get(param_name_id);
            if (param_name.len == 0) continue;
            const param_type = try self.lowerTypeIdx(param_type_idx);

            // NOTE: We preserve the declared type (*struct(T)) for type checking.
            // The is_ref flag indicates bytecode should use copy-back semantics.
            // Only DBL-specific types (implied_decimal, fixed_decimal, array of u8) need this.
            const is_ref = param_direction_raw != 0 or ir.isDblRefType(param_type);

            try params.append(self.allocator, .{
                .name = param_name,
                .ty = param_type,
                .is_ref = is_ref,
            });
        }

        const return_type_idx: TypeIdx = @enumFromInt(self.store.extra_data.items[extra_idx]);
        const return_type = try self.lowerTypeIdx(return_type_idx);

        // Store param types for call-site type checking
        if (params.items.len > 0) {
            var param_types: std.ArrayListUnmanaged(ir.Type) = .{};
            defer param_types.deinit(self.allocator);
            for (params.items) |param| {
                try param_types.append(self.allocator, param.ty);
            }
            const types_slice = try param_types.toOwnedSlice(self.allocator);
            try self.fn_param_types.put(qualified_name, types_slice);
        }

        // Register return type for method call resolution
        try self.fn_return_types.put(qualified_name, return_type);
    }

    /// Lower impl method BODY only (signature already collected)
    fn lowerImplMethodBody(self: *Self, stmt_idx: StmtIdx, qualified_name: []const u8) LowerError!void {
        const data = self.store.stmtData(stmt_idx);

        debug.print(.ir, "Lowering impl method body: {s}", .{qualified_name});

        // Parse extra data: [param_count, type_param_count, type_params..., param1_name, param1_type, ..., return_type, body]
        const extra_start = data.getParamsStart();
        const param_count = self.store.getExtra(extra_start);
        const type_param_count = self.store.extra_data.items[extra_start.toInt() + 1];

        var params: std.ArrayListUnmanaged(ir.FunctionType.Param) = .{};
        defer params.deinit(self.allocator);

        // Skip past type params: each type param is 2 values (name, bound)
        var extra_idx = extra_start.toInt() + 2 + type_param_count * 2;
        for (0..param_count) |_| {
            const param_name_id: StringId = @enumFromInt(self.store.extra_data.items[extra_idx]);
            const param_type_idx: TypeIdx = @enumFromInt(self.store.extra_data.items[extra_idx + 1]);
            const param_direction_raw: u32 = self.store.extra_data.items[extra_idx + 2];
            extra_idx += 4;

            const param_name = self.strings.get(param_name_id);
            if (param_name.len == 0) continue;
            const param_type = try self.lowerTypeIdx(param_type_idx);

            // NOTE: We preserve the declared type (*struct(T)) for type checking.
            // The is_ref flag indicates bytecode should use copy-back semantics.
            // Only DBL-specific types (implied_decimal, fixed_decimal, array of u8) need this.
            const is_ref = param_direction_raw != 0 or ir.isDblRefType(param_type);

            try params.append(self.allocator, .{
                .name = param_name,
                .ty = param_type,
                .is_ref = is_ref,
            });
        }

        const return_type_idx: TypeIdx = @enumFromInt(self.store.extra_data.items[extra_idx]);
        const body_idx: StmtIdx = @enumFromInt(self.store.extra_data.items[extra_idx + 1]);

        const return_type = try self.lowerTypeIdx(return_type_idx);

        // Create function type with qualified name
        const func_type = ir.FunctionType{
            .params = try params.toOwnedSlice(self.allocator),
            .return_type = return_type,
            .is_variadic = false,
        };

        const func = try ir.Function.init(self.allocator, qualified_name, func_type);

        self.current_func = func;
        self.current_block = func.entry;
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

    /// Lower a default trait method implementation for a specific type
    /// This generates a function like `Point.debug` using the default body from `Display.debug`
    fn lowerDefaultTraitMethod(
        self: *Self,
        trait_method: TraitMethodSig,
        target_type_name: []const u8,
        qualified_name: []const u8,
    ) LowerError!void {
        debug.print(.ir, "Lowering default trait method: {s}", .{qualified_name});

        // Build the function parameters from raw param data
        // raw_param_data format: [name0, type0, name1, type1, ...]
        var params: std.ArrayListUnmanaged(ir.FunctionType.Param) = .{};
        defer params.deinit(self.allocator);

        // Look up the target struct type to use for 'self'
        const target_struct_type: ?ir.Type = blk: {
            if (self.struct_types.get(target_type_name)) |s| {
                break :blk .{ .@"struct" = s };
            }
            if (self.instantiated_structs.get(target_type_name)) |s| {
                break :blk .{ .@"struct" = s };
            }
            break :blk null;
        };

        const raw_data = trait_method.raw_param_data;
        if (raw_data) |data| {
            var i: usize = 0;
            while (i < data.len) : (i += 2) {
                const name_id: StringId = @enumFromInt(data[i]);
                const type_idx: TypeIdx = @enumFromInt(data[i + 1]);

                const param_name = self.strings.get(name_id);

                // For 'self' parameter, use the target struct type instead of the trait type
                const param_type = if (std.mem.eql(u8, param_name, "self") and target_struct_type != null)
                    target_struct_type.?
                else
                    try self.lowerTypeIdx(type_idx);

                try params.append(self.allocator, .{
                    .name = param_name,
                    .ty = param_type,
                    .is_ref = false,
                });
            }
        }

        // Resolve return type
        const return_type = if (trait_method.return_type_idx != .null)
            try self.lowerTypeIdx(trait_method.return_type_idx)
        else
            .void;

        // Store param types for call-site type checking (before moving params to func_type)
        if (params.items.len > 0) {
            var param_types: std.ArrayListUnmanaged(ir.Type) = .{};
            defer param_types.deinit(self.allocator);
            for (params.items) |param| {
                try param_types.append(self.allocator, param.ty);
            }
            const types_slice = try param_types.toOwnedSlice(self.allocator);
            try self.fn_param_types.put(qualified_name, types_slice);
        }

        // Create function type
        const func_type = ir.FunctionType{
            .params = try params.toOwnedSlice(self.allocator),
            .return_type = return_type,
            .is_variadic = false,
        };

        // Create function using the proper init
        const func = try ir.Function.init(self.allocator, qualified_name, func_type);

        self.current_func = func;
        self.current_block = func.entry;
        self.scopes.reset();

        // Note: We don't need to track self_type explicitly - method resolution happens
        // based on the qualified function name (e.g., Point.debug) which is handled
        // by the call expression lowering.

        // Add parameters as local variables (especially 'self')
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

        // Lower the default body
        const body_stmt_idx = trait_method.default_body_stmt_idx.?;
        try self.lowerStatement(body_stmt_idx);

        // Add implicit return if not terminated
        if (!self.current_block.?.isTerminated()) {
            try self.emit(.{ .return_ = null });
        }

        // Register the function
        try self.fn_return_types.put(qualified_name, return_type);
        try self.module.addFunction(func);
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
            .generic_instance => {
                // Generic type like Container<i64> or Box<i64>
                // Build the full mangled name
                const type_data = self.store.typeData(type_idx);
                const base_type_idx: TypeIdx = @enumFromInt(type_data.a);
                const args_start = type_data.b;

                // Get base type name
                const base_name = self.getTypeName(base_type_idx) orelse return null;

                // Get type argument count (first value in extra_data at args_start)
                const arg_count = self.store.extra_data.items[args_start];

                // Build mangled name: BaseName<Arg1, Arg2, ...>
                var name_buf: [256]u8 = undefined;
                var stream = std.io.fixedBufferStream(&name_buf);
                const writer = stream.writer();
                writer.writeAll(base_name) catch return null;
                writer.writeAll("<") catch return null;

                for (0..arg_count) |i| {
                    if (i > 0) writer.writeAll(", ") catch return null;
                    const arg_type_idx: TypeIdx = @enumFromInt(self.store.extra_data.items[args_start + 1 + i]);
                    const arg_name = self.getTypeName(arg_type_idx) orelse "?";
                    writer.writeAll(arg_name) catch return null;
                }
                writer.writeAll(">") catch return null;

                // Allocate and return the mangled name
                return self.allocator.dupe(u8, stream.getWritten()) catch return null;
            },
            // Primitive types
            .i8 => return "i8",
            .i16 => return "i16",
            .i32 => return "i32",
            .i64 => return "i64",
            .isize => return "isize",
            .u8 => return "u8",
            .u16 => return "u16",
            .u32 => return "u32",
            .u64 => return "u64",
            .usize => return "usize",
            .f32 => return "f32",
            .f64 => return "f64",
            .bool => return "bool",
            .string => return "string",
            .void => return "void",
            else => return null,
        }
    }

    /// Resolve a TypeIdx to its name (for bound checking)
    fn resolveTypeIdxName(self: *Self, type_idx: ast.TypeIdx) ?[]const u8 {
        if (type_idx.isNull()) return null;

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

    /// Check if an IR type implements a trait
    fn typeImplementsTrait(self: *Self, ir_type: ir.Type, trait_name: []const u8) bool {
        // Get the type name from the IR type
        const type_name: []const u8 = switch (ir_type) {
            .i32 => "i32",
            .i64 => "i64",
            .f32 => "f32",
            .f64 => "f64",
            .bool => "bool",
            .string => "string",
            .@"struct" => |s| s.name,
            else => return false, // Unknown types don't implement traits
        };

        // Check if there's an impl block for this type+trait combination
        const key = ImplKey{
            .trait_name = trait_name,
            .type_name = type_name,
        };

        return self.impl_methods.contains(key);
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

    /// Collect function signature (params, types, and defaults)
    /// This runs in an earlier pass so types and defaults are available at call sites
    fn collectFnSignature(self: *Self, stmt_idx: StmtIdx) LowerError!void {
        const data = self.store.stmtData(stmt_idx);
        const name_id = data.getName();
        const name = self.strings.get(name_id);
        if (name.len == 0) return LowerError.UndefinedVariable;

        // Parse extra data: [param_count, type_param_count, type_params..., param1_name, param1_type, param1_is_ref, param1_default, ..., return_type, body]
        const extra_start = data.getParamsStart();
        const param_count = self.store.getExtra(extra_start);
        const type_param_count = self.store.extra_data.items[extra_start.toInt() + 1];

        // Skip generic functions - they'll be instantiated at call sites
        if (type_param_count > 0) {
            debug.print(.ir, "Skipping generic function signature: {s} (has {d} type params)", .{ name, type_param_count });
            return;
        }

        // Collect default expressions and types for each parameter
        var param_defaults: std.ArrayListUnmanaged(u32) = .{};
        defer param_defaults.deinit(self.allocator);
        var param_types: std.ArrayListUnmanaged(ir.Type) = .{};
        defer param_types.deinit(self.allocator);

        // Skip past type params: each type param is 2 values (name, bound)
        var extra_idx = extra_start.toInt() + 2 + type_param_count * 2;
        for (0..param_count) |_| {
            const param_type_idx: ast.TypeIdx = @enumFromInt(self.store.extra_data.items[extra_idx + 1]);
            const default_expr_raw: u32 = self.store.extra_data.items[extra_idx + 3];
            extra_idx += 4; // 4 values per param

            param_defaults.append(self.allocator, default_expr_raw) catch return LowerError.OutOfMemory;
            const param_type = try self.lowerTypeIdx(param_type_idx);
            param_types.append(self.allocator, param_type) catch return LowerError.OutOfMemory;
        }

        // Store param defaults and types for this function (for call-site handling)
        if (param_count > 0) {
            const defaults_slice = param_defaults.toOwnedSlice(self.allocator) catch return LowerError.OutOfMemory;
            self.fn_param_defaults.put(name, defaults_slice) catch return LowerError.OutOfMemory;
            const types_slice = param_types.toOwnedSlice(self.allocator) catch return LowerError.OutOfMemory;
            self.fn_param_types.put(name, types_slice) catch return LowerError.OutOfMemory;
        }

        // Extract and store return type
        const return_type_idx: ast.TypeIdx = @enumFromInt(self.store.extra_data.items[extra_idx]);
        const return_type = try self.lowerTypeIdx(return_type_idx);
        self.fn_return_types.put(name, return_type) catch return LowerError.OutOfMemory;
    }

    /// Check if a type implements a trait
    pub fn implementsTrait(self: *Self, type_name: []const u8, trait_name: []const u8) bool {
        const key = ImplKey{
            .trait_name = trait_name,
            .type_name = type_name,
        };
        return self.impl_methods.contains(key);
    }

    /// Find a trait method implementation for a type (searches all trait impls)
    /// Returns the qualified function name (e.g., "Point.print") or null if not found
    pub fn findTraitMethodForType(self: *Self, type_name: []const u8, method_name: []const u8) ?[]const u8 {
        // Iterate over all impl entries to find one that matches this type and method
        var iter = self.impl_methods.iterator();
        while (iter.next()) |entry| {
            const key = entry.key_ptr.*;
            const methods = entry.value_ptr.*;

            // Check if this impl is for the given type (and not an inherent impl)
            if (std.mem.eql(u8, key.type_name, type_name) and !std.mem.eql(u8, key.trait_name, key.type_name)) {
                // Check if this impl has the method we're looking for
                for (methods) |method| {
                    if (std.mem.eql(u8, method.method_name, method_name)) {
                        // Build qualified function name: TypeName.method_name
                        var fn_name_buf: [256]u8 = undefined;
                        const qualified_fn_name = std.fmt.bufPrint(&fn_name_buf, "{s}.{s}", .{ type_name, method_name }) catch {
                            return null;
                        };
                        return self.allocator.dupe(u8, qualified_fn_name) catch null;
                    }
                }
            }
        }
        return null;
    }

    /// Build VTables from all trait implementations
    fn buildVTables(self: *Self) LowerError!void {
        var iter = self.impl_methods.iterator();
        while (iter.next()) |entry| {
            const key = entry.key_ptr.*;
            const methods = entry.value_ptr.*;

            // Build vtable entries
            var vtable_entries: std.ArrayListUnmanaged(ir.VTableEntry) = .{};
            errdefer vtable_entries.deinit(self.allocator);

            for (methods) |method| {
                // Create qualified function name: TypeName.method_name
                var fn_name_buf: [256]u8 = undefined;
                const qualified_fn_name = std.fmt.bufPrint(&fn_name_buf, "{s}.{s}", .{ key.type_name, method.method_name }) catch {
                    return LowerError.OutOfMemory;
                };
                const fn_name = try self.allocator.dupe(u8, qualified_fn_name);

                try vtable_entries.append(self.allocator, .{
                    .method_name = method.method_name,
                    .fn_name = fn_name,
                });
            }

            // Create vtable
            const vtable = ir.VTable{
                .trait_name = key.trait_name,
                .type_name = key.type_name,
                .methods = try vtable_entries.toOwnedSlice(self.allocator),
            };

            try self.module.vtables.append(self.allocator, vtable);
        }
    }

    /// Export trait definitions to the module for method index lookup during dispatch
    fn exportTraitDefs(self: *Self) LowerError!void {
        var iter = self.trait_defs.iterator();
        while (iter.next()) |entry| {
            const trait_def = entry.value_ptr.*;

            // Convert lower_types.TraitMethodSig to ir.TraitMethodSig
            // Now we can resolve types since all types are registered
            var methods: std.ArrayListUnmanaged(ir.TraitMethodSig) = .{};
            for (trait_def.methods) |method| {
                // Resolve the return type (now safe since types are registered)
                const return_type = if (method.return_type_idx != .null)
                    self.lowerTypeIdx(method.return_type_idx) catch .void
                else
                    .void;

                // Generate default function name if this method has a default
                const default_fn_name: ?[]const u8 = if (method.has_default)
                    std.fmt.allocPrint(self.allocator, "{s}.{s}", .{ trait_def.name, method.name }) catch null
                else
                    null;

                try methods.append(self.allocator, .{
                    .name = method.name,
                    .param_count = method.param_count,
                    .return_type = return_type,
                    .default_fn_name = default_fn_name,
                });
            }

            try self.module.traits.append(self.allocator, .{
                .name = trait_def.name,
                .methods = try methods.toOwnedSlice(self.allocator),
            });
        }
    }

    /// Lower a function definition
    fn lowerFnDef(self: *Self, stmt_idx: StmtIdx) LowerError!void {
        const data = self.store.stmtData(stmt_idx);
        const loc = self.store.stmtLoc(stmt_idx);
        self.current_loc = loc; // Set location for error messages

        const name_id = data.getName();
        const name = self.strings.get(name_id);
        if (name.len == 0) return LowerError.UndefinedVariable;

        debug.print(.ir, "Lowering function: {s}", .{name});

        // Parse extra data: [param_count, type_param_count, type_params..., param1_name, param1_type, ..., return_type, body]
        const extra_start = data.getParamsStart();
        const param_count = self.store.getExtra(extra_start);
        const type_param_count = self.store.extra_data.items[extra_start.toInt() + 1];

        // If this is a generic function, store it as a template for later instantiation
        if (type_param_count > 0) {
            debug.print(.ir, "  -> Generic function with {d} type params, storing as template", .{type_param_count});

            // Collect type param names and bounds
            // Format: [param_count, type_param_count, tp1_name, tp1_bound, tp2_name, tp2_bound, ...]
            var type_param_names: std.ArrayListUnmanaged(StringId) = .{};
            defer type_param_names.deinit(self.allocator);
            var type_param_bounds: std.ArrayListUnmanaged(ast.TypeIdx) = .{};
            defer type_param_bounds.deinit(self.allocator);

            for (0..type_param_count) |i| {
                const name_idx = extra_start.toInt() + 2 + @as(u32, @intCast(i)) * 2;
                const bound_idx = name_idx + 1;
                const param_name_id: StringId = @enumFromInt(self.store.extra_data.items[name_idx]);
                const param_bound: ast.TypeIdx = @enumFromInt(self.store.extra_data.items[bound_idx]);
                try type_param_names.append(self.allocator, param_name_id);
                try type_param_bounds.append(self.allocator, param_bound);
            }

            const generic_def = GenericDef{
                .stmt_idx = stmt_idx,
                .type_param_count = @intCast(type_param_count),
                .type_param_names = try type_param_names.toOwnedSlice(self.allocator),
                .type_param_bounds = try type_param_bounds.toOwnedSlice(self.allocator),
            };

            try self.generic_fn_defs.put(name, generic_def);
            return;
        }

        var params: std.ArrayListUnmanaged(ir.FunctionType.Param) = .{};
        defer params.deinit(self.allocator);

        // Skip past type params: each type param is 2 values (name, bound)
        var extra_idx = extra_start.toInt() + 2 + type_param_count * 2;
        for (0..param_count) |_| {
            const param_name_id: StringId = @enumFromInt(self.store.extra_data.items[extra_idx]);
            const param_type_idx: TypeIdx = @enumFromInt(self.store.extra_data.items[extra_idx + 1]);
            const param_direction_raw: u32 = self.store.extra_data.items[extra_idx + 2];
            // default_expr at [extra_idx + 3] already collected in collectFnSignature
            extra_idx += 4; // 4 values per param: name, type, is_ref, default_value

            const param_name = self.strings.get(param_name_id);
            if (param_name.len == 0) continue;
            const param_type = try self.lowerTypeIdx(param_type_idx);

            // Track if this is a ref parameter for bytecode write-back.
            // NOTE: We preserve the declared type (*struct(T)) for type checking.
            // The is_ref flag indicates bytecode should use copy-back semantics.
            // Only DBL-specific types (implied_decimal, fixed_decimal, array of u8) need this.
            const is_ref = param_direction_raw != 0 or ir.isDblRefType(param_type);

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

    /// Lower an impl method with a qualified name (TypeName.method_name)
    fn lowerImplMethod(self: *Self, stmt_idx: StmtIdx, qualified_name: []const u8) LowerError!void {
        const data = self.store.stmtData(stmt_idx);

        debug.print(.ir, "Lowering impl method: {s}", .{qualified_name});

        // Parse extra data: [param_count, type_param_count, type_params..., param1_name, param1_type, ..., return_type, body]
        const extra_start = data.getParamsStart();
        const param_count = self.store.getExtra(extra_start);
        const type_param_count = self.store.extra_data.items[extra_start.toInt() + 1];

        var params: std.ArrayListUnmanaged(ir.FunctionType.Param) = .{};
        defer params.deinit(self.allocator);

        // Skip past type params: each type param is 2 values (name, bound)
        var extra_idx = extra_start.toInt() + 2 + type_param_count * 2;
        for (0..param_count) |_| {
            const param_name_id: StringId = @enumFromInt(self.store.extra_data.items[extra_idx]);
            const param_type_idx: TypeIdx = @enumFromInt(self.store.extra_data.items[extra_idx + 1]);
            const param_direction_raw: u32 = self.store.extra_data.items[extra_idx + 2];
            extra_idx += 4; // 4 values per param: name, type, is_ref, default_value

            const param_name = self.strings.get(param_name_id);
            if (param_name.len == 0) continue;
            const param_type = try self.lowerTypeIdx(param_type_idx);

            // NOTE: We preserve the declared type (*struct(T)) for type checking.
            // The is_ref flag indicates bytecode should use copy-back semantics.
            // Only DBL-specific types (implied_decimal, fixed_decimal, array of u8) need this.
            const is_ref = param_direction_raw != 0 or ir.isDblRefType(param_type);

            try params.append(self.allocator, .{
                .name = param_name,
                .ty = param_type,
                .is_ref = is_ref,
            });
        }

        const return_type_idx: TypeIdx = @enumFromInt(self.store.extra_data.items[extra_idx]);
        const body_idx: StmtIdx = @enumFromInt(self.store.extra_data.items[extra_idx + 1]);

        const return_type = try self.lowerTypeIdx(return_type_idx);

        // Store param types for call-site type checking (before moving params to func_type)
        if (params.items.len > 0) {
            var param_types: std.ArrayListUnmanaged(ir.Type) = .{};
            defer param_types.deinit(self.allocator);
            for (params.items) |param| {
                try param_types.append(self.allocator, param.ty);
            }
            const types_slice = try param_types.toOwnedSlice(self.allocator);
            try self.fn_param_types.put(qualified_name, types_slice);
        }

        // Register return type for method call resolution
        try self.fn_return_types.put(qualified_name, return_type);

        // Create function type with qualified name
        const func_type = ir.FunctionType{
            .params = try params.toOwnedSlice(self.allocator),
            .return_type = return_type,
            .is_variadic = false,
        };

        const func = try ir.Function.init(self.allocator, qualified_name, func_type);

        self.current_func = func;
        self.current_block = func.entry;
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

    /// Lower a test definition into a test function
    fn lowerTestDef(self: *Self, stmt_idx: StmtIdx) LowerError!void {
        const test_data = self.store.getTestDef(stmt_idx);
        const test_name = self.strings.get(test_data.name);

        debug.print(.ir, "Lowering test: {s}", .{test_name});
        log.debug("Lowering test: {s}", .{test_name});

        // Generate mangled function name: __test_<mangled_name>
        // Replace spaces and special chars with underscores
        var mangled_buf: [256]u8 = undefined;
        var mangled_len: usize = 0;

        // Prefix
        const prefix = "__test_";
        @memcpy(mangled_buf[0..prefix.len], prefix);
        mangled_len = prefix.len;

        // Mangle the test name
        for (test_name) |c| {
            if (mangled_len >= mangled_buf.len - 1) break;
            if (std.ascii.isAlphanumeric(c)) {
                mangled_buf[mangled_len] = std.ascii.toLower(c);
            } else {
                mangled_buf[mangled_len] = '_';
            }
            mangled_len += 1;
        }

        // Test functions have no params and return void
        const func_type = ir.FunctionType{
            .params = &.{},
            .return_type = .void,
            .is_variadic = false,
        };

        // Create function (Function.init dupes the name internally)
        const func = try ir.Function.init(self.allocator, mangled_buf[0..mangled_len], func_type);
        func.is_test = true;
        func.test_name = test_name;

        self.current_func = func;
        self.current_block = func.entry;

        // Clear variables for new function scope
        self.scopes.reset();

        // Lower test body
        try self.lowerStatement(test_data.body);

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
    pub fn lowerStatement(self: *Self, stmt_idx: StmtIdx) LowerError!void {
        const tag = self.store.stmtTag(stmt_idx);
        const loc = self.store.stmtLoc(stmt_idx);
        const data = self.store.stmtData(stmt_idx);

        // Track current location for error messages in nested calls
        self.current_loc = loc;

        log.debug("lowerStatement: {s} at line {d}", .{ @tagName(tag), loc.line });

        switch (tag) {
            .assignment => {
                try self.emitDebugLine(loc);
                const ir_loc = lower_expr.toIrLocForStmt(self, stmt_idx);
                try lower_stmt.lowerAssignment(self, data, ir_loc);
            },
            .if_stmt => {
                try self.emitDebugLine(loc);
                try lower_stmt.lowerIf(self, stmt_idx, data);
            },
            .return_stmt => {
                try self.emitDebugLine(loc);
                try lower_stmt.lowerReturn(self, data);
            },
            .block => try lower_stmt.lowerBlock(self, data),
            .record_block => try lower_stmt.lowerRecordBlock(self, data),
            .while_stmt => {
                try self.emitDebugLine(loc);
                try lower_stmt.lowerWhile(self, data);
            },
            .for_stmt => {
                try self.emitDebugLine(loc);
                try lower_stmt.lowerFor(self, stmt_idx, data);
            },
            .loop_stmt => {
                try self.emitDebugLine(loc);
                try lower_stmt.lowerLoop(self, data);
            },
            .break_stmt => {
                try self.emitDebugLine(loc);
                try lower_stmt.lowerBreak(self, loc);
            },
            .continue_stmt => {
                try self.emitDebugLine(loc);
                try lower_stmt.lowerContinue(self, loc);
            },
            .expression => {
                try self.emitDebugLine(loc);
                try lower_stmt.lowerExpressionStmt(self, data);
            },
            .let_decl => {
                try self.emitDebugLine(loc);
                try lower_stmt.lowerLetDecl(self, stmt_idx, data);
            },
            .const_decl => {
                try self.emitDebugLine(loc);
                try lower_stmt.lowerConstDecl(self, stmt_idx, data);
            },
            .io_open => {
                try self.emitDebugLine(loc);
                try lower_io.lowerIoOpen(self, stmt_idx);
            },
            .io_close => {
                try self.emitDebugLine(loc);
                try lower_io.lowerIoClose(self, stmt_idx);
            },
            .io_read => {
                try self.emitDebugLine(loc);
                try lower_io.lowerIoRead(self, stmt_idx);
            },
            .io_write => {
                try self.emitDebugLine(loc);
                try lower_io.lowerIoWrite(self, stmt_idx);
            },
            .io_store => {
                try self.emitDebugLine(loc);
                try lower_io.lowerIoStore(self, stmt_idx);
            },
            .io_delete => {
                try self.emitDebugLine(loc);
                try lower_io.lowerIoDelete(self, stmt_idx);
            },
            .try_stmt => {
                try self.emitDebugLine(loc);
                try self.lowerTry(stmt_idx);
            },
            .throw_stmt => {
                try self.emitDebugLine(loc);
                try lower_stmt.lowerThrow(self, stmt_idx);
            },
            .defer_stmt => {
                // TODO: Proper defer implementation with scope-based execution
                // For now, just record the defer - actual execution happens at scope exit
                try self.emitDebugLine(loc);
                try self.lowerDefer(stmt_idx);
            },
            .match_stmt => {
                try self.emitDebugLine(loc);
                try self.lowerMatch(stmt_idx);
            },
            .field_view => {
                try self.emitDebugLine(loc);
                try lower_stmt.lowerFieldView(self, stmt_idx);
            },
            .import_stmt => {
                // Track imported namespace for validation
                try self.trackImport(stmt_idx);
            },
            .fn_def, .type_alias, .trait_def, .impl_block, .test_def => {
                // Handled in earlier passes or no runtime code
            },
            .struct_def => {
                // Process struct_def in blocks (e.g., DBL RECORD inside function)
                // This creates the struct type for later use in the same block
                try self.lowerStructDef(stmt_idx);
            },
            .union_def => {
                try self.lowerUnionDef(stmt_idx);
            },
            .enum_def => {
                try self.lowerEnumDef(stmt_idx);
            },
            .comptime_if => {
                // Handle comptime if inline during IR lowering
                // Get comptime_if data:
                // - data.a = condition expression
                // - data.b >> 16 = then body statement
                // - data.b & 0xFFFF = index into extra_data for else body
                const comptime_data = self.store.stmtData(stmt_idx);
                const condition_expr = ast.ExprIdx.fromInt(comptime_data.a);
                const then_body = ast.StmtIdx.fromInt(comptime_data.b >> 16);
                const extra_start = comptime_data.b & 0xFFFF;
                const else_body = ast.StmtIdx.fromInt(self.store.extra_data.items[extra_start]);

                // Evaluate the condition at compile time
                if (self.evaluateComptimeCondition(condition_expr)) {
                    // Include then branch
                    if (then_body != .null) {
                        try self.lowerStatement(then_body);
                    }
                } else {
                    // Include else branch if present
                    if (else_body != .null) {
                        try self.lowerStatement(else_body);
                    }
                }
            },
            .comptime_block => {
                // Handle comptime block inline - just lower the body
                const block_data = self.store.stmtData(stmt_idx);
                const body = ast.StmtIdx.fromInt(block_data.a);
                if (body != .null) {
                    try self.lowerStatement(body);
                }
            },
        }
    }

    /// Evaluate a comptime condition expression
    /// Returns true if the condition evaluates to a truthy value at compile time
    fn evaluateComptimeCondition(self: *Self, condition_expr: ast.ExprIdx) bool {
        // Use the stored comptime evaluator which has top-level constants registered
        const result = self.comptime_evaluator.evaluateExpression(condition_expr) catch |err| {
            log.debug("evaluateComptimeCondition: evaluation error: {}", .{err});
            // On error, default to false (exclude the branch)
            return false;
        };

        // Check if the result is truthy
        return result.isTruthy();
    }

    /// Lower a buffer expression, handling struct types specially by emitting load_struct_buf
    /// to serialize all struct fields into a contiguous buffer for I/O operations.
    /// Uses StructHelper for centralized struct detection.
    pub fn lowerStructBufferOrExpression(self: *Self, buffer_idx: ExprIdx) LowerError!ir.Value {
        const func = self.current_func orelse return LowerError.OutOfMemory;

        // Check if buffer expression is a struct type using centralized detection
        if (StructHelper.detectStructTypeFromExpr(&self.scopes, self.store, self.strings, buffer_idx)) |info| {
            // Emit load_struct_buf to serialize the struct
            log.debug("lowerStructBufferOrExpression: serializing '{s}' type '{s}'", .{ info.base_name, info.structName() });
            debug.print(.ir, "lowerStructBufferOrExpression: emitting load_struct_buf for '{s}' type '{s}'", .{ info.base_name, info.structName() });

            const result = try StructHelper.makeResultValue(func, info, self.allocator, &self.allocated_types);
            const inst = StructHelper.makeLoadStructBufInst(info, result);
            try self.emit(inst);
            return result;
        }

        // Not a struct type variable - use normal expression lowering
        return try self.lowerExpression(buffer_idx);
    }

    /// Lower try/catch statement
    fn lowerTry(self: *Self, stmt_idx: StmtIdx) LowerError!void {
        const func = self.current_func orelse return LowerError.OutOfMemory;
        const data = self.store.stmtData(stmt_idx);
        const loc = self.store.stmtLoc(stmt_idx);

        // data.a = try_body (StmtIdx)
        // data.b = extra_start pointing to: [err_binding, catch_body, finally_body]
        const try_body: StmtIdx = @enumFromInt(data.a);
        const extra_start = data.b;
        const err_binding_raw = self.store.extra_data.items[extra_start];
        const err_binding: StringId = @enumFromInt(err_binding_raw);
        const catch_body: StmtIdx = @enumFromInt(self.store.extra_data.items[extra_start + 1]);
        const finally_body_raw = self.store.extra_data.items[extra_start + 2];
        // StmtIdx.null is maxInt(u32), not 0 - check against the actual null sentinel
        const finally_body: ?StmtIdx = if (finally_body_raw != StmtIdx.null.toInt()) @enumFromInt(finally_body_raw) else null;

        // finally was removed from Cot (use defer instead)
        // This error only triggers for DBL legacy code using finally
        if (finally_body != null) {
            self.setErrorContext(
                LowerError.UnsupportedFeature,
                "'finally' is not supported - use 'defer' instead",
                .{},
                loc,
                "defer provides the same cleanup semantics as finally",
                .{},
            );
            return LowerError.UnsupportedFeature;
        }

        // Create blocks for the catch handler and continuation
        const catch_block = try func.createBlock("catch");
        const end_block = try func.createBlock("try_end");

        // Emit try_begin - sets up exception handler pointing to catch_block
        try self.emit(.{ .try_begin = .{ .catch_block = catch_block } });

        // Lower the try body
        try self.lowerStatement(try_body);

        // Emit try_end - clears the exception handler
        try self.emit(.{ .try_end = {} });

        // Jump past the catch block (normal flow)
        try self.emit(.{ .jump = .{ .target = end_block } });

        // Switch to catch block
        self.current_block = catch_block;

        // Handle error binding if present: catch (err) { ... }
        var error_value: ?ir.Value = null;
        if (!err_binding.isNull()) {
            const err_name = self.strings.get(err_binding);

            // Create alloca for the error variable (string type)
            const string_type = ir.Type{ .string = {} };
            const ty_ptr = try self.allocator.create(ir.Type);
            ty_ptr.* = string_type;
            try self.allocated_types.append(self.allocator, ty_ptr);

            const alloca_result = func.newValue(.{ .ptr = ty_ptr });
            try self.emit(.{
                .alloca = .{
                    .ty = string_type,
                    .name = err_name,
                    .result = alloca_result,
                },
            });

            // Add to scope so the catch body can access it
            try self.scopes.put(err_name, alloca_result);
            error_value = alloca_result;

            log.debug("lowerTry: created error binding '{s}' at {d}:{d}", .{ err_name, loc.line, loc.column });
        }

        // Emit catch_begin - will store r0 (error value) to error_value if provided
        try self.emit(.{ .catch_begin = .{ .error_type = null, .error_value = error_value } });

        // Lower the catch body
        try self.lowerStatement(catch_body);

        // Jump to end
        try self.emit(.{ .jump = .{ .target = end_block } });

        // Switch to end block for continuation
        self.current_block = end_block;
    }

    /// Lower defer statement - push deferred body onto the defer stack
    fn lowerDefer(self: *Self, stmt_idx: StmtIdx) LowerError!void {
        const data = self.store.stmtData(stmt_idx);
        const body: StmtIdx = @enumFromInt(data.a);

        // Push the deferred body onto the stack (will be executed at scope exit)
        self.defers.append(self.allocator, body) catch return LowerError.OutOfMemory;
        log.debug("lowerDefer: pushed defer body, stack size={d}", .{self.defers.items.len});
    }

    /// Mark the start of a new scope for defer tracking
    pub fn pushDeferScope(self: *Self) LowerError!void {
        self.defer_scope_marks.append(self.allocator, self.defers.items.len) catch return LowerError.OutOfMemory;
    }

    /// Emit all defers in the current scope (in reverse order) and pop the scope mark
    pub fn popDeferScopeAndEmit(self: *Self) LowerError!void {
        if (self.defer_scope_marks.items.len == 0) return;

        const mark = self.defer_scope_marks.pop().?;
        const defers_in_scope = self.defers.items[mark..];

        // Emit defers in reverse order (LIFO)
        var i: usize = defers_in_scope.len;
        while (i > 0) {
            i -= 1;
            const body = defers_in_scope[i];
            log.debug("popDeferScopeAndEmit: emitting defer body at index {d}", .{mark + i});
            try self.lowerStatement(body);
        }

        // Remove the defers we just emitted
        self.defers.shrinkRetainingCapacity(mark);
    }

    /// Emit all defers up to a certain scope depth (for break/continue through multiple scopes)
    /// Does NOT pop the scope marks - caller must handle that
    pub fn emitDefersToScope(self: *Self, target_depth: usize) LowerError!void {
        if (self.defer_scope_marks.items.len == 0) return;
        if (target_depth >= self.defer_scope_marks.items.len) return;

        const target_mark = self.defer_scope_marks.items[target_depth];
        const defers_to_emit = self.defers.items[target_mark..];

        // Emit defers in reverse order (LIFO)
        var i: usize = defers_to_emit.len;
        while (i > 0) {
            i -= 1;
            const body = defers_to_emit[i];
            log.debug("emitDefersToScope: emitting defer body at index {d}", .{target_mark + i});
            try self.lowerStatement(body);
        }
    }

    /// Emit ALL defers (for return statements - must execute all defers before returning)
    pub fn emitAllDefers(self: *Self) LowerError!void {
        // Emit all defers in reverse order
        var i: usize = self.defers.items.len;
        while (i > 0) {
            i -= 1;
            const body = self.defers.items[i];
            log.debug("emitAllDefers: emitting defer body at index {d}", .{i});
            try self.lowerStatement(body);
        }
    }

    /// Lower match statement
    fn lowerMatch(self: *Self, stmt_idx: StmtIdx) LowerError!void {
        const func = self.current_func orelse return LowerError.OutOfMemory;
        const data = self.store.stmtData(stmt_idx);

        // data.a = scrutinee expr, data.b = arms_start in extra_data
        const scrutinee_idx = data.getExpr();
        const arms_start: ast.ExtraIdx = @enumFromInt(data.b);

        const scrutinee_val = try self.lowerExpression(scrutinee_idx);

        // For variant types, extract the tag for comparison
        const comparison_val = if (scrutinee_val.ty == .variant) blk: {
            const tag_val = func.newValue(.i64);
            try self.emit(.{
                .variant_get_tag = .{
                    .variant = scrutinee_val,
                    .result = tag_val,
                },
            });
            break :blk tag_val;
        } else scrutinee_val;

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

            const arm_block = arm_blocks.items[i];

            // Check for default case (pattern_idx.isNull())
            if (pattern_idx.isNull()) {
                // Default case - unconditionally branch to arm
                try self.emit(.{ .jump = .{ .target = arm_block } });
            } else {
                // Lower pattern expression
                const pattern_val = try self.lowerExpression(pattern_idx);

                // Compare scrutinee (or its tag for variants) with pattern
                const cmp_result = func.newValue(.bool);
                try self.emit(.{
                    .icmp = .{
                        .cond = .eq,
                        .lhs = comparison_val,
                        .rhs = pattern_val,
                        .result = cmp_result,
                    },
                });

                // Branch based on comparison
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
            }

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

    /// Lower an expression to an IR value (delegates to lower_expr module)
    pub fn lowerExpression(self: *Self, expr_idx: ExprIdx) LowerError!ir.Value {
        return lower_expr.lowerExpression(self, expr_idx);
    }

    // ============================================================
    // Expression Handlers (delegated to lower_expr module)
    // ============================================================

    /// Lower an lvalue expression to get a pointer (delegates to lower_expr module)
    pub fn lowerLValue(self: *Self, expr_idx: ExprIdx) LowerError!ir.Value {
        return lower_expr.lowerLValue(self, expr_idx);
    }

    /// Helper for lowerLValue (delegates to lower_expr module)
    pub fn lowerLValueFromExpr(self: *Self, expr_idx: ExprIdx) LowerError!ir.Value {
        return lower_expr.lowerLValueFromExpr(self, expr_idx);
    }

    /// Lower a type index to an IR type (delegates to lower_expr module)
    pub fn lowerTypeIdx(self: *Self, type_idx: TypeIdx) LowerError!ir.Type {
        return lower_expr.lowerTypeIdx(self, type_idx);
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
        std.mem.eql(u8, name, "close"))
    {
        return .void;
    }

    // Default: return type of first argument or void
    if (args.len > 0) {
        return args[0].ty;
    }
    return .void;
}

/// Context for types imported from dependencies (for cross-package compilation)
pub const DependencyTypeContext = struct {
    /// Map struct names to their IR struct types (from compiled dependencies)
    struct_types: std.StringHashMap(*const ir.StructType),
    /// Map union names to their IR union types
    union_types: std.StringHashMap(*const ir.UnionType),
    /// Map enum names to their variant values (enum name -> (variant name -> i64))
    enum_types: std.StringHashMap(std.StringHashMap(i64)),
    /// Map function names to their return types (for cross-package calls)
    fn_return_types: std.StringHashMap(ir.Type),

    pub fn init(allocator: Allocator) DependencyTypeContext {
        return .{
            .struct_types = std.StringHashMap(*const ir.StructType).init(allocator),
            .union_types = std.StringHashMap(*const ir.UnionType).init(allocator),
            .enum_types = std.StringHashMap(std.StringHashMap(i64)).init(allocator),
            .fn_return_types = std.StringHashMap(ir.Type).init(allocator),
        };
    }

    pub fn deinit(self: *DependencyTypeContext) void {
        self.struct_types.deinit();
        self.union_types.deinit();
        // Free inner enum variant maps
        var it = self.enum_types.iterator();
        while (it.next()) |entry| {
            entry.value_ptr.deinit();
        }
        self.enum_types.deinit();
        self.fn_return_types.deinit();
    }
};

/// Options for lowering
pub const LowerOptions = struct {
    /// Run IR verification after lowering (catches bugs early)
    verify: bool = false,

    /// Emit explicit ARC instructions (arc_retain/arc_release) during lowering.
    /// When false (default), the VM handles ARC at runtime via writeRegister/writeStack.
    /// When true, the compiler emits ARC instructions for assignments, returns, and scope exits.
    emit_arc: bool = false,

    /// Types from compiled dependencies (for cross-package compilation)
    dependency_types: ?*DependencyTypeContext = null,

    /// Source file path (for @file() builtin and error messages)
    source_file: ?[]const u8 = null,
};

/// Main entry point for lowering
pub fn lower(
    allocator: Allocator,
    store: *const NodeStore,
    strings: *StringInterner,
    top_level: []const StmtIdx,
    module_name: []const u8,
) LowerError!*ir.Module {
    return lowerWithOptions(allocator, store, strings, top_level, module_name, .{});
}

/// Error detail structure for better error reporting
pub const ErrorDetail = struct {
    message: []const u8,
    context: []const u8,
    line: u32,
    column: u32,
    file_path: ?[]const u8 = null,
};

/// Result type for lowering with error details
pub const LowerResult = union(enum) {
    ok: *ir.Module,
    err: struct {
        kind: LowerError,
        detail: ?ErrorDetail,
    },
};

/// Main entry point for lowering with options
pub fn lowerWithOptions(
    allocator: Allocator,
    store: *const NodeStore,
    strings: *StringInterner,
    top_level: []const StmtIdx,
    module_name: []const u8,
    options: LowerOptions,
) LowerError!*ir.Module {
    const result = lowerWithDetails(allocator, store, strings, top_level, module_name, options);
    switch (result) {
        .ok => |module| return module,
        .err => |e| return e.kind,
    }
}

/// Main entry point for lowering with detailed error reporting
pub fn lowerWithDetails(
    allocator: Allocator,
    store: *const NodeStore,
    strings: *StringInterner,
    top_level: []const StmtIdx,
    module_name: []const u8,
    options: LowerOptions,
) LowerResult {
    var lowerer = Lowerer.init(allocator, store, strings, module_name, options) catch |err| {
        return .{ .err = .{ .kind = err, .detail = null } };
    };

    const module = lowerer.lowerProgram(top_level) catch |err| {
        // Extract error context BEFORE deinit
        const detail: ?ErrorDetail = if (lowerer.getLastError()) |ctx| .{
            .message = allocator.dupe(u8, ctx.message) catch ctx.message,
            .context = allocator.dupe(u8, ctx.context) catch ctx.context,
            .line = ctx.line,
            .column = ctx.column,
            .file_path = ctx.file_path,
        } else null;

        lowerer.deinit();
        return .{ .err = .{ .kind = err, .detail = detail } };
    };

    // Optional IR verification
    if (options.verify) {
        var verifier = verify.Verifier.init(allocator, module);
        defer verifier.deinit();

        verifier.verify() catch {
            // Log verification errors
            for (verifier.getErrors()) |e| {
                log.err("IR verification: {any}", .{e});
            }
            lowerer.deinit();
            return .{ .err = .{ .kind = LowerError.VerificationFailed, .detail = null } };
        };
    }

    lowerer.deinit();
    return .{ .ok = module };
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
/// This version collects multiple errors by analyzing each function independently
pub fn analyzeSemantics(
    allocator: Allocator,
    store: *const NodeStore,
    strings: *StringInterner,
    top_level: []const StmtIdx,
) ![]SemanticDiagnostic {
    var diagnostics: std.ArrayListUnmanaged(SemanticDiagnostic) = .empty;
    errdefer diagnostics.deinit(allocator);

    // First, try early passes that collect definitions (less likely to fail)
    var lowerer = Lowerer.init(allocator, store, strings, "analysis", .{}) catch {
        return diagnostics.toOwnedSlice(allocator);
    };
    defer lowerer.deinit();

    // Run definition collection passes - these rarely fail
    for (top_level) |stmt_idx| {
        lowerer.processStatementForDefinitions(stmt_idx, .structs) catch {};
    }
    for (top_level) |stmt_idx| {
        lowerer.processStatementForDefinitions(stmt_idx, .traits) catch {};
    }
    for (top_level) |stmt_idx| {
        lowerer.processStatementForDefinitions(stmt_idx, .fn_sigs) catch {};
    }
    for (top_level) |stmt_idx| {
        lowerer.processStatementForDefinitions(stmt_idx, .impl_sigs) catch {};
    }
    for (top_level) |stmt_idx| {
        lowerer.processStatementForDefinitions(stmt_idx, .globals) catch {};
    }

    // Now analyze each function body separately to collect multiple errors
    for (top_level) |stmt_idx| {
        const tag = store.stmtTag(stmt_idx);
        if (tag == .fn_def) {
            // Clear any previous error before analyzing this function
            lowerer.last_error = null;

            // Try to lower this function body
            lowerer.lowerFnDef(stmt_idx) catch {
                // Capture the error for this function
                if (lowerer.getLastError()) |ctx| {
                    try diagnostics.append(allocator, .{
                        .line = ctx.line,
                        .column = ctx.column,
                        .message = try allocator.dupe(u8, ctx.message),
                        .severity = .@"error",
                    });
                }
                // Continue to next function
            };
        } else if (tag == .test_def) {
            lowerer.last_error = null;
            lowerer.lowerTestDef(stmt_idx) catch {
                if (lowerer.getLastError()) |ctx| {
                    try diagnostics.append(allocator, .{
                        .line = ctx.line,
                        .column = ctx.column,
                        .message = try allocator.dupe(u8, ctx.message),
                        .severity = .@"error",
                    });
                }
            };
        }
    }

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

// Pull in comprehensive IR lowering tests
test {
    _ = @import("lower_test.zig");
}
