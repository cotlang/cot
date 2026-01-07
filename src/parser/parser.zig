//! Cot Core Parser
//!
//! Parses modern Cot syntax into a NodeStore-based AST.
//! Uses StringInterner for string deduplication and SoA storage for cache efficiency.
//!
//! For DBL syntax (.dbl files), use the cot-dbl frontend package.

const std = @import("std");
const log = std.log.scoped(.parser);
const Token = @import("../lexer/token.zig").Token;
const TokenType = @import("../lexer/token.zig").TokenType;

// Pratt parser for expressions
const pratt = @import("pratt.zig");
const Precedence = pratt.Precedence;

// SoA-based AST
const ast = @import("../ast/mod.zig");
const NodeStore = ast.NodeStore;
const SourceLoc = ast.SourceLoc;
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

pub const ParseError = error{
    UnexpectedToken,
    ExpectedExpression,
    ExpectedIdentifier,
    ExpectedType,
    InvalidSyntax,
    OutOfMemory,
    TooNested,
};

pub const Error = struct {
    message: []const u8,
    line: usize,
    column: usize,
};

/// Result of parsing
pub const ParseResult = struct {
    /// SoA-based NodeStore
    store: *NodeStore,
    /// Top-level statement indices
    top_level: []const StmtIdx,

    pub fn deinit(self: *ParseResult, allocator: std.mem.Allocator) void {
        allocator.free(self.top_level);
    }
};

/// Represents a type parameter in scope (e.g., T in fn foo<T>)
pub const TypeParamInfo = struct {
    name: StringId,
    index: u16,
    bound: TypeIdx, // .null if no bound
};

pub const Parser = struct {
    allocator: std.mem.Allocator,
    tokens: []const Token,
    current: usize,
    errors: std.ArrayListAligned(Error, null),
    nesting_depth: u8,

    // New SoA storage
    store: *NodeStore,
    strings: *StringInterner,

    // Type parameters currently in scope (for generic functions/structs)
    type_params: std.ArrayListUnmanaged(TypeParamInfo),

    // Flag to disable struct initializer parsing (e.g., in for-loop iterable)
    allow_struct_init: bool = true,

    // Flag for handling `>>` as two `>` tokens in generic contexts.
    // When the parser needs a `>` but sees `>>`, it consumes the `>>` and sets this flag.
    // The next time a `>` is needed, this flag is cleared instead of advancing.
    pending_gt: bool = false,

    const Self = @This();
    const MAX_NESTING_DEPTH: u8 = 128;
    const MAX_ERRORS: usize = 100;
    const MAX_TYPE_PARAMS: usize = 16;
    const MAX_ASSOC_TYPES: usize = 8;
    // Marker to distinguish associated types from methods in scratch buffer
    const ASSOC_TYPE_MARKER: u32 = 0xFFFFFFFE;

    pub fn init(allocator: std.mem.Allocator, tokens: []const Token, store: *NodeStore, strings: *StringInterner) Self {
        return .{
            .allocator = allocator,
            .tokens = tokens,
            .current = 0,
            .errors = .empty,
            .nesting_depth = 0,
            .store = store,
            .strings = strings,
            .type_params = .{},
        };
    }

    pub fn deinit(self: *Self) void {
        self.errors.deinit(self.allocator);
        self.type_params.deinit(self.allocator);
    }

    /// Enter a nested context (expression, block, etc.)
    fn enterNesting(self: *Self) ParseError!void {
        if (self.nesting_depth >= MAX_NESTING_DEPTH) {
            self.addError("Expression or block nested too deeply (max 128 levels)");
            return error.TooNested;
        }
        self.nesting_depth += 1;
    }

    /// Exit a nested context
    fn exitNesting(self: *Self) void {
        self.nesting_depth -= 1;
    }

    /// Add an error to the error list
    fn addError(self: *Self, message: []const u8) void {
        if (self.errors.items.len >= MAX_ERRORS) return;
        const token = self.peek();
        self.errors.append(self.allocator, .{
            .message = message,
            .line = token.line,
            .column = token.column,
        }) catch {};
    }

    /// Check if we've hit the error limit
    fn tooManyErrors(self: *Self) bool {
        return self.errors.items.len >= MAX_ERRORS;
    }

    /// Check if parsing had errors
    pub fn hasErrors(self: *Self) bool {
        return self.errors.items.len > 0;
    }

    /// Get the error count
    pub fn errorCount(self: *Self) usize {
        return self.errors.items.len;
    }

    /// Get current source location
    fn currentLoc(self: *Self) SourceLoc {
        const token = self.peek();
        return SourceLoc.init(@intCast(token.line), @intCast(token.column));
    }

    /// Intern a string (get or create StringId)
    fn internString(self: *Self, str: []const u8) !StringId {
        return self.strings.intern(str);
    }

    // ============================================================
    // Main Parse Entry Point
    // ============================================================

    pub fn parse(self: *Self) ParseError![]StmtIdx {
        var top_level: std.ArrayListUnmanaged(StmtIdx) = .{};
        errdefer top_level.deinit(self.allocator);

        while (!self.isAtEnd() and !self.tooManyErrors()) {
            if (self.parseTopLevel()) |stmt_idx| {
                top_level.append(self.allocator, stmt_idx) catch return error.OutOfMemory;
            } else |_| {
                self.synchronize();
            }
        }

        return top_level.toOwnedSlice(self.allocator) catch return error.OutOfMemory;
    }

    // ============================================================
    // Top-Level Declarations
    // ============================================================

    fn parseTopLevel(self: *Self) ParseError!StmtIdx {
        const token = self.peek();

        return switch (token.type) {
            .kw_fn => self.parseFnDef(),
            .kw_struct => self.parseStructDef(),
            .kw_union => self.parseUnionDef(),
            .kw_enum => self.parseEnumDef(),
            .kw_trait => self.parseTraitDef(),
            .kw_impl => self.parseImplBlock(),
            .kw_const => self.parseConstDecl(),
            .kw_var => self.parseVarDecl(),
            .kw_type => self.parseTypeAlias(),
            .kw_import => self.parseImport(),
            .kw_test => self.parseTestDef(),
            .kw_pub => self.parsePubDecl(),
            else => {
                self.addError("Expected 'fn', 'struct', 'union', 'enum', 'trait', 'impl', 'const', 'var', 'type', 'test', or 'import' at top level");
                _ = self.advance();
                return error.UnexpectedToken;
            },
        };
    }

    fn parsePubDecl(self: *Self) ParseError!StmtIdx {
        _ = try self.consume(.kw_pub, "Expected 'pub'");
        // Parse the actual declaration (fn, struct, etc.)
        return self.parseTopLevel();
    }

    fn parseFnDef(self: *Self) ParseError!StmtIdx {
        const loc = self.currentLoc();
        _ = try self.consume(.kw_fn, "Expected 'fn'");
        const name_token = try self.consume(.identifier, "Expected function name");
        const name = self.internString(name_token.lexeme) catch return error.OutOfMemory;

        // Parse optional type parameters: fn foo<T, U>
        const type_param_count = try self.parseTypeParams();
        const type_params_start = self.storeTypeParams(type_param_count) catch return error.OutOfMemory;

        _ = try self.consume(.lparen, "Expected '('");

        // Parse parameters into scratch buffer
        try self.store.markScratch();
        errdefer {
            self.store.rollbackScratch();
            self.clearTypeParams(type_param_count);
        }

        if (!self.check(.rparen)) {
            while (true) {
                // Handle both regular identifiers and 'self' keyword
                const is_self = self.match(&[_]TokenType{.kw_self});
                const param_name = if (is_self)
                    self.internString("self") catch return error.OutOfMemory
                else blk: {
                    const param_name_token = try self.consume(.identifier, "Expected parameter name");
                    break :blk self.internString(param_name_token.lexeme) catch return error.OutOfMemory;
                };

                // For 'self' parameter, type annotation is optional (defaults to Self)
                const param_type = if (is_self and !self.check(.colon)) blk: {
                    // Use a special "Self" type that gets resolved during type checking
                    break :blk self.store.addSelfType() catch return error.OutOfMemory;
                } else blk: {
                    _ = try self.consume(.colon, "Expected ':'");
                    break :blk try self.parseType();
                };

                // Parse optional default value: param: Type = expr
                var default_expr: u32 = 0; // 0 = ExprIdx.null (required param)
                if (self.match(&[_]TokenType{.equals})) {
                    const expr = try self.parseExpression();
                    default_expr = expr.toInt();
                }

                // Store as [name, type, is_ref, default_value] in scratch u32 buffer
                // is_ref: 0=val (default for Cot), 1=ref
                self.store.pushScratchU32(@intFromEnum(param_name)) catch return error.OutOfMemory;
                self.store.pushScratchU32(param_type.toInt()) catch return error.OutOfMemory;
                self.store.pushScratchU32(0) catch return error.OutOfMemory; // val by default
                self.store.pushScratchU32(default_expr) catch return error.OutOfMemory;

                if (!self.match(&[_]TokenType{.comma})) break;
            }
        }

        _ = try self.consume(.rparen, "Expected ')'");

        // Return type: Zig-style (type directly after ')', or void if '{' follows)
        var return_type: TypeIdx = .null;
        if (!self.check(.lbrace)) {
            return_type = try self.parseType();
        }

        _ = try self.consume(.lbrace, "Expected '{'");

        // Parse body
        const body = try self.parseBlock();

        // Get params from scratch and duplicate them
        const params = self.store.getScratchU32s();
        const params_copy = self.allocator.dupe(u32, params) catch return error.OutOfMemory;
        // Roll back scratch instead of commit - we've copied the data so we don't need it in scratch
        // This prevents nested scratch usage from leaking data to parent callers
        self.store.rollbackScratch();

        // Extract type param (name, bound) pairs for generic function storage
        var type_param_pairs: [MAX_TYPE_PARAMS * 2]u32 = undefined;
        if (type_param_count > 0) {
            const start_idx = self.type_params.items.len - type_param_count;
            for (self.type_params.items[start_idx..], 0..) |param, i| {
                type_param_pairs[i * 2] = @intFromEnum(param.name);
                type_param_pairs[i * 2 + 1] = param.bound.toInt();
            }
        }
        _ = type_params_start; // Stored separately in extra_data, not needed for fn_def

        // Clear type params from scope
        self.clearTypeParams(type_param_count);

        return self.store.addGenericFnDef(name, type_param_pairs[0 .. type_param_count * 2], params_copy, return_type, body, loc) catch return error.OutOfMemory;
    }

    fn parseStructDef(self: *Self) ParseError!StmtIdx {
        const loc = self.currentLoc();
        _ = try self.consume(.kw_struct, "Expected 'struct'");
        const name_token = try self.consume(.identifier, "Expected struct name");
        const name = self.internString(name_token.lexeme) catch return error.OutOfMemory;

        // Parse optional type parameters: struct Foo<T, U>
        const type_param_count = try self.parseTypeParams();
        const type_params_start = self.storeTypeParams(type_param_count) catch return error.OutOfMemory;

        _ = try self.consume(.lbrace, "Expected '{'");

        // Parse fields into scratch buffer
        try self.store.markScratch();
        errdefer {
            self.store.rollbackScratch();
            self.clearTypeParams(type_param_count);
        }

        while (!self.check(.rbrace) and !self.isAtEnd()) {
            const field_name_token = try self.consume(.identifier, "Expected field name");
            const field_name = self.internString(field_name_token.lexeme) catch return error.OutOfMemory;
            _ = try self.consume(.colon, "Expected ':'");
            const field_type = try self.parseType();

            // Store as [name, type] pairs
            self.store.pushScratchU32(@intFromEnum(field_name)) catch return error.OutOfMemory;
            self.store.pushScratchU32(field_type.toInt()) catch return error.OutOfMemory;

            // Optional comma
            _ = self.match(&[_]TokenType{.comma});
        }

        _ = try self.consume(.rbrace, "Expected '}'");

        // Clear type params from scope
        self.clearTypeParams(type_param_count);

        // Get fields from scratch
        const fields = self.store.getScratchU32s();
        self.store.commitScratch();

        // Store struct def - fields and type params are stored in extra_data
        const fields_start = self.store.extra_data.items.len;
        self.store.extra_data.append(self.allocator, @intCast(fields.len / 2)) catch return error.OutOfMemory; // field count
        self.store.extra_data.append(self.allocator, type_param_count) catch return error.OutOfMemory; // type param count
        self.store.extra_data.append(self.allocator, type_params_start) catch return error.OutOfMemory; // type params start
        for (fields) |f| {
            self.store.extra_data.append(self.allocator, f) catch return error.OutOfMemory;
        }

        // Add struct_def statement
        const idx: StmtIdx = @enumFromInt(@as(u32, @intCast(self.store.stmt_tags.items.len)));
        self.store.stmt_tags.append(self.allocator, .struct_def) catch return error.OutOfMemory;
        self.store.stmt_locs.append(self.allocator, loc) catch return error.OutOfMemory;
        self.store.stmt_data.append(self.allocator, .{
            .a = @intFromEnum(name),
            .b = @intCast(fields_start),
        }) catch return error.OutOfMemory;

        return idx;
    }

    /// Parse union definition: union Name { field: Type, ... }
    /// All fields in a union share the same memory (like C union)
    fn parseUnionDef(self: *Self) ParseError!StmtIdx {
        const loc = self.currentLoc();
        _ = try self.consume(.kw_union, "Expected 'union'");
        const name_token = try self.consume(.identifier, "Expected union name");
        const name = self.internString(name_token.lexeme) catch return error.OutOfMemory;

        _ = try self.consume(.lbrace, "Expected '{'");

        // Parse variants into scratch buffer
        try self.store.markScratch();
        errdefer self.store.rollbackScratch();

        while (!self.check(.rbrace) and !self.isAtEnd()) {
            const variant_name_token = try self.consume(.identifier, "Expected variant name");
            const variant_name = self.internString(variant_name_token.lexeme) catch return error.OutOfMemory;
            _ = try self.consume(.colon, "Expected ':'");
            const variant_type = try self.parseType();

            // Store as [name, type] pairs
            self.store.pushScratchU32(@intFromEnum(variant_name)) catch return error.OutOfMemory;
            self.store.pushScratchU32(variant_type.toInt()) catch return error.OutOfMemory;

            // Optional comma
            _ = self.match(&[_]TokenType{.comma});
        }

        _ = try self.consume(.rbrace, "Expected '}'");

        // Get variants from scratch
        const variants = self.store.getScratchU32s();
        self.store.commitScratch();

        // Store union def in extra_data
        const variants_start = self.store.extra_data.items.len;
        self.store.extra_data.append(self.allocator, @intCast(variants.len / 2)) catch return error.OutOfMemory; // variant count
        for (variants) |v| {
            self.store.extra_data.append(self.allocator, v) catch return error.OutOfMemory;
        }

        // Add union_def statement
        const idx: StmtIdx = @enumFromInt(@as(u32, @intCast(self.store.stmt_tags.items.len)));
        self.store.stmt_tags.append(self.allocator, .union_def) catch return error.OutOfMemory;
        self.store.stmt_locs.append(self.allocator, loc) catch return error.OutOfMemory;
        self.store.stmt_data.append(self.allocator, .{
            .a = @intFromEnum(name),
            .b = @intCast(variants_start),
        }) catch return error.OutOfMemory;

        return idx;
    }

    fn parseEnumDef(self: *Self) ParseError!StmtIdx {
        const loc = self.currentLoc();
        _ = try self.consume(.kw_enum, "Expected 'enum'");
        const name_token = try self.consume(.identifier, "Expected enum name");
        const name = self.internString(name_token.lexeme) catch return error.OutOfMemory;

        _ = try self.consume(.lbrace, "Expected '{'");

        // Parse variants into scratch buffer
        try self.store.markScratch();
        errdefer self.store.rollbackScratch();

        while (!self.check(.rbrace) and !self.isAtEnd()) {
            const variant_token = try self.consume(.identifier, "Expected variant name");
            const variant_name = self.internString(variant_token.lexeme) catch return error.OutOfMemory;

            self.store.pushScratchU32(@intFromEnum(variant_name)) catch return error.OutOfMemory;

            // Optional comma
            _ = self.match(&[_]TokenType{.comma});
        }

        _ = try self.consume(.rbrace, "Expected '}'");

        // Get variants from scratch
        const variants = self.store.getScratchU32s();
        self.store.commitScratch();

        // Store enum def
        const variants_start = self.store.extra_data.items.len;
        self.store.extra_data.append(self.allocator, @intCast(variants.len)) catch return error.OutOfMemory;
        for (variants) |v| {
            self.store.extra_data.append(self.allocator, v) catch return error.OutOfMemory;
        }

        const idx: StmtIdx = @enumFromInt(@as(u32, @intCast(self.store.stmt_tags.items.len)));
        self.store.stmt_tags.append(self.allocator, .enum_def) catch return error.OutOfMemory;
        self.store.stmt_locs.append(self.allocator, loc) catch return error.OutOfMemory;
        self.store.stmt_data.append(self.allocator, .{
            .a = @intFromEnum(name),
            .b = @intCast(variants_start),
        }) catch return error.OutOfMemory;

        return idx;
    }

    /// Parse trait definition: trait Name<T> { type Item; fn method(self) -> T; }
    fn parseTraitDef(self: *Self) ParseError!StmtIdx {
        const loc = self.currentLoc();
        _ = try self.consume(.kw_trait, "Expected 'trait'");
        const name_token = try self.consume(.identifier, "Expected trait name");
        const name = self.internString(name_token.lexeme) catch return error.OutOfMemory;

        // Parse optional type parameters: trait Numeric<T>
        const type_param_count = try self.parseTypeParams();
        const type_params_start = self.storeTypeParams(type_param_count) catch return error.OutOfMemory;

        _ = try self.consume(.lbrace, "Expected '{'");

        // Parse associated types and method signatures
        try self.store.markScratch();
        errdefer {
            self.store.rollbackScratch();
            self.clearTypeParams(type_param_count);
        }

        // First pass: collect associated types (stored at beginning of scratch)
        // We store them separately and count them
        var assoc_types_temp: [MAX_ASSOC_TYPES * 2]u32 = undefined; // [name, bound] pairs
        var assoc_type_count: u32 = 0;

        var method_count: u32 = 0;
        while (!self.check(.rbrace) and !self.isAtEnd()) {
            // Check for associated type: type Name; or type Name: Bound;
            if (self.check(.kw_type)) {
                _ = self.advance(); // consume 'type'
                const type_name_token = try self.consume(.identifier, "Expected associated type name");
                const type_name_id = self.internString(type_name_token.lexeme) catch return error.OutOfMemory;

                // Optional bound: type Item: Comparable;
                var bound_type: TypeIdx = .null;
                if (self.match(&[_]TokenType{.colon})) {
                    bound_type = try self.parseType();
                }

                _ = self.match(&[_]TokenType{.semicolon}); // optional semicolon

                if (assoc_type_count >= MAX_ASSOC_TYPES) {
                    self.addError("Too many associated types in trait (max 8)");
                    return error.InvalidSyntax;
                }

                // Store in temp buffer
                assoc_types_temp[assoc_type_count * 2] = @intFromEnum(type_name_id);
                assoc_types_temp[assoc_type_count * 2 + 1] = bound_type.toInt();
                assoc_type_count += 1;
                continue;
            }

            // Parse method signature: fn name(params) -> return_type;
            _ = try self.consume(.kw_fn, "Expected 'fn' or 'type' in trait definition");
            const method_name_token = try self.consume(.identifier, "Expected method name");
            const method_name = self.internString(method_name_token.lexeme) catch return error.OutOfMemory;

            _ = try self.consume(.lparen, "Expected '('");

            // Parse parameters - collect them first, store after we know param_count
            var params_temp: [64]u32 = undefined; // Max 16 params per method (4 values each: name, type, is_ref, default)
            var param_count: u32 = 0;
            if (!self.check(.rparen)) {
                while (true) {
                    if (param_count >= 16) {
                        self.addError("Too many parameters in trait method (max 16)");
                        return error.InvalidSyntax;
                    }
                    // Handle both regular identifiers and 'self' keyword
                    const param_name_str = if (self.match(&[_]TokenType{.kw_self}))
                        self.internString("self") catch return error.OutOfMemory
                    else blk: {
                        const param_name_token = try self.consume(.identifier, "Expected parameter name");
                        break :blk self.internString(param_name_token.lexeme) catch return error.OutOfMemory;
                    };
                    _ = try self.consume(.colon, "Expected ':'");
                    const param_type = try self.parseType();

                    params_temp[param_count * 4] = @intFromEnum(param_name_str);
                    params_temp[param_count * 4 + 1] = param_type.toInt();
                    params_temp[param_count * 4 + 2] = 0; // val (default for Cot)
                    params_temp[param_count * 4 + 3] = 0; // no default value (required)
                    param_count += 1;

                    if (!self.match(&[_]TokenType{.comma})) break;
                }
            }

            _ = try self.consume(.rparen, "Expected ')'");

            // Return type: Zig-style (type directly after ')', or void if terminator follows)
            var return_type: TypeIdx = .null;
            if (!self.check(.semicolon) and !self.check(.comma) and !self.check(.rbrace)) {
                return_type = try self.parseType();
            }

            // Check for default implementation body (optional)
            var default_body: StmtIdx = .null;
            if (self.check(.lbrace)) {
                _ = self.advance(); // consume '{'
                default_body = try self.parseBlock();
            } else {
                // Expect semicolon or comma for methods without default body
                _ = self.match(&[_]TokenType{ .semicolon, .comma });
            }

            // Store method signature: [method_name, param_count, return_type, has_default, default_body, param_quads...]
            // This order makes parsing easier - we know param_count before reading params
            self.store.pushScratchU32(@intFromEnum(method_name)) catch return error.OutOfMemory;
            self.store.pushScratchU32(param_count) catch return error.OutOfMemory;
            self.store.pushScratchU32(return_type.toInt()) catch return error.OutOfMemory;
            self.store.pushScratchU32(if (default_body != .null) 1 else 0) catch return error.OutOfMemory;
            self.store.pushScratchU32(default_body.toInt()) catch return error.OutOfMemory;
            // Now store the param quads (name, type, is_ref, default_value)
            for (0..param_count) |i| {
                self.store.pushScratchU32(params_temp[i * 4]) catch return error.OutOfMemory;
                self.store.pushScratchU32(params_temp[i * 4 + 1]) catch return error.OutOfMemory;
                self.store.pushScratchU32(params_temp[i * 4 + 2]) catch return error.OutOfMemory;
                self.store.pushScratchU32(params_temp[i * 4 + 3]) catch return error.OutOfMemory;
            }

            method_count += 1;
        }

        _ = try self.consume(.rbrace, "Expected '}'");

        // Clear type params from scope
        self.clearTypeParams(type_param_count);

        // Get method signatures from scratch
        const methods = self.store.getScratchU32s();
        self.store.commitScratch();

        // Store trait def with new layout:
        // [method_count, type_param_count, type_params_start, assoc_type_count, ...assoc_types..., ...methods...]
        const extra_start = self.store.extra_data.items.len;
        self.store.extra_data.append(self.allocator, method_count) catch return error.OutOfMemory;
        self.store.extra_data.append(self.allocator, type_param_count) catch return error.OutOfMemory;
        self.store.extra_data.append(self.allocator, type_params_start) catch return error.OutOfMemory;
        self.store.extra_data.append(self.allocator, assoc_type_count) catch return error.OutOfMemory;

        // Store associated types: [name, bound] pairs
        for (0..assoc_type_count) |i| {
            self.store.extra_data.append(self.allocator, assoc_types_temp[i * 2]) catch return error.OutOfMemory;
            self.store.extra_data.append(self.allocator, assoc_types_temp[i * 2 + 1]) catch return error.OutOfMemory;
        }

        // Store method signatures
        for (methods) |m| {
            self.store.extra_data.append(self.allocator, m) catch return error.OutOfMemory;
        }

        const idx: StmtIdx = @enumFromInt(@as(u32, @intCast(self.store.stmt_tags.items.len)));
        self.store.stmt_tags.append(self.allocator, .trait_def) catch return error.OutOfMemory;
        self.store.stmt_locs.append(self.allocator, loc) catch return error.OutOfMemory;
        self.store.stmt_data.append(self.allocator, .{
            .a = @intFromEnum(name),
            .b = @intCast(extra_start),
        }) catch return error.OutOfMemory;

        return idx;
    }

    /// Parse impl block: impl Trait for Type { type Item = T; methods }
    fn parseImplBlock(self: *Self) ParseError!StmtIdx {
        const loc = self.currentLoc();
        _ = try self.consume(.kw_impl, "Expected 'impl'");

        // Parse the trait name (or just a type for inherent impls)
        const trait_type = try self.parseType();

        // Check for "for Type"
        var target_type: TypeIdx = .null;
        if (self.match(&[_]TokenType{.kw_for})) {
            target_type = try self.parseType();
        }

        _ = try self.consume(.lbrace, "Expected '{'");

        // Parse associated type bindings and method implementations
        try self.store.markScratch();
        errdefer self.store.rollbackScratch();

        // Track associated type bindings separately
        var assoc_bindings_temp: [MAX_ASSOC_TYPES * 2]u32 = undefined; // [name, concrete_type] pairs
        var assoc_binding_count: u32 = 0;

        var method_count: u32 = 0;
        while (!self.check(.rbrace) and !self.isAtEnd()) {
            // Check for associated type binding: type Name = ConcreteType;
            if (self.check(.kw_type)) {
                _ = self.advance(); // consume 'type'
                const type_name_token = try self.consume(.identifier, "Expected associated type name");
                const type_name_id = self.internString(type_name_token.lexeme) catch return error.OutOfMemory;

                _ = try self.consume(.equals, "Expected '=' after associated type name");
                const concrete_type = try self.parseType();

                _ = self.match(&[_]TokenType{.semicolon}); // optional semicolon

                if (assoc_binding_count >= MAX_ASSOC_TYPES) {
                    self.addError("Too many associated type bindings in impl (max 8)");
                    return error.InvalidSyntax;
                }

                // Store in temp buffer
                assoc_bindings_temp[assoc_binding_count * 2] = @intFromEnum(type_name_id);
                assoc_bindings_temp[assoc_binding_count * 2 + 1] = concrete_type.toInt();
                assoc_binding_count += 1;
                continue;
            }

            // Each method is a full fn definition
            const method_stmt = try self.parseFnDef();
            self.store.pushScratchU32(method_stmt.toInt()) catch return error.OutOfMemory;
            method_count += 1;
        }

        _ = try self.consume(.rbrace, "Expected '}'");

        // Get methods from scratch
        const methods = self.store.getScratchU32s();
        self.store.commitScratch();

        // Store impl block with new layout:
        // [method_count, trait_type, target_type, assoc_binding_count, ...assoc_bindings..., method_stmt_idx...]
        const extra_start = self.store.extra_data.items.len;
        self.store.extra_data.append(self.allocator, method_count) catch return error.OutOfMemory;
        self.store.extra_data.append(self.allocator, trait_type.toInt()) catch return error.OutOfMemory;
        self.store.extra_data.append(self.allocator, target_type.toInt()) catch return error.OutOfMemory;
        self.store.extra_data.append(self.allocator, assoc_binding_count) catch return error.OutOfMemory;

        // Store associated type bindings: [name, concrete_type] pairs
        for (0..assoc_binding_count) |i| {
            self.store.extra_data.append(self.allocator, assoc_bindings_temp[i * 2]) catch return error.OutOfMemory;
            self.store.extra_data.append(self.allocator, assoc_bindings_temp[i * 2 + 1]) catch return error.OutOfMemory;
        }

        // Store method statement indices
        for (methods) |m| {
            self.store.extra_data.append(self.allocator, m) catch return error.OutOfMemory;
        }

        const idx: StmtIdx = @enumFromInt(@as(u32, @intCast(self.store.stmt_tags.items.len)));
        self.store.stmt_tags.append(self.allocator, .impl_block) catch return error.OutOfMemory;
        self.store.stmt_locs.append(self.allocator, loc) catch return error.OutOfMemory;
        self.store.stmt_data.append(self.allocator, .{
            .a = trait_type.toInt(),
            .b = @intCast(extra_start),
        }) catch return error.OutOfMemory;

        return idx;
    }

    fn parseConstDecl(self: *Self) ParseError!StmtIdx {
        const loc = self.currentLoc();
        _ = try self.consume(.kw_const, "Expected 'const'");
        const name_token = try self.consume(.identifier, "Expected constant name");
        const name = self.internString(name_token.lexeme) catch return error.OutOfMemory;

        // Optional type annotation
        var type_idx: TypeIdx = .null;
        if (self.match(&[_]TokenType{.colon})) {
            type_idx = try self.parseType();
        }

        _ = try self.consume(.equals, "Expected '='");
        const init_expr = try self.parseExpression();

        const stmt = self.store.addConstDecl(name, type_idx, init_expr, loc) catch return error.OutOfMemory;
        // Optionally consume semicolon
        _ = self.match(&[_]TokenType{.semicolon});
        return stmt;
    }

    /// Parse var declaration (Zig-style mutable variable): var name: Type = expr
    fn parseVarDecl(self: *Self) ParseError!StmtIdx {
        const loc = self.currentLoc();
        _ = try self.consume(.kw_var, "Expected 'var'");

        const name_token = try self.consume(.identifier, "Expected variable name");
        const name = self.internString(name_token.lexeme) catch return error.OutOfMemory;

        // Optional type annotation
        var type_idx: TypeIdx = .null;
        if (self.match(&[_]TokenType{.colon})) {
            type_idx = try self.parseType();
        }

        _ = try self.consume(.equals, "Expected '='");
        const init_expr = try self.parseExpression();

        // var is always mutable (is_mut = true)
        const stmt = self.store.addLetDecl(name, type_idx, init_expr, true, loc) catch return error.OutOfMemory;
        // Optionally consume semicolon
        _ = self.match(&[_]TokenType{.semicolon});
        return stmt;
    }

    /// Parse view declaration: view name: Type = @base_field or view name: Type = @base_field + offset
    /// Creates a memory alias to another variable
    fn parseViewDecl(self: *Self) ParseError!StmtIdx {
        const loc = self.currentLoc();
        _ = try self.consume(.kw_view, "Expected 'view'");

        const name_token = try self.consume(.identifier, "Expected view name");
        const name = self.internString(name_token.lexeme) catch return error.OutOfMemory;

        // Type annotation is required for views
        _ = try self.consume(.colon, "Expected ':'");
        const type_idx = try self.parseType();

        _ = try self.consume(.equals, "Expected '='");

        // Expect @ followed by base field name
        _ = try self.consume(.at, "Expected '@' before base field name");
        const base_field_token = try self.consume(.identifier, "Expected base field name");
        const base_field = self.internString(base_field_token.lexeme) catch return error.OutOfMemory;

        // Parse optional offset: + or - followed by number
        var offset: i32 = 0;
        if (self.match(&[_]TokenType{.plus})) {
            const offset_token = try self.consume(.integer_literal, "Expected offset value");
            offset = @intCast(std.fmt.parseInt(i32, offset_token.lexeme, 10) catch 0);
        } else if (self.match(&[_]TokenType{.minus})) {
            const offset_token = try self.consume(.integer_literal, "Expected offset value");
            offset = -@as(i32, @intCast(std.fmt.parseInt(i32, offset_token.lexeme, 10) catch 0));
        }

        return self.store.addFieldView(name, type_idx, base_field, offset, loc) catch return error.OutOfMemory;
    }

    fn parseTypeAlias(self: *Self) ParseError!StmtIdx {
        const loc = self.currentLoc();
        _ = try self.consume(.kw_type, "Expected 'type'");
        const name_token = try self.consume(.identifier, "Expected type name");
        const name = self.internString(name_token.lexeme) catch return error.OutOfMemory;
        _ = try self.consume(.equals, "Expected '='");
        const aliased_type = try self.parseType();

        // Add type_alias statement
        const idx: StmtIdx = @enumFromInt(@as(u32, @intCast(self.store.stmt_tags.items.len)));
        self.store.stmt_tags.append(self.allocator, .type_alias) catch return error.OutOfMemory;
        self.store.stmt_locs.append(self.allocator, loc) catch return error.OutOfMemory;
        self.store.stmt_data.append(self.allocator, .{
            .a = @intFromEnum(name),
            .b = aliased_type.toInt(),
        }) catch return error.OutOfMemory;

        return idx;
    }

    fn parseImport(self: *Self) ParseError!StmtIdx {
        const loc = self.currentLoc();
        _ = try self.consume(.kw_import, "Expected 'import'");

        // Can be identifier or string literal
        var module_name: StringId = undefined;
        if (self.match(&[_]TokenType{.string_literal})) {
            const lexeme = self.previous().lexeme;
            const str = if (lexeme.len >= 2) lexeme[1 .. lexeme.len - 1] else lexeme;
            module_name = self.internString(str) catch return error.OutOfMemory;
        } else {
            const name_token = try self.consume(.identifier, "Expected module name");
            module_name = self.internString(name_token.lexeme) catch return error.OutOfMemory;
        }

        return self.store.addImport(module_name, loc) catch return error.OutOfMemory;
    }

    /// Parse a test definition: test "name" { body }
    fn parseTestDef(self: *Self) ParseError!StmtIdx {
        const loc = self.currentLoc();
        _ = try self.consume(.kw_test, "Expected 'test'");

        // Expect test name as string literal
        const name_token = try self.consume(.string_literal, "Expected test name as string literal");
        const name_lexeme = name_token.lexeme;
        // Strip quotes from string literal
        const name_str = if (name_lexeme.len >= 2) name_lexeme[1 .. name_lexeme.len - 1] else name_lexeme;
        const name = self.internString(name_str) catch return error.OutOfMemory;

        // Expect block body
        _ = try self.consume(.lbrace, "Expected '{' after test name");
        const body = try self.parseBlock();

        return self.store.addTestDef(name, body, loc) catch return error.OutOfMemory;
    }

    // ============================================================
    // Statements
    // ============================================================

    fn parseStatement(self: *Self) ParseError!StmtIdx {
        const token = self.peek();

        return switch (token.type) {
            .kw_if => self.parseIf(),
            .kw_switch => self.parseSwitch(),
            .kw_for => self.parseFor(),
            .kw_while => self.parseWhile(),
            .kw_loop => self.parseLoop(),
            .kw_break => self.parseBreak(),
            .kw_continue => self.parseContinue(),
            .kw_return => self.parseReturn(),
            .kw_var => self.parseVarDecl(),
            .kw_const => self.parseConstDecl(),
            .kw_view => self.parseViewDecl(),
            .kw_try => self.parseTry(),
            .kw_throw => self.parseThrow(),
            .kw_defer => self.parseDefer(),
            .kw_comptime => self.parseComptime(),
            .lbrace => self.parseBlockStmt(),
            else => self.parseExpressionStatement(),
        };
    }

    fn parseBlockStmt(self: *Self) ParseError!StmtIdx {
        _ = try self.consume(.lbrace, "Expected '{'");
        return self.parseBlock();
    }

    fn parseBlock(self: *Self) ParseError!StmtIdx {
        try self.enterNesting();
        errdefer self.exitNesting();

        const loc = self.currentLoc();

        // Collect statements into scratch buffer
        try self.store.markScratch();
        errdefer self.store.rollbackScratch();

        while (!self.check(.rbrace) and !self.isAtEnd()) {
            if (self.parseStatement()) |stmt| {
                self.store.pushScratchStmt(stmt) catch return error.OutOfMemory;
            } else |_| {
                self.synchronize();
            }
        }

        _ = try self.consume(.rbrace, "Expected '}'");

        const stmts = self.store.getScratchStmts();
        const stmts_copy = self.allocator.dupe(StmtIdx, stmts) catch return error.OutOfMemory;
        self.store.commitScratch();

        self.exitNesting();
        return self.store.addBlock(stmts_copy, loc) catch return error.OutOfMemory;
    }

    fn parseIf(self: *Self) ParseError!StmtIdx {
        try self.enterNesting();
        errdefer self.exitNesting();

        const loc = self.currentLoc();
        _ = try self.consume(.kw_if, "Expected 'if'");
        _ = try self.consume(.lparen, "Expected '(' after 'if'");

        // Disable struct init parsing for condition to avoid ambiguity with < operator
        // e.g., "if (x < 10)" should not be parsed as "if (x<type>)"
        const prev_allow_struct_init = self.allow_struct_init;
        self.allow_struct_init = false;
        const condition = try self.parseExpression();
        self.allow_struct_init = prev_allow_struct_init;

        _ = try self.consume(.rparen, "Expected ')' after condition");

        _ = try self.consume(.lbrace, "Expected '{'");
        const then_body = try self.parseBlock();

        var else_body: StmtIdx = .null;
        if (self.match(&[_]TokenType{.kw_else})) {
            if (self.check(.kw_if)) {
                // else if
                else_body = try self.parseIf();
            } else {
                _ = try self.consume(.lbrace, "Expected '{'");
                else_body = try self.parseBlock();
            }
        }

        self.exitNesting();
        return self.store.addIfStmt(condition, then_body, else_body, loc) catch return error.OutOfMemory;
    }

    /// Parse switch statement (Zig-style): switch (expr) { ... }
    fn parseSwitch(self: *Self) ParseError!StmtIdx {
        try self.enterNesting();
        errdefer self.exitNesting();

        const loc = self.currentLoc();
        _ = try self.consume(.kw_switch, "Expected 'switch'");

        // Zig-style: require parentheses around scrutinee
        _ = try self.consume(.lparen, "Expected '(' after 'switch'");
        // Disable struct init parsing to avoid ambiguity with < operator
        const prev_allow_struct_init = self.allow_struct_init;
        self.allow_struct_init = false;
        const scrutinee = try self.parseExpression();
        self.allow_struct_init = prev_allow_struct_init;
        _ = try self.consume(.rparen, "Expected ')' after switch expression");

        return self.parseSwitchBody(scrutinee, loc);
    }

    /// Switch body parsing
    fn parseSwitchBody(self: *Self, scrutinee: ExprIdx, loc: SourceLoc) ParseError!StmtIdx {
        // Note: enterNesting already called by caller

        _ = try self.consume(.lbrace, "Expected '{'");

        // Parse match arms into scratch buffer
        try self.store.markScratch();
        errdefer self.store.rollbackScratch();

        while (!self.check(.rbrace) and !self.isAtEnd()) {
            // Parse pattern (expression, identifier, or 'else' for default)
            var pattern: ExprIdx = undefined;
            if (self.match(&[_]TokenType{.kw_else})) {
                // Default case - use null as pattern (will be handled by lowerer)
                pattern = .null;
            } else {
                pattern = try self.parseExpression();
            }

            _ = try self.consume(.fat_arrow, "Expected '=>'");

            // Parse arm body
            var arm_body: StmtIdx = undefined;
            if (self.check(.lbrace)) {
                _ = self.advance();
                arm_body = try self.parseBlock();
            } else {
                // Single expression as body
                const expr = try self.parseExpression();
                arm_body = self.store.addExprStmt(expr, self.currentLoc()) catch return error.OutOfMemory;
            }

            // Store as [pattern_expr, body_stmt] pairs
            self.store.pushScratchU32(pattern.toInt()) catch return error.OutOfMemory;
            self.store.pushScratchU32(arm_body.toInt()) catch return error.OutOfMemory;

            // Optional comma
            _ = self.match(&[_]TokenType{.comma});
        }

        _ = try self.consume(.rbrace, "Expected '}'");

        const arms = self.store.getScratchU32s();
        self.store.commitScratch();

        // Store match statement
        const arms_start = self.store.extra_data.items.len;
        self.store.extra_data.append(self.allocator, @intCast(arms.len / 2)) catch return error.OutOfMemory; // arm count
        for (arms) |a| {
            self.store.extra_data.append(self.allocator, a) catch return error.OutOfMemory;
        }

        const idx: StmtIdx = @enumFromInt(@as(u32, @intCast(self.store.stmt_tags.items.len)));
        self.store.stmt_tags.append(self.allocator, .match_stmt) catch return error.OutOfMemory;
        self.store.stmt_locs.append(self.allocator, loc) catch return error.OutOfMemory;
        self.store.stmt_data.append(self.allocator, .{
            .a = scrutinee.toInt(),
            .b = @intCast(arms_start),
        }) catch return error.OutOfMemory;

        self.exitNesting();
        return idx;
    }

    fn parseFor(self: *Self) ParseError!StmtIdx {
        try self.enterNesting();
        errdefer self.exitNesting();

        const loc = self.currentLoc();
        _ = try self.consume(.kw_for, "Expected 'for'");
        const binding_token = try self.consume(.identifier, "Expected loop variable");
        const binding = self.internString(binding_token.lexeme) catch return error.OutOfMemory;
        _ = try self.consume(.kw_in, "Expected 'in'");

        // Disable struct init parsing for iterable to avoid ambiguity with body block
        const prev_allow_struct_init = self.allow_struct_init;
        self.allow_struct_init = false;
        const iterable = try self.parseExpression();
        self.allow_struct_init = prev_allow_struct_init;

        _ = try self.consume(.lbrace, "Expected '{'");
        const body = try self.parseBlock();

        self.exitNesting();
        return self.store.addForStmt(binding, iterable, body, loc) catch return error.OutOfMemory;
    }

    fn parseWhile(self: *Self) ParseError!StmtIdx {
        try self.enterNesting();
        errdefer self.exitNesting();

        const loc = self.currentLoc();
        _ = try self.consume(.kw_while, "Expected 'while'");
        _ = try self.consume(.lparen, "Expected '(' after 'while'");

        // Disable struct init parsing for condition to avoid ambiguity with < operator
        // e.g., "while (i < 10)" should not be parsed as "while (i<type>)"
        const prev_allow_struct_init = self.allow_struct_init;
        self.allow_struct_init = false;
        const condition = try self.parseExpression();
        self.allow_struct_init = prev_allow_struct_init;

        _ = try self.consume(.rparen, "Expected ')' after condition");

        _ = try self.consume(.lbrace, "Expected '{'");
        const body = try self.parseBlock();

        self.exitNesting();
        return self.store.addWhileStmt(condition, body, loc) catch return error.OutOfMemory;
    }

    fn parseLoop(self: *Self) ParseError!StmtIdx {
        try self.enterNesting();
        errdefer self.exitNesting();

        const loc = self.currentLoc();
        _ = try self.consume(.kw_loop, "Expected 'loop'");

        _ = try self.consume(.lbrace, "Expected '{'");
        const body = try self.parseBlock();

        self.exitNesting();
        return self.store.addLoopStmt(body, loc) catch return error.OutOfMemory;
    }

    fn parseBreak(self: *Self) ParseError!StmtIdx {
        const loc = self.currentLoc();
        _ = try self.consume(.kw_break, "Expected 'break'");
        // Optionally consume semicolon
        _ = self.match(&[_]TokenType{.semicolon});
        return self.store.addBreak(loc) catch return error.OutOfMemory;
    }

    fn parseContinue(self: *Self) ParseError!StmtIdx {
        const loc = self.currentLoc();
        _ = try self.consume(.kw_continue, "Expected 'continue'");
        // Optionally consume semicolon
        _ = self.match(&[_]TokenType{.semicolon});
        return self.store.addContinue(loc) catch return error.OutOfMemory;
    }

    fn parseReturn(self: *Self) ParseError!StmtIdx {
        const loc = self.currentLoc();
        _ = try self.consume(.kw_return, "Expected 'return'");

        var value: ExprIdx = .null;
        if (!self.check(.rbrace) and !self.check(.semicolon) and !self.isAtEnd()) {
            // Check if there's actually an expression following
            const next = self.peek().type;
            if (next != .kw_fn and next != .kw_struct and next != .kw_const and
                next != .kw_var and next != .kw_if and next != .kw_for and
                next != .kw_while and next != .kw_loop and next != .kw_return)
            {
                // Note: We DON'T disable allow_struct_init here because:
                // - "return n < 10" is distinguished from "return Type<Args>{}" by checking
                //   if the identifier starts with uppercase (type naming convention)
                // - See parseUnary() around line 1604 where is_type_name check prevents
                //   lowercase variables from being parsed as generic struct inits
                value = try self.parseExpression();
            }
        }

        // Optionally consume semicolon
        _ = self.match(&[_]TokenType{.semicolon});

        return self.store.addReturn(value, loc) catch return error.OutOfMemory;
    }

    fn parseTry(self: *Self) ParseError!StmtIdx {
        try self.enterNesting();
        errdefer self.exitNesting();

        const loc = self.currentLoc();
        _ = try self.consume(.kw_try, "Expected 'try'");

        _ = try self.consume(.lbrace, "Expected '{'");
        const try_body = try self.parseBlock();

        _ = try self.consume(.kw_catch, "Expected 'catch'");

        // Optional error binding: catch (err)
        var err_binding: StringId = .null_id;
        if (self.match(&[_]TokenType{.lparen})) {
            const err_token = try self.consume(.identifier, "Expected error variable");
            err_binding = self.internString(err_token.lexeme) catch return error.OutOfMemory;
            _ = try self.consume(.rparen, "Expected ')'");
        }

        _ = try self.consume(.lbrace, "Expected '{'");
        const catch_body = try self.parseBlock();

        // Note: finally was removed from Cot - use defer instead
        self.exitNesting();
        return self.store.addTryStmt(try_body, err_binding, catch_body, .null, loc) catch return error.OutOfMemory;
    }

    fn parseThrow(self: *Self) ParseError!StmtIdx {
        const loc = self.currentLoc();
        _ = try self.consume(.kw_throw, "Expected 'throw'");
        const error_expr = try self.parseExpression();

        const idx: StmtIdx = @enumFromInt(@as(u32, @intCast(self.store.stmt_tags.items.len)));
        self.store.stmt_tags.append(self.allocator, .throw_stmt) catch return error.OutOfMemory;
        self.store.stmt_locs.append(self.allocator, loc) catch return error.OutOfMemory;
        self.store.stmt_data.append(self.allocator, .{
            .a = error_expr.toInt(),
            .b = 0,
        }) catch return error.OutOfMemory;

        return idx;
    }

    /// Parse defer statement: defer expr or defer { block }
    /// The deferred code runs at scope exit (in reverse order of declaration)
    fn parseDefer(self: *Self) ParseError!StmtIdx {
        const loc = self.currentLoc();
        _ = try self.consume(.kw_defer, "Expected 'defer'");

        // Check if it's a block or expression
        const body = if (self.check(.lbrace)) blk: {
            _ = try self.consume(.lbrace, "Expected '{'");
            break :blk try self.parseBlock();
        } else blk: {
            // Single expression - wrap as expression statement
            const expr = try self.parseExpression();
            break :blk self.store.addExprStmt(expr, loc) catch return error.OutOfMemory;
        };

        return self.store.addDeferStmt(body, loc) catch return error.OutOfMemory;
    }

    fn parseComptime(self: *Self) ParseError!StmtIdx {
        const loc = self.currentLoc();
        _ = try self.consume(.kw_comptime, "Expected 'comptime'");

        // Check for comptime if or comptime block
        if (self.check(.kw_if)) {
            return self.parseComptimeIf(loc);
        } else {
            return self.parseComptimeBlock(loc);
        }
    }

    fn parseComptimeIf(self: *Self, loc: SourceLoc) ParseError!StmtIdx {
        try self.enterNesting();
        errdefer self.exitNesting();

        _ = try self.consume(.kw_if, "Expected 'if'");
        _ = try self.consume(.lparen, "Expected '(' after 'if'");
        const condition = try self.parseExpression();
        _ = try self.consume(.rparen, "Expected ')' after condition");

        _ = try self.consume(.lbrace, "Expected '{'");
        const then_body = try self.parseBlock();

        var else_body: StmtIdx = .null;
        if (self.match(&[_]TokenType{.kw_else})) {
            if (self.check(.kw_if)) {
                // comptime else if
                const else_loc = self.currentLoc();
                else_body = try self.parseComptimeIf(else_loc);
            } else {
                _ = try self.consume(.lbrace, "Expected '{'");
                else_body = try self.parseBlock();
            }
        }

        // Store comptime_if
        const extra_start = self.store.extra_data.items.len;
        self.store.extra_data.append(self.allocator, else_body.toInt()) catch return error.OutOfMemory;

        const idx: StmtIdx = @enumFromInt(@as(u32, @intCast(self.store.stmt_tags.items.len)));
        self.store.stmt_tags.append(self.allocator, .comptime_if) catch return error.OutOfMemory;
        self.store.stmt_locs.append(self.allocator, loc) catch return error.OutOfMemory;
        self.store.stmt_data.append(self.allocator, .{
            .a = condition.toInt(),
            .b = (then_body.toInt() << 16) | @as(u32, @intCast(extra_start)),
        }) catch return error.OutOfMemory;

        self.exitNesting();
        return idx;
    }

    fn parseComptimeBlock(self: *Self, loc: SourceLoc) ParseError!StmtIdx {
        _ = try self.consume(.lbrace, "Expected '{'");
        const body = try self.parseBlock();

        const idx: StmtIdx = @enumFromInt(@as(u32, @intCast(self.store.stmt_tags.items.len)));
        self.store.stmt_tags.append(self.allocator, .comptime_block) catch return error.OutOfMemory;
        self.store.stmt_locs.append(self.allocator, loc) catch return error.OutOfMemory;
        self.store.stmt_data.append(self.allocator, .{
            .a = body.toInt(),
            .b = 0,
        }) catch return error.OutOfMemory;

        return idx;
    }

    fn parseExpressionStatement(self: *Self) ParseError!StmtIdx {
        const loc = self.currentLoc();
        const expr = try self.parseExpression();

        // Check for assignment
        if (self.match(&[_]TokenType{.equals})) {
            const value = try self.parseExpression();
            // Optionally consume semicolon
            _ = self.match(&[_]TokenType{.semicolon});
            return self.store.addAssignment(expr, value, loc) catch return error.OutOfMemory;
        }

        // Check for compound assignment
        if (self.match(&[_]TokenType{ .plus_equals, .minus_equals, .star_equals, .slash_equals, .pipe_equals, .amp_equals })) {
            const op: BinaryOp = switch (self.previous().type) {
                .plus_equals => .add,
                .minus_equals => .sub,
                .star_equals => .mul,
                .slash_equals => .div,
                .pipe_equals => .bit_or,
                .amp_equals => .bit_and,
                else => unreachable,
            };
            const rhs = try self.parseExpression();
            // Desugar x += y to x = x + y
            const binop = self.store.addBinary(expr, op, rhs, loc) catch return error.OutOfMemory;
            // Optionally consume semicolon
            _ = self.match(&[_]TokenType{.semicolon});
            return self.store.addAssignment(expr, binop, loc) catch return error.OutOfMemory;
        }

        // Create expression statement
        const stmt = self.store.addExprStmt(expr, loc) catch return error.OutOfMemory;

        // Optionally consume semicolon (allows both `expr` and `expr;` syntax)
        _ = self.match(&[_]TokenType{.semicolon});

        return stmt;
    }

    // ============================================================
    // Expressions (Pratt Parser)
    // ============================================================
    //
    // Uses a table-driven Pratt parsing algorithm instead of
    // separate functions for each precedence level.
    // See pratt.zig for precedence definitions.

    /// Parse an expression with default (lowest) precedence
    fn parseExpression(self: *Self) ParseError!ExprIdx {
        return self.parseExpressionPrec(.assignment);
    }

    /// Parse an expression with minimum precedence level
    /// This is the core Pratt parsing algorithm
    fn parseExpressionPrec(self: *Self, min_prec: Precedence) ParseError!ExprIdx {
        // Parse prefix expression (unary or primary)
        var left = try self.parsePrefixExpr();

        // Parse infix operators while precedence is high enough
        while (true) {
            const current_prec = pratt.getPrecedence(self.peek().type);

            // Stop if current operator has lower precedence than minimum
            if (@intFromEnum(current_prec) <= @intFromEnum(min_prec)) {
                break;
            }

            // Handle postfix operators (call, index, member access)
            if (pratt.isPostfixOperator(self.peek().type)) {
                left = try self.parsePostfixOp(left);
                continue;
            }

            // Handle cast operator (as)
            if (pratt.isCastOperator(self.peek().type)) {
                left = try self.parseCastOp(left);
                continue;
            }

            // Handle type test operator (is)
            if (pratt.isTypeTestOperator(self.peek().type)) {
                left = try self.parseIsOp(left);
                continue;
            }

            // Handle binary operators
            if (pratt.isBinaryOperator(self.peek().type)) {
                left = try self.parseBinaryOp(left, current_prec);
                continue;
            }

            // No more operators at this precedence
            break;
        }

        return left;
    }

    /// Parse a prefix expression (unary operator or primary)
    /// This includes parsing any postfix operators that follow the primary
    fn parsePrefixExpr(self: *Self) ParseError!ExprIdx {
        const token_type = self.peek().type;

        // Unary operators
        if (pratt.tokenToUnaryOp(token_type)) |op| {
            // Handle minus specially - could be unary or binary
            // In prefix position, it's always unary
            _ = self.advance();
            const loc = self.currentLoc();
            // Parse operand including any postfix operators like .method()
            const operand = try self.parsePrefixExpr();
            return self.store.addUnary(op, operand, loc) catch return error.OutOfMemory;
        }

        // Primary expression followed by any postfix operators
        var result = try self.parsePrimary();

        // Parse postfix operators (call, index, member access)
        while (pratt.isPostfixOperator(self.peek().type)) {
            result = try self.parsePostfixOp(result);
        }

        return result;
    }

    /// Parse a binary operator and right-hand side
    fn parseBinaryOp(self: *Self, left: ExprIdx, prec: Precedence) ParseError!ExprIdx {
        const op_token = self.advance();
        const loc = self.currentLoc();

        const op = pratt.tokenToBinaryOp(op_token.type) orelse {
            self.addError("Expected binary operator");
            return error.UnexpectedToken;
        };

        // Right-associative operators would use `prec`, left-associative use `prec.next()`
        // All our binary operators are left-associative
        const right = try self.parseExpressionPrec(prec);

        return self.store.addBinary(left, op, right, loc) catch return error.OutOfMemory;
    }

    /// Parse a cast operator: expr as type
    /// Converts to a call to cast_alpha, cast_decimal, or cast_integer
    fn parseCastOp(self: *Self, left: ExprIdx) ParseError!ExprIdx {
        _ = self.advance(); // consume 'as'
        const loc = self.currentLoc();

        // Parse the target type name
        const type_token = try self.consume(.identifier, "Expected type name after 'as'");
        const type_name = type_token.lexeme;

        // Map type name to cast function
        // For enum types or i64, we use cast_integer since enums are integers
        const cast_func: []const u8 = if (std.mem.eql(u8, type_name, "alpha") or std.mem.eql(u8, type_name, "string"))
            "cast_alpha"
        else if (std.mem.eql(u8, type_name, "decimal") or std.mem.eql(u8, type_name, "f64"))
            "cast_decimal"
        else
            // All other types (integer, int, i64, i32, u64, u32, or enum types) use cast_integer
            // Enum values are integers, so casting to/from enum is effectively integer cast
            "cast_integer";

        // Create call expression: cast_func(left)
        const func_id = self.internString(cast_func) catch return error.OutOfMemory;
        const func_expr = self.store.addIdentifier(func_id, loc) catch return error.OutOfMemory;

        // Create single-element args array
        var args_buf: [1]ExprIdx = .{left};
        const args = self.allocator.dupe(ExprIdx, &args_buf) catch return error.OutOfMemory;

        return self.store.addCall(func_expr, args, loc) catch return error.OutOfMemory;
    }

    /// Parse a type test operator: expr is Type
    /// Returns a boolean expression that tests if expr is of type Type
    fn parseIsOp(self: *Self, left: ExprIdx) ParseError!ExprIdx {
        _ = self.advance(); // consume 'is'
        const loc = self.currentLoc();

        // Parse the target type
        const type_idx = try self.parseType();

        return self.store.addIsExpr(left, type_idx, loc) catch return error.OutOfMemory;
    }

    /// Parse postfix operators (call, index, member access)
    fn parsePostfixOp(self: *Self, expr: ExprIdx) ParseError!ExprIdx {
        var result = expr;

        while (true) {
            if (self.match(&[_]TokenType{.lparen})) {
                result = try self.finishCall(result);
            } else if (self.match(&[_]TokenType{.period})) {
                // Check for pointer dereference: expr.*
                if (self.match(&[_]TokenType{.star})) {
                    result = self.store.addUnary(.deref, result, self.currentLoc()) catch return error.OutOfMemory;
                } else {
                    const field_token = try self.consume(.identifier, "Expected field name");
                    const field = self.internString(field_token.lexeme) catch return error.OutOfMemory;
                    result = self.store.addMember(result, field, self.currentLoc()) catch return error.OutOfMemory;
                }
            } else if (self.match(&[_]TokenType{.lbracket})) {
                try self.enterNesting();
                errdefer self.exitNesting();

                // Parse at precedence above range to stop before '..' operator
                const start_expr = try self.parseExpressionPrec(.range);

                // Check for slice syntax: expr[start..end] or expr[start..=end]
                if (self.match(&[_]TokenType{ .range, .range_inclusive })) {
                    const inclusive = self.previous().type == .range_inclusive;
                    // Parse end expression (can be any expression)
                    const end_expr = try self.parseExpression();
                    _ = try self.consume(.rbracket, "Expected ']'");
                    self.exitNesting();
                    result = self.store.addSliceExpr(result, start_expr, end_expr, inclusive, self.currentLoc()) catch return error.OutOfMemory;
                } else {
                    // Regular index expression: expr[index]
                    _ = try self.consume(.rbracket, "Expected ']'");
                    self.exitNesting();
                    result = self.store.addIndex(result, start_expr, self.currentLoc()) catch return error.OutOfMemory;
                }
            } else if (self.match(&[_]TokenType{.question_dot})) {
                // Optional member access: expr?.field (null-safe)
                const field_token = try self.consume(.identifier, "Expected field name");
                const field = self.internString(field_token.lexeme) catch return error.OutOfMemory;
                result = self.store.addOptionalMember(result, field, self.currentLoc()) catch return error.OutOfMemory;
            } else if (self.match(&[_]TokenType{.question_lbracket})) {
                // Optional index access: expr?[index] (null-safe, returns null on out-of-bounds)
                try self.enterNesting();
                errdefer self.exitNesting();
                const index_expr = try self.parseExpression();
                _ = try self.consume(.rbracket, "Expected ']'");
                self.exitNesting();
                result = self.store.addOptionalIndex(result, index_expr, self.currentLoc()) catch return error.OutOfMemory;
            } else {
                break;
            }
        }

        return result;
    }

    fn finishCall(self: *Self, callee: ExprIdx) ParseError!ExprIdx {
        try self.enterNesting();
        errdefer self.exitNesting();

        const loc = self.currentLoc();

        // Collect arguments
        try self.store.markScratch();
        errdefer self.store.rollbackScratch();

        if (!self.check(.rparen)) {
            while (true) {
                const arg = try self.parseExpression();
                self.store.pushScratchExpr(arg) catch return error.OutOfMemory;
                if (!self.match(&[_]TokenType{.comma})) break;
            }
        }

        _ = try self.consume(.rparen, "Expected ')'");

        const args = self.store.getScratchExprs();
        const args_copy = self.allocator.dupe(ExprIdx, args) catch return error.OutOfMemory;
        self.store.commitScratch();

        self.exitNesting();
        return self.store.addCall(callee, args_copy, loc) catch return error.OutOfMemory;
    }

    fn parsePrimary(self: *Self) ParseError!ExprIdx {
        const loc = self.currentLoc();

        // Boolean literals
        if (self.match(&[_]TokenType{.kw_true})) {
            return self.store.addBoolLiteral(true, loc) catch return error.OutOfMemory;
        }
        if (self.match(&[_]TokenType{.kw_false})) {
            return self.store.addBoolLiteral(false, loc) catch return error.OutOfMemory;
        }

        // Null literal
        if (self.match(&[_]TokenType{.kw_null})) {
            return self.store.addNullLiteral(loc) catch return error.OutOfMemory;
        }

        // Integer literal
        if (self.match(&[_]TokenType{.integer_literal})) {
            const value = std.fmt.parseInt(i64, self.previous().lexeme, 10) catch 0;
            return self.store.addIntLiteral(value, loc) catch return error.OutOfMemory;
        }

        // Decimal/float literal
        if (self.match(&[_]TokenType{.decimal_literal})) {
            const value = std.fmt.parseFloat(f64, self.previous().lexeme) catch 0.0;
            return self.store.addFloatLiteral(value, loc) catch return error.OutOfMemory;
        }

        // String literal
        if (self.match(&[_]TokenType{.string_literal})) {
            const lexeme = self.previous().lexeme;
            const str = if (lexeme.len >= 2) lexeme[1 .. lexeme.len - 1] else lexeme;
            const str_id = self.internString(str) catch return error.OutOfMemory;
            return self.store.addStringLiteral(str_id, loc) catch return error.OutOfMemory;
        }

        // Interpolated string: "Hello ${name}!"
        if (self.match(&[_]TokenType{.string_interp_start})) {
            return self.parseInterpString(loc);
        }

        // Identifier
        if (self.match(&[_]TokenType{.identifier})) {
            const name = self.internString(self.previous().lexeme) catch return error.OutOfMemory;

            // Check for generic struct initializer: Name<T> { ... }
            // or regular struct initializer: Name { ... }
            if (self.allow_struct_init) {
                if (self.check(.lt)) {
                    // Could be a generic struct init: Box<i64> { ... }
                    // Only treat as generic struct init if the identifier starts with uppercase
                    // (type naming convention). This avoids "n < 10" being parsed as "n<type>".
                    const lexeme = self.previous().lexeme;
                    const is_type_name = lexeme.len > 0 and lexeme[0] >= 'A' and lexeme[0] <= 'Z';
                    if (is_type_name) {
                        return self.parseGenericStructInit(name, loc);
                    }
                } else if (self.check(.lbrace)) {
                    return self.parseStructInit(name, loc);
                }
            }

            return self.store.addIdentifier(name, loc) catch return error.OutOfMemory;
        }

        // Self
        if (self.match(&[_]TokenType{.kw_self})) {
            const name = self.internString("self") catch return error.OutOfMemory;
            return self.store.addIdentifier(name, loc) catch return error.OutOfMemory;
        }

        // New expression: new Type{ ... } or new Type<T>{ ... } or new Type<T>
        if (self.match(&[_]TokenType{.kw_new})) {
            return self.parseNewExpr(loc);
        }

        // Grouping or tuple
        if (self.match(&[_]TokenType{.lparen})) {
            try self.enterNesting();
            errdefer self.exitNesting();
            const inner = try self.parseExpression();
            _ = try self.consume(.rparen, "Expected ')'");
            self.exitNesting();
            return self.store.addGrouping(inner, loc) catch return error.OutOfMemory;
        }

        // Array literal: [1, 2, 3]
        if (self.match(&[_]TokenType{.lbracket})) {
            return self.parseArrayLiteral(loc);
        }

        // Lambda: |x| expr or |x, y| { ... }
        if (self.match(&[_]TokenType{.pipe})) {
            return self.parseLambda(loc);
        }

        // Range expression starting with ..
        if (self.match(&[_]TokenType{ .range, .range_inclusive })) {
            const inclusive = self.previous().type == .range_inclusive;
            const end = try self.parseExpression();
            return self.parseRangeExpr(.null, end, inclusive, loc);
        }

        // Comptime builtin: @name() or @name(args)
        if (self.match(&[_]TokenType{.at})) {
            return self.parseComptimeBuiltin(loc);
        }

        self.addError("Expected expression");
        return error.ExpectedExpression;
    }

    /// Parse a comptime builtin expression: @name() or @name(args)
    fn parseComptimeBuiltin(self: *Self, loc: SourceLoc) ParseError!ExprIdx {
        // Expect an identifier after @
        const name_token = try self.consume(.identifier, "Expected builtin name after '@'");
        const name = self.internString(name_token.lexeme) catch return error.OutOfMemory;

        // Expect opening parenthesis
        _ = try self.consume(.lparen, "Expected '(' after builtin name");

        // Parse arguments
        try self.store.markScratch();
        errdefer self.store.rollbackScratch();

        if (!self.check(.rparen)) {
            while (true) {
                const arg = try self.parseExpression();
                self.store.pushScratchExpr(arg) catch return error.OutOfMemory;
                if (!self.match(&[_]TokenType{.comma})) break;
            }
        }

        _ = try self.consume(.rparen, "Expected ')'");

        const args = self.store.getScratchExprs();
        self.store.commitScratch();

        // Create array from scratch exprs
        var args_array: std.ArrayListUnmanaged(ExprIdx) = .{};
        errdefer args_array.deinit(self.allocator);

        for (args) |arg_expr| {
            args_array.append(self.allocator, arg_expr) catch return error.OutOfMemory;
        }

        return self.store.addComptimeBuiltin(name, args_array.items, loc) catch return error.OutOfMemory;
    }

    fn parseArrayLiteral(self: *Self, loc: SourceLoc) ParseError!ExprIdx {
        try self.enterNesting();
        errdefer self.exitNesting();

        try self.store.markScratch();
        errdefer self.store.rollbackScratch();

        if (!self.check(.rbracket)) {
            while (true) {
                const elem = try self.parseExpression();
                self.store.pushScratchExpr(elem) catch return error.OutOfMemory;
                if (!self.match(&[_]TokenType{.comma})) break;
            }
        }

        _ = try self.consume(.rbracket, "Expected ']'");

        const elems = self.store.getScratchExprs();
        self.store.commitScratch();

        // Store array_init
        const elems_start = self.store.extra_data.items.len;
        self.store.extra_data.append(self.allocator, @intCast(elems.len)) catch return error.OutOfMemory;
        for (elems) |e| {
            self.store.extra_data.append(self.allocator, e.toInt()) catch return error.OutOfMemory;
        }

        const idx: ExprIdx = @enumFromInt(@as(u32, @intCast(self.store.expr_tags.items.len)));
        self.store.expr_tags.append(self.allocator, .array_init) catch return error.OutOfMemory;
        self.store.expr_locs.append(self.allocator, loc) catch return error.OutOfMemory;
        self.store.expr_data.append(self.allocator, .{
            .a = @intCast(elems_start),
            .b = @intCast(elems.len),
        }) catch return error.OutOfMemory;

        self.exitNesting();
        return idx;
    }

    fn parseStructInit(self: *Self, type_name: StringId, loc: SourceLoc) ParseError!ExprIdx {
        _ = try self.consume(.lbrace, "Expected '{'");

        try self.enterNesting();
        errdefer self.exitNesting();

        try self.store.markScratch();
        errdefer self.store.rollbackScratch();

        while (!self.check(.rbrace) and !self.isAtEnd()) {
            // Parse .field = value
            _ = try self.consume(.period, "Expected '.'");
            const field_token = try self.consume(.identifier, "Expected field name");
            const field_name = self.internString(field_token.lexeme) catch return error.OutOfMemory;
            _ = try self.consume(.equals, "Expected '='");
            const value = try self.parseExpression();

            self.store.pushScratchU32(@intFromEnum(field_name)) catch return error.OutOfMemory;
            self.store.pushScratchU32(value.toInt()) catch return error.OutOfMemory;

            // Optional comma
            _ = self.match(&[_]TokenType{.comma});
        }

        _ = try self.consume(.rbrace, "Expected '}'");

        const fields = self.store.getScratchU32s();
        self.store.commitScratch();

        // Store struct_init
        const fields_start = self.store.extra_data.items.len;
        self.store.extra_data.append(self.allocator, @intCast(fields.len / 2)) catch return error.OutOfMemory;
        for (fields) |f| {
            self.store.extra_data.append(self.allocator, f) catch return error.OutOfMemory;
        }

        const idx: ExprIdx = @enumFromInt(@as(u32, @intCast(self.store.expr_tags.items.len)));
        self.store.expr_tags.append(self.allocator, .struct_init) catch return error.OutOfMemory;
        self.store.expr_locs.append(self.allocator, loc) catch return error.OutOfMemory;
        self.store.expr_data.append(self.allocator, .{
            .a = @intFromEnum(type_name),
            .b = @intCast(fields_start),
        }) catch return error.OutOfMemory;

        self.exitNesting();
        return idx;
    }

    /// Parse a generic struct initializer: Box<i64>{ .value = 42 }
    fn parseGenericStructInit(self: *Self, type_name: StringId, loc: SourceLoc) ParseError!ExprIdx {
        // Parse type arguments: <T, U, ...>
        _ = try self.consume(.lt, "Expected '<'");

        try self.store.markScratch();
        errdefer self.store.rollbackScratch();

        // Collect type arguments
        var type_arg_count: u32 = 0;
        while (!self.checkGt() and !self.isAtEnd()) {
            const type_arg = try self.parseType();
            self.store.pushScratchU32(type_arg.toInt()) catch return error.OutOfMemory;
            type_arg_count += 1;
            if (!self.match(&[_]TokenType{.comma})) break;
        }

        _ = try self.consumeGt("Expected '>' after type arguments");

        // Now parse the struct initializer body: { .field = value, ... }
        _ = try self.consume(.lbrace, "Expected '{' after type arguments in struct initializer");

        try self.enterNesting();
        errdefer self.exitNesting();

        // Parse field initializers
        var field_count: u32 = 0;
        while (!self.check(.rbrace) and !self.isAtEnd()) {
            // Parse .field = value
            _ = try self.consume(.period, "Expected '.'");
            const field_token = try self.consume(.identifier, "Expected field name");
            const field_name = self.internString(field_token.lexeme) catch return error.OutOfMemory;
            _ = try self.consume(.equals, "Expected '='");
            const value = try self.parseExpression();

            self.store.pushScratchU32(@intFromEnum(field_name)) catch return error.OutOfMemory;
            self.store.pushScratchU32(value.toInt()) catch return error.OutOfMemory;
            field_count += 1;

            // Optional comma
            _ = self.match(&[_]TokenType{.comma});
        }

        _ = try self.consume(.rbrace, "Expected '}'");

        const scratch_data = self.store.getScratchU32s();
        self.store.commitScratch();

        // Store generic_struct_init in extra_data:
        // [type_arg_count, type_args..., field_count, field_name/value pairs...]
        const extra_start = self.store.extra_data.items.len;
        self.store.extra_data.append(self.allocator, type_arg_count) catch return error.OutOfMemory;
        // Type args are at the beginning of scratch_data
        for (scratch_data[0..type_arg_count]) |type_arg_raw| {
            self.store.extra_data.append(self.allocator, type_arg_raw) catch return error.OutOfMemory;
        }
        self.store.extra_data.append(self.allocator, field_count) catch return error.OutOfMemory;
        // Field data starts after type args (each field is 2 u32s: name, value)
        for (scratch_data[type_arg_count .. type_arg_count + field_count * 2]) |f| {
            self.store.extra_data.append(self.allocator, f) catch return error.OutOfMemory;
        }

        const idx: ExprIdx = @enumFromInt(@as(u32, @intCast(self.store.expr_tags.items.len)));
        self.store.expr_tags.append(self.allocator, .generic_struct_init) catch return error.OutOfMemory;
        self.store.expr_locs.append(self.allocator, loc) catch return error.OutOfMemory;
        self.store.expr_data.append(self.allocator, .{
            .a = @intFromEnum(type_name),
            .b = @intCast(extra_start),
        }) catch return error.OutOfMemory;

        self.exitNesting();
        return idx;
    }

    /// Parse a new expression: new Type{ ... } or new Type<T>{ ... } or new Type<T>
    fn parseNewExpr(self: *Self, loc: SourceLoc) ParseError!ExprIdx {
        // Expect type name
        const type_token = try self.consume(.identifier, "Expected type name after 'new'");
        const type_name = self.internString(type_token.lexeme) catch return error.OutOfMemory;

        // Check for generic type arguments: <T, U, ...>
        const has_type_args = self.check(.lt);

        // Check if we have type args or a brace (for struct init)
        if (has_type_args) {
            // Parse generic: new Box<i64>{ ... } or new List<i64>
            _ = try self.consume(.lt, "Expected '<'");

            try self.store.markScratch();
            errdefer self.store.rollbackScratch();

            // Collect type arguments
            var type_arg_count: u32 = 0;
            while (!self.checkGt() and !self.isAtEnd()) {
                const type_arg = try self.parseType();
                self.store.pushScratchU32(type_arg.toInt()) catch return error.OutOfMemory;
                type_arg_count += 1;
                if (!self.match(&[_]TokenType{.comma})) break;
            }

            _ = try self.consumeGt("Expected '>' after type arguments");

            const type_args = self.store.getScratchU32s();

            // Check if we have an initializer
            if (self.check(.lbrace)) {
                // Parse struct/collection initializer
                _ = try self.consume(.lbrace, "Expected '{'");
                try self.enterNesting();
                errdefer self.exitNesting();

                // Check if it's field-style (.field = value) or element-style (value, value)
                if (self.check(.period)) {
                    // Struct-style: { .field = value, ... }
                    try self.store.markScratch();
                    errdefer self.store.rollbackScratch();

                    while (!self.check(.rbrace) and !self.isAtEnd()) {
                        _ = try self.consume(.period, "Expected '.'");
                        const field_token = try self.consume(.identifier, "Expected field name");
                        const field_name = self.internString(field_token.lexeme) catch return error.OutOfMemory;
                        _ = try self.consume(.equals, "Expected '='");
                        const value = try self.parseExpression();

                        self.store.pushScratchU32(@intFromEnum(field_name)) catch return error.OutOfMemory;
                        self.store.pushScratchU32(value.toInt()) catch return error.OutOfMemory;

                        _ = self.match(&[_]TokenType{.comma});
                    }

                    _ = try self.consume(.rbrace, "Expected '}'");

                    const fields = self.store.getScratchU32s();
                    self.store.commitScratch();

                    // Store new_expr with type args and fields
                    const extra_start = self.store.extra_data.items.len;
                    // [type_arg_count, type_args..., field_count, fields...]
                    self.store.extra_data.append(self.allocator, type_arg_count) catch return error.OutOfMemory;
                    for (type_args) |ta| {
                        self.store.extra_data.append(self.allocator, ta) catch return error.OutOfMemory;
                    }
                    self.store.extra_data.append(self.allocator, @intCast(fields.len / 2)) catch return error.OutOfMemory;
                    for (fields) |f| {
                        self.store.extra_data.append(self.allocator, f) catch return error.OutOfMemory;
                    }

                    self.store.rollbackScratch(); // rollback type_args scratch

                    const idx: ExprIdx = @enumFromInt(@as(u32, @intCast(self.store.expr_tags.items.len)));
                    self.store.expr_tags.append(self.allocator, .new_expr) catch return error.OutOfMemory;
                    self.store.expr_locs.append(self.allocator, loc) catch return error.OutOfMemory;
                    self.store.expr_data.append(self.allocator, .{
                        .a = @intFromEnum(type_name),
                        .b = @intCast(extra_start),
                    }) catch return error.OutOfMemory;

                    self.exitNesting();
                    return idx;
                } else {
                    // Collection-style: { value, value, ... } for List
                    try self.store.markScratch();
                    errdefer self.store.rollbackScratch();

                    while (!self.check(.rbrace) and !self.isAtEnd()) {
                        const elem = try self.parseExpression();
                        self.store.pushScratchU32(elem.toInt()) catch return error.OutOfMemory;
                        _ = self.match(&[_]TokenType{.comma});
                    }

                    _ = try self.consume(.rbrace, "Expected '}'");

                    const elems = self.store.getScratchU32s();
                    self.store.commitScratch();

                    // Store new_expr with type args and elements (negative field count indicates elements)
                    const extra_start = self.store.extra_data.items.len;
                    self.store.extra_data.append(self.allocator, type_arg_count) catch return error.OutOfMemory;
                    for (type_args) |ta| {
                        self.store.extra_data.append(self.allocator, ta) catch return error.OutOfMemory;
                    }
                    // Use high bit to indicate element mode vs field mode
                    const elem_count_marker: u32 = 0x80000000 | @as(u32, @intCast(elems.len));
                    self.store.extra_data.append(self.allocator, elem_count_marker) catch return error.OutOfMemory;
                    for (elems) |e| {
                        self.store.extra_data.append(self.allocator, e) catch return error.OutOfMemory;
                    }

                    self.store.rollbackScratch(); // rollback type_args scratch

                    const idx: ExprIdx = @enumFromInt(@as(u32, @intCast(self.store.expr_tags.items.len)));
                    self.store.expr_tags.append(self.allocator, .new_expr) catch return error.OutOfMemory;
                    self.store.expr_locs.append(self.allocator, loc) catch return error.OutOfMemory;
                    self.store.expr_data.append(self.allocator, .{
                        .a = @intFromEnum(type_name),
                        .b = @intCast(extra_start),
                    }) catch return error.OutOfMemory;

                    self.exitNesting();
                    return idx;
                }
            } else {
                // No brace - empty collection: new List<i64>
                const extra_start = self.store.extra_data.items.len;
                self.store.extra_data.append(self.allocator, type_arg_count) catch return error.OutOfMemory;
                for (type_args) |ta| {
                    self.store.extra_data.append(self.allocator, ta) catch return error.OutOfMemory;
                }
                self.store.extra_data.append(self.allocator, 0) catch return error.OutOfMemory; // no fields/elements

                self.store.commitScratch();

                const idx: ExprIdx = @enumFromInt(@as(u32, @intCast(self.store.expr_tags.items.len)));
                self.store.expr_tags.append(self.allocator, .new_expr) catch return error.OutOfMemory;
                self.store.expr_locs.append(self.allocator, loc) catch return error.OutOfMemory;
                self.store.expr_data.append(self.allocator, .{
                    .a = @intFromEnum(type_name),
                    .b = @intCast(extra_start),
                }) catch return error.OutOfMemory;

                return idx;
            }
        } else if (self.check(.lbrace)) {
            // Non-generic struct init: new Point{ .x = 1 }
            _ = try self.consume(.lbrace, "Expected '{'");
            try self.enterNesting();
            errdefer self.exitNesting();

            try self.store.markScratch();
            errdefer self.store.rollbackScratch();

            while (!self.check(.rbrace) and !self.isAtEnd()) {
                _ = try self.consume(.period, "Expected '.'");
                const field_token = try self.consume(.identifier, "Expected field name");
                const field_name = self.internString(field_token.lexeme) catch return error.OutOfMemory;
                _ = try self.consume(.equals, "Expected '='");
                const value = try self.parseExpression();

                self.store.pushScratchU32(@intFromEnum(field_name)) catch return error.OutOfMemory;
                self.store.pushScratchU32(value.toInt()) catch return error.OutOfMemory;

                _ = self.match(&[_]TokenType{.comma});
            }

            _ = try self.consume(.rbrace, "Expected '}'");

            const fields = self.store.getScratchU32s();
            self.store.commitScratch();

            const extra_start = self.store.extra_data.items.len;
            // [type_arg_count=0, field_count, fields...]
            self.store.extra_data.append(self.allocator, 0) catch return error.OutOfMemory; // no type args
            self.store.extra_data.append(self.allocator, @intCast(fields.len / 2)) catch return error.OutOfMemory;
            for (fields) |f| {
                self.store.extra_data.append(self.allocator, f) catch return error.OutOfMemory;
            }

            const idx: ExprIdx = @enumFromInt(@as(u32, @intCast(self.store.expr_tags.items.len)));
            self.store.expr_tags.append(self.allocator, .new_expr) catch return error.OutOfMemory;
            self.store.expr_locs.append(self.allocator, loc) catch return error.OutOfMemory;
            self.store.expr_data.append(self.allocator, .{
                .a = @intFromEnum(type_name),
                .b = @intCast(extra_start),
            }) catch return error.OutOfMemory;

            self.exitNesting();
            return idx;
        } else {
            self.addError("Expected '{' or '<' after type name in new expression");
            return error.InvalidSyntax;
        }
    }

    /// Parse an interpolated string: "Hello ${name}!"
    /// Token stream: string_interp_start, (string_content | interp_expr_start expr interp_expr_end)*, string_interp_end
    fn parseInterpString(self: *Self, loc: SourceLoc) ParseError!ExprIdx {
        // Build parts array: alternating [tag, data] pairs
        // tag 0 = string_content (data = StringId)
        // tag 1 = expression (data = ExprIdx)
        try self.store.markScratch();
        errdefer self.store.rollbackScratch();

        while (!self.check(.string_interp_end) and !self.isAtEnd()) {
            if (self.match(&[_]TokenType{.string_content})) {
                // Literal string part
                const content = self.previous().lexeme;
                const str_id = self.internString(content) catch return error.OutOfMemory;
                self.store.pushScratchU32(0) catch return error.OutOfMemory; // tag = string_content
                self.store.pushScratchU32(@intFromEnum(str_id)) catch return error.OutOfMemory;
            } else if (self.match(&[_]TokenType{.interp_expr_start})) {
                // Expression part: parse until interp_expr_end
                const expr = try self.parseExpression();
                _ = try self.consume(.interp_expr_end, "Expected '}' after interpolation expression");
                self.store.pushScratchU32(1) catch return error.OutOfMemory; // tag = expression
                self.store.pushScratchU32(@intFromEnum(expr)) catch return error.OutOfMemory;
            } else {
                self.addError("Expected string content or interpolation expression");
                return error.InvalidSyntax;
            }
        }

        _ = try self.consume(.string_interp_end, "Expected closing '\"' for interpolated string");

        // Get parts data and create the interp_string node
        const parts_data = self.store.getScratchU32s();
        const idx = self.store.addInterpString(parts_data, loc) catch return error.OutOfMemory;
        self.store.commitScratch();
        return idx;
    }

    fn parseLambda(self: *Self, loc: SourceLoc) ParseError!ExprIdx {
        try self.enterNesting();
        errdefer self.exitNesting();

        // Parse parameters: |x, y|
        try self.store.markScratch();
        errdefer self.store.rollbackScratch();

        if (!self.check(.pipe)) {
            while (true) {
                const param_token = try self.consume(.identifier, "Expected parameter name");
                const param_name = self.internString(param_token.lexeme) catch return error.OutOfMemory;
                self.store.pushScratchU32(@intFromEnum(param_name)) catch return error.OutOfMemory;

                // Optional type
                if (self.match(&[_]TokenType{.colon})) {
                    const param_type = try self.parseType();
                    self.store.pushScratchU32(param_type.toInt()) catch return error.OutOfMemory;
                } else {
                    self.store.pushScratchU32(TypeIdx.null.toInt()) catch return error.OutOfMemory;
                }

                // Direction: in by default for lambdas
                self.store.pushScratchU32(0) catch return error.OutOfMemory;

                if (!self.match(&[_]TokenType{.comma})) break;
            }
        }

        _ = try self.consume(.pipe, "Expected '|'");

        const params = self.store.getScratchU32s();
        self.store.commitScratch();

        // Parse body
        var body: StmtIdx = undefined;
        if (self.check(.lbrace)) {
            _ = self.advance();
            body = try self.parseBlock();
        } else {
            // Single expression body
            const expr = try self.parseExpression();
            body = self.store.addReturn(expr, self.currentLoc()) catch return error.OutOfMemory;
        }

        // Store lambda
        const params_start = self.store.extra_data.items.len;
        // Each param has 3 values: name, type, direction
        self.store.extra_data.append(self.allocator, @intCast(params.len / 3)) catch return error.OutOfMemory;
        for (params) |p| {
            self.store.extra_data.append(self.allocator, p) catch return error.OutOfMemory;
        }
        self.store.extra_data.append(self.allocator, body.toInt()) catch return error.OutOfMemory;

        const idx: ExprIdx = @enumFromInt(@as(u32, @intCast(self.store.expr_tags.items.len)));
        self.store.expr_tags.append(self.allocator, .lambda) catch return error.OutOfMemory;
        self.store.expr_locs.append(self.allocator, loc) catch return error.OutOfMemory;
        self.store.expr_data.append(self.allocator, .{
            .a = @intCast(params_start),
            .b = 0,
        }) catch return error.OutOfMemory;

        self.exitNesting();
        return idx;
    }

    fn parseRangeExpr(self: *Self, start: ExprIdx, end: ExprIdx, inclusive: bool, loc: SourceLoc) ParseError!ExprIdx {
        const op: BinaryOp = if (inclusive) .range_inclusive else .range;
        const idx: ExprIdx = @enumFromInt(@as(u32, @intCast(self.store.expr_tags.items.len)));
        self.store.expr_tags.append(self.allocator, .range) catch return error.OutOfMemory;
        self.store.expr_locs.append(self.allocator, loc) catch return error.OutOfMemory;
        self.store.expr_data.append(self.allocator, .{
            .a = start.toInt(),
            .b = (end.toInt() << 8) | @intFromEnum(op),
        }) catch return error.OutOfMemory;
        return idx;
    }

    // ============================================================
    // Generic Type Parameters
    // ============================================================

    /// Parse type parameters: <T, U: Bound, V>
    /// Returns the number of type parameters added to scope
    fn parseTypeParams(self: *Self) ParseError!u16 {
        if (!self.check(.lt)) return 0;
        _ = self.advance(); // consume '<'

        const start_count = self.type_params.items.len;
        var param_index: u16 = 0;

        while (!self.checkGt() and !self.isAtEnd()) {
            if (param_index >= MAX_TYPE_PARAMS) {
                self.addError("Too many type parameters (max 16)");
                return error.InvalidSyntax;
            }

            // Parse type parameter name
            const name_token = try self.consume(.identifier, "Expected type parameter name");
            const name = self.internString(name_token.lexeme) catch return error.OutOfMemory;

            // Optional bound: T: Bound
            var bound: TypeIdx = .null;
            if (self.match(&[_]TokenType{.colon})) {
                // Don't use type params when parsing the bound (avoid self-reference)
                bound = try self.parseType();
            }

            // Add to scope
            self.type_params.append(self.allocator, .{
                .name = name,
                .index = param_index,
                .bound = bound,
            }) catch return error.OutOfMemory;

            param_index += 1;

            if (!self.match(&[_]TokenType{.comma})) break;
        }

        _ = try self.consumeGt("Expected '>' after type parameters");
        return @intCast(self.type_params.items.len - start_count);
    }

    /// Clear type parameters from scope (call after parsing generic item body)
    fn clearTypeParams(self: *Self, count: u16) void {
        if (count > 0) {
            self.type_params.shrinkRetainingCapacity(self.type_params.items.len - count);
        }
    }

    /// Look up a type parameter by name, returns null if not found
    fn findTypeParam(self: *Self, name: StringId) ?TypeParamInfo {
        for (self.type_params.items) |param| {
            if (@intFromEnum(param.name) == @intFromEnum(name)) {
                return param;
            }
        }
        return null;
    }

    /// Store type parameters in extra_data for later use
    /// Format: [count, name0, bound0, name1, bound1, ...]
    fn storeTypeParams(self: *Self, count: u16) !u32 {
        if (count == 0) return 0;

        const start: u32 = @intCast(self.store.extra_data.items.len);
        try self.store.extra_data.append(self.allocator, count);

        const start_idx = self.type_params.items.len - count;
        for (self.type_params.items[start_idx..]) |param| {
            try self.store.extra_data.append(self.allocator, @intFromEnum(param.name));
            try self.store.extra_data.append(self.allocator, param.bound.toInt());
        }

        return start;
    }

    // ============================================================
    // Type Parsing
    // ============================================================

    fn parseType(self: *Self) ParseError!TypeIdx {
        // Optional pointer prefix
        if (self.match(&[_]TokenType{.star})) {
            const is_const = self.match(&[_]TokenType{.kw_const});
            const pointee = try self.parseType();
            const mutability: ast.Mutability = if (is_const) .@"const" else .mut;
            return self.store.addPointerType(pointee, mutability) catch return error.OutOfMemory;
        }

        // Optional reference prefix
        if (self.match(&[_]TokenType{.ampersand})) {
            const is_const = self.match(&[_]TokenType{.kw_const});
            const pointee = try self.parseType();
            const mutability: ast.Mutability = if (is_const) .@"const" else .mut;
            return self.store.addPointerType(pointee, mutability) catch return error.OutOfMemory;
        }

        // Optional question mark for optional
        if (self.match(&[_]TokenType{.question})) {
            const inner = try self.parseType();
            return self.store.addOptionalType(inner) catch return error.OutOfMemory;
        }

        // Weak reference type: weak T
        if (self.match(&[_]TokenType{.kw_weak})) {
            const inner = try self.parseType();
            return self.store.addWeakType(inner) catch return error.OutOfMemory;
        }

        // Trait object type: dyn Trait
        if (self.match(&[_]TokenType{.kw_dyn})) {
            const trait_name_token = try self.consume(.identifier, "Expected trait name after 'dyn'");
            const trait_name_id = self.strings.intern(trait_name_token.lexeme) catch return error.OutOfMemory;
            return self.store.addTraitObjectType(trait_name_id) catch return error.OutOfMemory;
        }

        // Array type: [N]T or []T
        if (self.match(&[_]TokenType{.lbracket})) {
            if (self.match(&[_]TokenType{.rbracket})) {
                // Slice type: []T
                const elem_type = try self.parseType();
                return self.store.addSliceType(elem_type) catch return error.OutOfMemory;
            } else {
                // Array type: [N]T
                const size_token = try self.consume(.integer_literal, "Expected array size");
                const size = std.fmt.parseInt(u32, size_token.lexeme, 10) catch 0;
                _ = try self.consume(.rbracket, "Expected ']'");
                const elem_type = try self.parseType();
                return self.store.addArrayType(elem_type, size) catch return error.OutOfMemory;
            }
        }

        // Named type or primitive
        const name_token = try self.consume(.identifier, "Expected type name");
        const name = name_token.lexeme;

        // Check for built-in types first
        if (std.mem.eql(u8, name, "i8")) return self.store.addPrimitiveType(.i8) catch return error.OutOfMemory;
        if (std.mem.eql(u8, name, "i16")) return self.store.addPrimitiveType(.i16) catch return error.OutOfMemory;
        if (std.mem.eql(u8, name, "i32") or std.mem.eql(u8, name, "int")) return self.store.addPrimitiveType(.i32) catch return error.OutOfMemory;
        if (std.mem.eql(u8, name, "i64")) return self.store.addPrimitiveType(.i64) catch return error.OutOfMemory;
        if (std.mem.eql(u8, name, "u8")) return self.store.addPrimitiveType(.u8) catch return error.OutOfMemory;
        if (std.mem.eql(u8, name, "u16")) return self.store.addPrimitiveType(.u16) catch return error.OutOfMemory;
        if (std.mem.eql(u8, name, "u32")) return self.store.addPrimitiveType(.u32) catch return error.OutOfMemory;
        if (std.mem.eql(u8, name, "u64")) return self.store.addPrimitiveType(.u64) catch return error.OutOfMemory;
        if (std.mem.eql(u8, name, "isize")) return self.store.addPrimitiveType(.isize) catch return error.OutOfMemory;
        if (std.mem.eql(u8, name, "usize")) return self.store.addPrimitiveType(.usize) catch return error.OutOfMemory;
        if (std.mem.eql(u8, name, "f32")) return self.store.addPrimitiveType(.f32) catch return error.OutOfMemory;
        if (std.mem.eql(u8, name, "f64")) return self.store.addPrimitiveType(.f64) catch return error.OutOfMemory;
        if (std.mem.eql(u8, name, "bool")) return self.store.addPrimitiveType(.bool) catch return error.OutOfMemory;
        if (std.mem.eql(u8, name, "void")) return self.store.addPrimitiveType(.void) catch return error.OutOfMemory;
        if (std.mem.eql(u8, name, "string")) return self.store.addPrimitiveType(.string) catch return error.OutOfMemory;

        // Built-in generic types: Result<T, E>, Option<T>, and Map<K, V>
        if (std.mem.eql(u8, name, "Result")) {
            return try self.parseResultType();
        }
        if (std.mem.eql(u8, name, "Option")) {
            return try self.parseOptionType();
        }
        if (std.mem.eql(u8, name, "Map")) {
            return try self.parseMapType();
        }
        if (std.mem.eql(u8, name, "List")) {
            return try self.parseListType();
        }

        const name_id = self.internString(name) catch return error.OutOfMemory;

        // Check if this is a type parameter in scope
        if (self.findTypeParam(name_id)) |param| {
            const base_type = self.store.addTypeParam(param.name, param.index) catch return error.OutOfMemory;
            // Check for associated type: T.Item
            return self.maybeParseAssociatedType(base_type);
        }

        // Check for generic instantiation: Foo<T, U>
        if (self.check(.lt)) {
            const base_type = try self.parseGenericInstantiation(name_id);
            // Check for associated type on generic instance
            return self.maybeParseAssociatedType(base_type);
        }

        // Plain named type reference
        const base_type = self.store.addNamedType(name_id) catch return error.OutOfMemory;
        // Check for associated type: Self.Item or TypeName.Item
        return self.maybeParseAssociatedType(base_type);
    }

    /// Check for and parse associated type suffix: .AssocName
    /// Returns the original type if no associated type suffix, otherwise the associated type
    fn maybeParseAssociatedType(self: *Self, base_type: TypeIdx) ParseError!TypeIdx {
        if (self.check(.period)) {
            // Peek ahead to see if it's an identifier (associated type) vs struct init (.field = ...)
            if (self.peekNext().type == .identifier) {
                _ = self.advance(); // consume '.'
                const assoc_name_token = try self.consume(.identifier, "Expected associated type name");
                const assoc_name_id = self.internString(assoc_name_token.lexeme) catch return error.OutOfMemory;
                return self.store.addAssociatedType(base_type, assoc_name_id) catch return error.OutOfMemory;
            }
        }
        return base_type;
    }

    /// Parse generic type arguments: <T, U, i32>
    fn parseGenericInstantiation(self: *Self, base_name: StringId) ParseError!TypeIdx {
        _ = try self.consume(.lt, "Expected '<'");

        // Parse type arguments (use scratch buffer to collect them)
        try self.store.markScratch();
        errdefer self.store.rollbackScratch();

        var count: usize = 0;
        while (!self.checkGt() and !self.isAtEnd()) {
            if (count >= MAX_TYPE_PARAMS) {
                self.addError("Too many type arguments (max 16)");
                return error.InvalidSyntax;
            }

            const arg_type = try self.parseType();
            self.store.pushScratchU32(arg_type.toInt()) catch return error.OutOfMemory;
            count += 1;

            if (!self.match(&[_]TokenType{.comma})) break;
        }

        _ = try self.consumeGt("Expected '>' after type arguments");

        // Get type args from scratch and convert to TypeIdx slice
        const type_arg_u32s = self.store.getScratchU32s();
        self.store.commitScratch();

        // Convert u32 slice to TypeIdx slice for the call
        var type_args: [MAX_TYPE_PARAMS]TypeIdx = undefined;
        for (type_arg_u32s, 0..) |raw, i| {
            type_args[i] = TypeIdx.fromInt(raw);
        }

        // Create the base named type
        const base_type = self.store.addNamedType(base_name) catch return error.OutOfMemory;

        // Create the generic instance
        return self.store.addGenericInstance(base_type, type_args[0..type_arg_u32s.len]) catch return error.OutOfMemory;
    }

    /// Parse Result<T, E> built-in type -> error_union
    fn parseResultType(self: *Self) ParseError!TypeIdx {
        _ = try self.consume(.lt, "Expected '<' after Result");
        const value_type = try self.parseType();
        _ = try self.consume(.comma, "Expected ',' in Result<T, E>");
        const error_type = try self.parseType();
        _ = try self.consumeGt("Expected '>' after Result type arguments");
        return self.store.addErrorUnionType(value_type, error_type) catch return error.OutOfMemory;
    }

    /// Parse Option<T> built-in type -> optional
    fn parseOptionType(self: *Self) ParseError!TypeIdx {
        _ = try self.consume(.lt, "Expected '<' after Option");
        const inner_type = try self.parseType();
        _ = try self.consumeGt("Expected '>' after Option type argument");
        return self.store.addOptionalType(inner_type) catch return error.OutOfMemory;
    }

    /// Parse Map<K, V> built-in type
    fn parseMapType(self: *Self) ParseError!TypeIdx {
        _ = try self.consume(.lt, "Expected '<' after Map");
        const key_type = try self.parseType();
        _ = try self.consume(.comma, "Expected ',' in Map<K, V>");
        const value_type = try self.parseType();
        _ = try self.consumeGt("Expected '>' after Map type arguments");
        return self.store.addMapType(key_type, value_type) catch return error.OutOfMemory;
    }

    /// Parse List<T> built-in type
    fn parseListType(self: *Self) ParseError!TypeIdx {
        _ = try self.consume(.lt, "Expected '<' after List");
        const element_type = try self.parseType();
        _ = try self.consumeGt("Expected '>' after List type argument");
        return self.store.addListType(element_type) catch return error.OutOfMemory;
    }

    // ============================================================
    // Token Helpers
    // ============================================================

    fn check(self: *Self, token_type: TokenType) bool {
        if (self.isAtEnd()) return false;
        // Handle pending `>` from a split `>>` token
        if (token_type == .gt and self.pending_gt) return true;
        return self.peek().type == token_type;
    }

    /// Check if current token is `>`, also accepting `>>` (shr) in generic contexts.
    /// This allows parsing nested generics like `Iterator<Option<i64>>`.
    fn checkGt(self: *Self) bool {
        if (self.isAtEnd()) return false;
        if (self.pending_gt) return true;
        const t = self.peek().type;
        return t == .gt or t == .shr;
    }

    fn match(self: *Self, types: []const TokenType) bool {
        for (types) |t| {
            if (self.check(t)) {
                // Handle pending `>` from a split `>>` token - don't advance, just clear the flag
                if (t == .gt and self.pending_gt) {
                    self.pending_gt = false;
                    return true;
                }
                _ = self.advance();
                return true;
            }
        }
        return false;
    }

    fn advance(self: *Self) Token {
        if (!self.isAtEnd()) self.current += 1;
        return self.previous();
    }

    fn isAtEnd(self: *Self) bool {
        return self.peek().type == .eof;
    }

    fn peek(self: *Self) Token {
        return self.tokens[self.current];
    }

    fn peekNext(self: *Self) Token {
        if (self.current + 1 >= self.tokens.len) {
            return self.tokens[self.tokens.len - 1]; // Return EOF
        }
        return self.tokens[self.current + 1];
    }

    fn previous(self: *Self) Token {
        return self.tokens[self.current - 1];
    }

    fn consume(self: *Self, token_type: TokenType, message: []const u8) ParseError!Token {
        if (self.check(token_type)) return self.advance();

        // Enhanced diagnostic: show expected vs actual token
        const actual = self.peek();
        log.debug("consume failed: expected {s}, got {s} '{s}' at {d}:{d}", .{
            @tagName(token_type),
            @tagName(actual.type),
            actual.lexeme,
            actual.line,
            actual.column,
        });

        self.errors.append(self.allocator, .{
            .message = message,
            .line = self.peek().line,
            .column = self.peek().column,
        }) catch {};

        return error.UnexpectedToken;
    }

    /// Consume a `>` token, also accepting `>>` (shr) and splitting it into two `>` tokens.
    /// This is essential for parsing nested generics like `Map<string, Option<i64>>`.
    fn consumeGt(self: *Self, message: []const u8) ParseError!Token {
        // If we have a pending `>` from a previous `>>` split, consume it
        if (self.pending_gt) {
            self.pending_gt = false;
            // Return a synthetic token based on previous position
            return self.previous();
        }

        const current_token = self.peek();

        // Normal `>` token
        if (current_token.type == .gt) {
            return self.advance();
        }

        // `>>` (shr) token - split it into two `>` tokens
        if (current_token.type == .shr) {
            self.pending_gt = true; // Mark that we owe another `>`
            return self.advance(); // Consume the `>>` and return the first `>`
        }

        // Neither `>` nor `>>` - error
        log.debug("consumeGt failed: expected > or >>, got {s} '{s}' at {d}:{d}", .{
            @tagName(current_token.type),
            current_token.lexeme,
            current_token.line,
            current_token.column,
        });

        self.errors.append(self.allocator, .{
            .message = message,
            .line = current_token.line,
            .column = current_token.column,
        }) catch {};

        return error.UnexpectedToken;
    }

    fn synchronize(self: *Self) void {
        _ = self.advance();

        while (!self.isAtEnd()) {
            switch (self.peek().type) {
                .kw_fn,
                .kw_struct,
                .kw_enum,
                .kw_const,
                .kw_var,
                .kw_type,
                .kw_if,
                .kw_for,
                .kw_while,
                .kw_loop,
                .kw_return,
                .kw_import,
                .kw_try,
                .kw_switch,
                => return,
                else => _ = self.advance(),
            }
        }
    }
};

// ============================================================
// Tests
// ============================================================

test "parse empty program" {
    const tokens = [_]Token{
        .{ .type = .eof, .lexeme = "", .line = 1, .column = 1 },
    };

    var strings = StringInterner.init(std.testing.allocator);
    defer strings.deinit();

    var store = NodeStore.init(std.testing.allocator, &strings);
    defer store.deinit();

    var parser = Parser.init(std.testing.allocator, &tokens, &store, &strings);
    defer parser.deinit();

    const top_level = try parser.parse();
    defer std.testing.allocator.free(top_level);

    try std.testing.expectEqual(@as(usize, 0), top_level.len);
}

test "parse const declaration" {
    const tokens = [_]Token{
        .{ .type = .kw_const, .lexeme = "const", .line = 1, .column = 1 },
        .{ .type = .identifier, .lexeme = "x", .line = 1, .column = 7 },
        .{ .type = .equals, .lexeme = "=", .line = 1, .column = 9 },
        .{ .type = .integer_literal, .lexeme = "42", .line = 1, .column = 11 },
        .{ .type = .eof, .lexeme = "", .line = 1, .column = 13 },
    };

    var strings = StringInterner.init(std.testing.allocator);
    defer strings.deinit();

    var store = NodeStore.init(std.testing.allocator, &strings);
    defer store.deinit();

    var parser = Parser.init(std.testing.allocator, &tokens, &store, &strings);
    defer parser.deinit();

    const top_level = try parser.parse();
    defer std.testing.allocator.free(top_level);

    try std.testing.expectEqual(@as(usize, 1), top_level.len);
    try std.testing.expectEqual(StatementTag.const_decl, store.stmtTag(top_level[0]));
}

test "parse var declaration" {
    const tokens = [_]Token{
        .{ .type = .kw_var, .lexeme = "var", .line = 1, .column = 1 },
        .{ .type = .identifier, .lexeme = "x", .line = 1, .column = 5 },
        .{ .type = .equals, .lexeme = "=", .line = 1, .column = 7 },
        .{ .type = .integer_literal, .lexeme = "0", .line = 1, .column = 9 },
        .{ .type = .eof, .lexeme = "", .line = 1, .column = 10 },
    };

    var strings = StringInterner.init(std.testing.allocator);
    defer strings.deinit();

    var store = NodeStore.init(std.testing.allocator, &strings);
    defer store.deinit();

    var parser = Parser.init(std.testing.allocator, &tokens, &store, &strings);
    defer parser.deinit();

    const top_level = try parser.parse();
    defer std.testing.allocator.free(top_level);

    try std.testing.expectEqual(@as(usize, 1), top_level.len);
    try std.testing.expectEqual(StatementTag.let_decl, store.stmtTag(top_level[0]));
}

test "error collection - invalid top level" {
    const tokens = [_]Token{
        .{ .type = .identifier, .lexeme = "bad", .line = 1, .column = 1 },
        .{ .type = .identifier, .lexeme = "syntax", .line = 1, .column = 5 },
        .{ .type = .eof, .lexeme = "", .line = 1, .column = 12 },
    };

    var strings = StringInterner.init(std.testing.allocator);
    defer strings.deinit();

    var store = NodeStore.init(std.testing.allocator, &strings);
    defer store.deinit();

    var parser = Parser.init(std.testing.allocator, &tokens, &store, &strings);
    defer parser.deinit();

    const top_level = try parser.parse();
    defer std.testing.allocator.free(top_level);

    // Should have collected errors
    try std.testing.expect(parser.hasErrors());
    try std.testing.expect(parser.errorCount() >= 1);
}
