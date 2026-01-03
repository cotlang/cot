//! DBL Parser
//!
//! Parses DBL source tokens into a Cot Abstract Syntax Tree.
//! This is a dedicated parser for the DBL language that produces
//! the same AST types as the core Cot parser, enabling DBL source
//! to be compiled through the standard Cot pipeline.
//!
//! ## Architecture
//!
//! DBL Source → DBL Lexer → DBL Parser → cot.ast.NodeStore (StmtIdx[])
//!                                           ↓
//!                                   Shared: IR → Bytecode → VM
//!
//! ## Status
//!
//! This is a minimal implementation. Many DBL features are not yet supported.
//! Features are added incrementally as needed.

const std = @import("std");
const cot = @import("cot");
const Token = @import("token.zig").Token;
const TokenType = @import("token.zig").TokenType;
const dbl_types = @import("types.zig");
const AccessModifier = dbl_types.AccessModifier;
const ClassModifiers = dbl_types.ClassModifiers;
const ast = cot.ast;
const base = cot.base;
const NodeStore = ast.NodeStore;
const StringInterner = base.StringInterner;
const StmtIdx = ast.StmtIdx;
const ExprIdx = ast.ExprIdx;
const TypeIdx = ast.TypeIdx;
const SourceLoc = ast.SourceLoc;
const BinaryOp = ast.BinaryOp;
const UnaryOp = ast.UnaryOp;

pub const ParseError = error{
    UnexpectedToken,
    UnexpectedEof,
    InvalidExpression,
    InvalidStatement,
    InvalidDataType,
    OutOfMemory,
    TooNested,
};

pub const Parser = struct {
    allocator: std.mem.Allocator,
    tokens: []const Token,
    current: usize,
    errors: std.ArrayListAligned(Error, null),
    nesting_depth: u8,
    store: *NodeStore,
    strings: *StringInterner,
    pending_common_globals: std.ArrayListUnmanaged(StmtIdx),
    /// Pending impl_block statements from class definitions
    pending_class_impl_blocks: std.ArrayListUnmanaged(StmtIdx),
    /// Base field name for record overlays (,X syntax)
    /// Stores the first field name of the last non-overlay record
    last_record_base_field: ?base.StringId,

    const Self = @This();
    const MAX_NESTING_DEPTH: u8 = 128;
    const MAX_ERRORS: usize = 100;

    pub const Error = struct {
        message: []const u8,
        token: Token,
    };

    /// Initialize parser with tokens from DBL lexer
    pub fn init(allocator: std.mem.Allocator, tokens: []const Token, store: *NodeStore, strings: *StringInterner) Self {
        return .{
            .allocator = allocator,
            .tokens = tokens,
            .current = 0,
            .errors = .empty,
            .nesting_depth = 0,
            .store = store,
            .strings = strings,
            .pending_common_globals = .empty,
            .pending_class_impl_blocks = .empty,
            .last_record_base_field = null,
        };
    }

    /// Get source location from current token
    fn currentLoc(self: *Self) SourceLoc {
        const tok = self.peek();
        return .{
            .line = @intCast(tok.line),
            .column = @intCast(tok.column),
        };
    }

    /// Intern a string (for identifiers, etc.)
    /// DBL is case-insensitive, so identifiers are normalized to lowercase
    fn intern(self: *Self, str: []const u8) !base.StringId {
        // DBL case insensitivity: normalize identifiers to lowercase
        var lower_buf: [256]u8 = undefined;
        const lower_len = @min(str.len, lower_buf.len);
        const lower_str = std.ascii.lowerString(lower_buf[0..lower_len], str[0..lower_len]);
        return self.strings.intern(lower_str) catch return ParseError.OutOfMemory;
    }

    /// Intern a string literal (preserves case)
    fn internLiteral(self: *Self, str: []const u8) !base.StringId {
        return self.strings.intern(str) catch return ParseError.OutOfMemory;
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
        self.errors.append(self.allocator, .{
            .message = message,
            .token = self.peek(),
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

    /// Clean up parser resources
    pub fn deinit(self: *Self) void {
        self.errors.deinit(self.allocator);
        self.pending_common_globals.deinit(self.allocator);
        self.pending_class_impl_blocks.deinit(self.allocator);
    }

    /// Parse tokens into a list of top-level statements
    pub fn parse(self: *Self) ParseError![]const StmtIdx {
        var statements: std.ArrayListUnmanaged(StmtIdx) = .{};
        errdefer statements.deinit(self.allocator);

        while (!self.isAtEnd() and !self.tooManyErrors()) {
            if (self.parseStatement()) |stmt| {
                statements.append(self.allocator, stmt) catch return ParseError.OutOfMemory;
            } else |err| {
                self.synchronize();
                if (err == ParseError.OutOfMemory) return err;
            }
        }

        // Add pending class impl_blocks (these come after struct defs)
        for (self.pending_class_impl_blocks.items) |impl_stmt| {
            statements.append(self.allocator, impl_stmt) catch return ParseError.OutOfMemory;
        }

        // Add pending common block globals (these come after struct defs)
        for (self.pending_common_globals.items) |global_stmt| {
            statements.append(self.allocator, global_stmt) catch return ParseError.OutOfMemory;
        }

        return statements.toOwnedSlice(self.allocator) catch return ParseError.OutOfMemory;
    }

    /// Parse a single statement
    fn parseStatement(self: *Self) ParseError!StmtIdx {
        const token = self.peek();

        return switch (token.type) {
            // Procedure definitions
            .kw_main => self.parseMain(),
            .kw_function => self.parseFunction(),
            .kw_subroutine => self.parseSubroutine(),
            .kw_proc => {
                self.addError("'proc' must be inside main, subroutine, or function block");
                return ParseError.UnexpectedToken;
            },

            // Data definitions
            .kw_structure => self.parseStructure(),
            .kw_record => self.parseRecord(),
            .kw_common => self.parseCommon(),
            .kw_literal => self.parseLiteral(),

            // Testing
            .kw_test => self.parseTestDef(),

            // OOP definitions
            .kw_class => self.parseClass(.private, .{}),
            .kw_namespace => self.parseNamespace(),
            .kw_public, .kw_private, .kw_protected => self.parseAccessModifiedDecl(),
            .kw_abstract, .kw_sealed, .kw_partial => self.parseClassModifiedDecl(),

            // Control flow
            .kw_if => self.parseIf(),
            .kw_while => self.parseWhile(),
            .kw_for => self.parseFor(),
            .kw_loop => self.parseLoop(),
            .kw_begin => self.parseBlock(),
            .kw_using => self.parseUsing(),
            .kw_break => self.parseBreak(),
            .kw_continue => self.parseContinue(),
            .kw_xreturn, .kw_freturn, .kw_mreturn, .kw_return => self.parseReturn(),
            .kw_stop => self.parseStop(),

            // Error handling
            .kw_onerror => self.parseOnError(),
            .kw_offerror => self.parseOffError(),

            // Data manipulation
            .kw_clear => self.parseClear(),

            // Function calls
            .kw_xcall => self.parseXCall(),

            // End keywords
            .kw_end, .kw_endrecord, .kw_endgroup, .kw_endcase, .kw_endusing,
            .kw_endclass, .kw_endnamespace, .kw_endwhile, .kw_endliteral,
            .dir_else, .dir_end, .kw_else,
            => ParseError.InvalidStatement,

            // Identifier could be assignment or function call
            .identifier => self.parseIdentifierStatement(),

            .eof => ParseError.UnexpectedEof,

            // Unsupported - skip
            else => {
                self.addError("Unsupported DBL statement");
                _ = self.advance();
                return ParseError.InvalidStatement;
            },
        };
    }

    // ============================================================
    // Procedure Definitions
    // ============================================================

    fn parseMain(self: *Self) ParseError!StmtIdx {
        const loc = self.currentLoc();
        _ = self.advance(); // consume 'main'

        // Parse variable declarations until PROC (record block)
        var var_decls: std.ArrayListUnmanaged(StmtIdx) = .{};
        errdefer var_decls.deinit(self.allocator);

        while (!self.check(.kw_proc) and !self.check(.kw_endmain) and !self.isAtEnd()) {
            if (self.check(.kw_record)) {
                // Handle record block for local variables
                const record_block = try self.parseRecord();
                var_decls.append(self.allocator, record_block) catch return ParseError.OutOfMemory;
            } else {
                _ = self.advance();
            }
        }
        _ = self.match(&[_]TokenType{.kw_proc});

        var body: std.ArrayListUnmanaged(StmtIdx) = .{};
        errdefer body.deinit(self.allocator);

        // Add variable declarations to body first
        for (var_decls.items) |decl| {
            body.append(self.allocator, decl) catch return ParseError.OutOfMemory;
        }

        // Parse statements
        while (!self.check(.kw_endmain) and !self.isAtEnd()) {
            const stmt = self.parseStatement() catch |err| {
                self.synchronize();
                if (err == ParseError.OutOfMemory) return err;
                continue;
            };
            body.append(self.allocator, stmt) catch return ParseError.OutOfMemory;
        }

        _ = try self.consume(.kw_endmain, "Expected 'endmain'");

        const block_idx = self.store.addBlock(body.items, loc) catch return ParseError.OutOfMemory;
        body.deinit(self.allocator); // Free ArrayList backing memory after data is copied
        var_decls.deinit(self.allocator); // Free var_decls ArrayList

        const name_id = try self.intern("main");
        const return_type = self.store.addPrimitiveType(.i32) catch return ParseError.OutOfMemory;

        return self.store.addFnDef(name_id, &[_]u32{}, return_type, block_idx, loc) catch return ParseError.OutOfMemory;
    }

    fn parseFunction(self: *Self) ParseError!StmtIdx {
        const loc = self.currentLoc();
        _ = self.advance(); // consume 'function'

        const name_tok = try self.consume(.identifier, "Expected function name");
        const name_id = try self.intern(name_tok.lexeme);

        // Parse parameters until PROC
        var param_decls: std.ArrayListUnmanaged(StmtIdx) = .{};
        errdefer param_decls.deinit(self.allocator);

        while (!self.check(.kw_proc) and !self.check(.kw_endfunction) and !self.isAtEnd()) {
            if (self.check(.identifier)) {
                const param_name_tok = self.advance();
                const param_name_id = try self.intern(param_name_tok.lexeme);

                // Expect comma before type
                _ = self.match(&[_]TokenType{.comma});

                // Parse type specifier
                var param_type = TypeIdx.null;
                var is_struct_type = false;
                if (self.check(.identifier)) {
                    const type_tok = self.advance();
                    const type_result = self.parseDblTypeSpecWithInfo(type_tok.lexeme);
                    param_type = type_result.type_idx;
                    is_struct_type = type_result.is_struct;
                } else {
                    param_type = self.store.addPrimitiveType(.i64) catch return ParseError.OutOfMemory;
                }

                // Create parameter declaration with proper type
                const init_val = if (is_struct_type) ExprIdx.null else self.store.addIntLiteral(0, loc) catch return ParseError.OutOfMemory;
                const param_decl = self.store.addLetDecl(param_name_id, param_type, init_val, true, loc) catch return ParseError.OutOfMemory;
                param_decls.append(self.allocator, param_decl) catch return ParseError.OutOfMemory;
            } else if (self.check(.kw_record)) {
                const record_block = try self.parseRecord();
                param_decls.append(self.allocator, record_block) catch return ParseError.OutOfMemory;
            } else {
                _ = self.advance();
            }
        }
        _ = self.match(&[_]TokenType{.kw_proc});

        var body: std.ArrayListUnmanaged(StmtIdx) = .{};
        errdefer body.deinit(self.allocator);

        // Add parameter declarations to body first
        for (param_decls.items) |decl| {
            body.append(self.allocator, decl) catch return ParseError.OutOfMemory;
        }

        while (!self.check(.kw_endfunction) and !self.isAtEnd()) {
            const stmt = self.parseStatement() catch |err| {
                self.synchronize();
                if (err == ParseError.OutOfMemory) return err;
                continue;
            };
            body.append(self.allocator, stmt) catch return ParseError.OutOfMemory;
        }

        _ = try self.consume(.kw_endfunction, "Expected 'endfunction'");

        const block_idx = self.store.addBlock(body.items, loc) catch return ParseError.OutOfMemory;
        body.deinit(self.allocator); // Free ArrayList backing memory after data is copied
        param_decls.deinit(self.allocator); // Free param_decls ArrayList

        const return_type = self.store.addPrimitiveType(.i32) catch return ParseError.OutOfMemory;

        return self.store.addFnDef(name_id, &[_]u32{}, return_type, block_idx, loc) catch return ParseError.OutOfMemory;
    }

    fn parseSubroutine(self: *Self) ParseError!StmtIdx {
        const loc = self.currentLoc();
        _ = self.advance(); // consume 'subroutine'

        const name_tok = try self.consume(.identifier, "Expected subroutine name");
        const name_id = try self.intern(name_tok.lexeme);

        // Parse parameters until PROC
        // Format: param_name ,type
        // Parameters are stored as [name_id, type_idx, is_ref, default_value] quads
        var params: std.ArrayListUnmanaged(u32) = .{};
        errdefer params.deinit(self.allocator);
        var local_record_decls: std.ArrayListUnmanaged(StmtIdx) = .{};
        errdefer local_record_decls.deinit(self.allocator);

        while (!self.check(.kw_proc) and !self.check(.kw_endsubroutine) and !self.isAtEnd()) {
            // Check for ref modifier (inout/out = ref, in = val)
            // DBL subroutines default to ref (legacy inout behavior)
            var is_ref: u32 = 1; // ref by default for subroutines
            if (self.match(&[_]TokenType{.kw_inout})) {
                is_ref = 1; // ref
            } else if (self.match(&[_]TokenType{.kw_out})) {
                is_ref = 1; // ref (out is also ref)
            } else if (self.match(&[_]TokenType{.kw_in})) {
                is_ref = 0; // val
            }

            // Check for required modifier (DBL subroutines are optional by default)
            var is_required = false;
            if (self.match(&[_]TokenType{.kw_req})) {
                is_required = true;
            }

            if (self.check(.identifier)) {
                const param_name_tok = self.advance();
                const param_name_id = try self.intern(param_name_tok.lexeme);

                // Expect comma before type
                _ = self.match(&[_]TokenType{.comma});

                // Parse type specifier
                var param_type = TypeIdx.null;
                var type_char: u8 = 'i'; // default to integer for type-based defaults
                if (self.check(.identifier)) {
                    const type_tok = self.advance();
                    const type_result = self.parseDblTypeSpecWithInfo(type_tok.lexeme);
                    param_type = type_result.type_idx;
                    // Get type character for default value generation
                    if (type_tok.lexeme.len > 0) {
                        type_char = type_tok.lexeme[0];
                    }
                } else {
                    param_type = self.store.addPrimitiveType(.i64) catch return ParseError.OutOfMemory;
                }

                // Check for trailing modifiers after type: ,req or ,opt
                // This supports DBL syntax: param_name ,type ,req
                // Save current position in case we need to backtrack
                const saved_pos = self.current;
                if (self.match(&[_]TokenType{.comma})) {
                    if (self.match(&[_]TokenType{.kw_req})) {
                        is_required = true;
                    } else if (self.match(&[_]TokenType{.kw_opt})) {
                        // is_required stays false (optional)
                    } else {
                        // Not a modifier, backtrack (it might be the next param)
                        self.current = saved_pos;
                    }
                }

                // Parse optional default value: = expr
                // For DBL subroutines, params are optional by default (unless req specified)
                var default_expr: u32 = 0; // 0 = no default (required)
                if (self.match(&[_]TokenType{.equals})) {
                    // Explicit default value
                    const expr = try self.parseExpression();
                    default_expr = expr.toInt();
                } else if (!is_required) {
                    // Optional without explicit default - generate type-based default
                    // a (alpha) -> ""
                    // d (decimal) -> 0
                    // i (integer) -> 0
                    const default_loc = self.currentLoc();
                    if (type_char == 'a') {
                        const empty_str = try self.internLiteral("");
                        const str_expr = self.store.addStringLiteral(empty_str, default_loc) catch return ParseError.OutOfMemory;
                        default_expr = str_expr.toInt();
                    } else {
                        const zero_expr = self.store.addIntLiteral(0, default_loc) catch return ParseError.OutOfMemory;
                        default_expr = zero_expr.toInt();
                    }
                }

                // Add parameter as [name_id, type_idx, is_ref, default_value] quad
                params.append(self.allocator, @intFromEnum(param_name_id)) catch return ParseError.OutOfMemory;
                params.append(self.allocator, param_type.toInt()) catch return ParseError.OutOfMemory;
                params.append(self.allocator, is_ref) catch return ParseError.OutOfMemory;
                params.append(self.allocator, default_expr) catch return ParseError.OutOfMemory;
            } else if (self.check(.kw_record)) {
                // Handle inline record block for local variables
                const record_block = try self.parseRecord();
                local_record_decls.append(self.allocator, record_block) catch return ParseError.OutOfMemory;
            } else {
                _ = self.advance();
            }
        }
        _ = self.match(&[_]TokenType{.kw_proc});

        var body: std.ArrayListUnmanaged(StmtIdx) = .{};
        errdefer body.deinit(self.allocator);

        // Add local record declarations to body first
        for (local_record_decls.items) |decl| {
            body.append(self.allocator, decl) catch return ParseError.OutOfMemory;
        }

        while (!self.check(.kw_endsubroutine) and !self.isAtEnd()) {
            const stmt = self.parseStatement() catch |err| {
                self.synchronize();
                if (err == ParseError.OutOfMemory) return err;
                continue;
            };
            body.append(self.allocator, stmt) catch return ParseError.OutOfMemory;
        }

        _ = try self.consume(.kw_endsubroutine, "Expected 'endsubroutine'");

        const block_idx = self.store.addBlock(body.items, loc) catch return ParseError.OutOfMemory;
        body.deinit(self.allocator); // Free ArrayList backing memory after data is copied
        local_record_decls.deinit(self.allocator); // Free local_record_decls ArrayList

        const return_type = self.store.addPrimitiveType(.void) catch return ParseError.OutOfMemory;

        const result = self.store.addFnDef(name_id, params.items, return_type, block_idx, loc) catch return ParseError.OutOfMemory;
        params.deinit(self.allocator); // Free params ArrayList
        return result;
    }

    // ============================================================
    // Control Flow
    // ============================================================

    fn parseIf(self: *Self) ParseError!StmtIdx {
        const loc = self.currentLoc();
        try self.enterNesting();
        defer self.exitNesting();

        _ = self.advance(); // consume 'if'

        const condition = try self.parseExpression();

        // Optional 'then'
        _ = self.match(&[_]TokenType{.kw_then});

        const then_body = try self.parseStatement();

        var else_body = StmtIdx.null;
        if (self.match(&[_]TokenType{.kw_else})) {
            else_body = try self.parseStatement();
        }

        return self.store.addIfStmt(condition, then_body, else_body, loc) catch return ParseError.OutOfMemory;
    }

    fn parseWhile(self: *Self) ParseError!StmtIdx {
        const loc = self.currentLoc();
        try self.enterNesting();
        defer self.exitNesting();

        _ = self.advance(); // consume 'while'

        const condition = try self.parseExpression();

        // Two forms:
        // 1. while (cond) begin ... end   - block is the body
        // 2. while (cond) ... endwhile    - statements until endwhile
        const body = if (self.check(.kw_begin)) blk: {
            // Form 1: begin...end block is the body
            break :blk try self.parseStatement();
        } else blk: {
            // Form 2: collect statements until 'endwhile'
            var stmts: std.ArrayListUnmanaged(StmtIdx) = .{};
            errdefer stmts.deinit(self.allocator);

            while (!self.check(.kw_endwhile) and !self.isAtEnd()) {
                const stmt = self.parseStatement() catch |err| {
                    self.synchronize();
                    if (err == ParseError.OutOfMemory) return err;
                    continue;
                };
                stmts.append(self.allocator, stmt) catch return ParseError.OutOfMemory;
            }

            _ = try self.consume(.kw_endwhile, "Expected 'endwhile'");

            const result = self.store.addBlock(stmts.items, loc) catch return ParseError.OutOfMemory;
            stmts.deinit(self.allocator); // Free ArrayList backing memory after data is copied
            break :blk result;
        };

        return self.store.addWhileStmt(condition, body, loc) catch return ParseError.OutOfMemory;
    }

    fn parseFor(self: *Self) ParseError!StmtIdx {
        const loc = self.currentLoc();
        try self.enterNesting();
        defer self.exitNesting();

        _ = self.advance(); // consume 'for'

        const var_tok = try self.consume(.identifier, "Expected loop variable");
        const var_id = try self.intern(var_tok.lexeme);

        // Skip 'from' or '='
        _ = self.match(&[_]TokenType{ .kw_from, .equals });

        const start_expr = try self.parseExpression();

        // Skip 'thru'
        _ = self.match(&[_]TokenType{.kw_thru});

        const end_expr = try self.parseExpression();

        // Create range expression: start..end
        const range_expr = self.store.addBinary(start_expr, .range, end_expr, loc) catch return ParseError.OutOfMemory;

        const body = try self.parseStatement();

        return self.store.addForStmt(var_id, range_expr, body, loc) catch return ParseError.OutOfMemory;
    }

    fn parseLoop(self: *Self) ParseError!StmtIdx {
        const loc = self.currentLoc();
        try self.enterNesting();
        defer self.exitNesting();

        _ = self.advance(); // consume 'loop'

        const body = try self.parseStatement();

        return self.store.addLoopStmt(body, loc) catch return ParseError.OutOfMemory;
    }

    fn parseBlock(self: *Self) ParseError!StmtIdx {
        const loc = self.currentLoc();
        try self.enterNesting();
        defer self.exitNesting();

        _ = self.advance(); // consume 'begin'

        var stmts: std.ArrayListUnmanaged(StmtIdx) = .{};
        errdefer stmts.deinit(self.allocator);

        while (!self.check(.kw_end) and !self.isAtEnd()) {
            const stmt = self.parseStatement() catch |err| {
                self.synchronize();
                if (err == ParseError.OutOfMemory) return err;
                continue;
            };
            stmts.append(self.allocator, stmt) catch return ParseError.OutOfMemory;
        }

        _ = try self.consume(.kw_end, "Expected 'end'");

        const result = self.store.addBlock(stmts.items, loc) catch return ParseError.OutOfMemory;
        stmts.deinit(self.allocator); // Free ArrayList backing memory after data is copied
        return result;
    }

    fn parseBreak(self: *Self) ParseError!StmtIdx {
        const loc = self.currentLoc();
        _ = self.advance(); // consume 'break'
        return self.store.addBreak(loc) catch return ParseError.OutOfMemory;
    }

    fn parseContinue(self: *Self) ParseError!StmtIdx {
        const loc = self.currentLoc();
        _ = self.advance(); // consume 'continue'
        return self.store.addContinue(loc) catch return ParseError.OutOfMemory;
    }

    fn parseReturn(self: *Self) ParseError!StmtIdx {
        const loc = self.currentLoc();
        _ = self.advance(); // consume return keyword

        var value = ExprIdx.null;
        if (!self.isAtEnd() and !self.check(.kw_end) and !self.check(.kw_endfunction) and
            !self.check(.kw_endsubroutine) and !self.check(.kw_endmain))
        {
            // Try to parse return value
            value = self.parseExpression() catch ExprIdx.null;
        }

        return self.store.addReturn(value, loc) catch return ParseError.OutOfMemory;
    }

    fn parseStop(self: *Self) ParseError!StmtIdx {
        const loc = self.currentLoc();
        _ = self.advance(); // consume 'stop'
        // stop terminates the program - emit as return with no value
        return self.store.addReturn(ExprIdx.null, loc) catch return ParseError.OutOfMemory;
    }

    // ============================================================
    // Data Definitions
    // ============================================================

    /// Parse structure definition (DBL record type) -> Cot struct
    /// structure name
    ///     field ,type
    ///     ...
    /// endstructure
    fn parseStructure(self: *Self) ParseError!StmtIdx {
        const loc = self.currentLoc();
        _ = self.advance(); // consume 'structure'

        const name_tok = try self.consume(.identifier, "Expected structure name");
        const name_id = try self.intern(name_tok.lexeme);

        // Use scratch buffer for fields
        self.store.markScratch() catch return ParseError.OutOfMemory;
        errdefer self.store.rollbackScratch();

        // Parse fields until endstructure
        while (!self.check(.kw_endstructure) and !self.isAtEnd()) {
            if (self.check(.identifier)) {
                const field_name_tok = self.advance();
                const field_name_id = try self.intern(field_name_tok.lexeme);

                // Expect comma before type
                _ = self.match(&[_]TokenType{.comma});

                // Parse type specifier
                var field_type = TypeIdx.null;
                if (self.check(.identifier)) {
                    const type_tok = self.advance();
                    field_type = try self.parseDblTypeSpec(type_tok.lexeme);
                } else {
                    field_type = self.store.addPrimitiveType(.i64) catch return ParseError.OutOfMemory;
                }

                // Store as [name, type] pairs in scratch
                self.store.pushScratchU32(@intFromEnum(field_name_id)) catch return ParseError.OutOfMemory;
                self.store.pushScratchU32(field_type.toInt()) catch return ParseError.OutOfMemory;
            } else {
                _ = self.advance();
            }
        }

        _ = try self.consume(.kw_endstructure, "Expected 'endstructure'");

        // Get fields from scratch
        const fields = self.store.getScratchU32s();
        self.store.commitScratch();

        // Store struct def in extra_data
        const fields_start = self.store.extra_data.items.len;
        self.store.extra_data.append(self.allocator, @intCast(fields.len / 2)) catch return ParseError.OutOfMemory; // field count
        self.store.extra_data.append(self.allocator, 0) catch return ParseError.OutOfMemory; // type param count (none for DBL)
        self.store.extra_data.append(self.allocator, 0) catch return ParseError.OutOfMemory; // type params start
        for (fields) |f| {
            self.store.extra_data.append(self.allocator, f) catch return ParseError.OutOfMemory;
        }

        // Add struct_def statement
        const idx: StmtIdx = @enumFromInt(@as(u32, @intCast(self.store.stmt_tags.items.len)));
        self.store.stmt_tags.append(self.allocator, .struct_def) catch return ParseError.OutOfMemory;
        self.store.stmt_locs.append(self.allocator, loc) catch return ParseError.OutOfMemory;
        self.store.stmt_data.append(self.allocator, .{
            .a = @intFromEnum(name_id),
            .b = @intCast(fields_start),
        }) catch return ParseError.OutOfMemory;

        return idx;
    }

    /// Parse common block (global struct with implicit instance)
    /// common name
    ///     field ,type
    ///     ...
    /// endcommon
    ///
    /// DBL common blocks define a named global struct and create an implicit global instance.
    /// Returns the struct_def, and adds the global instance to pending_globals for later emission.
    fn parseCommon(self: *Self) ParseError!StmtIdx {
        const loc = self.currentLoc();
        _ = self.advance(); // consume 'common'

        const name_tok = try self.consume(.identifier, "Expected common block name");
        const name_id = try self.intern(name_tok.lexeme);

        // Use scratch buffer for fields
        self.store.markScratch() catch return ParseError.OutOfMemory;
        errdefer self.store.rollbackScratch();

        // Parse fields until endcommon
        while (!self.check(.kw_endcommon) and !self.isAtEnd()) {
            if (self.check(.identifier)) {
                const field_name_tok = self.advance();
                const field_name_id = try self.intern(field_name_tok.lexeme);

                // Expect comma before type
                _ = self.match(&[_]TokenType{.comma});

                // Parse type specifier
                var field_type = TypeIdx.null;
                if (self.check(.identifier)) {
                    const type_tok = self.advance();
                    field_type = try self.parseDblTypeSpec(type_tok.lexeme);
                } else {
                    field_type = self.store.addPrimitiveType(.i64) catch return ParseError.OutOfMemory;
                }

                // Store as [name, type] pairs in scratch
                self.store.pushScratchU32(@intFromEnum(field_name_id)) catch return ParseError.OutOfMemory;
                self.store.pushScratchU32(field_type.toInt()) catch return ParseError.OutOfMemory;
            } else {
                _ = self.advance();
            }
        }

        _ = try self.consume(.kw_endcommon, "Expected 'endcommon'");

        // Get fields from scratch
        const fields = self.store.getScratchU32s();
        self.store.commitScratch();

        // Store struct def in extra_data
        const fields_start = self.store.extra_data.items.len;
        self.store.extra_data.append(self.allocator, @intCast(fields.len / 2)) catch return ParseError.OutOfMemory; // field count
        self.store.extra_data.append(self.allocator, 0) catch return ParseError.OutOfMemory; // type param count
        self.store.extra_data.append(self.allocator, 0) catch return ParseError.OutOfMemory; // type params start
        for (fields) |f| {
            self.store.extra_data.append(self.allocator, f) catch return ParseError.OutOfMemory;
        }

        // Create struct_def statement
        const struct_idx: StmtIdx = @enumFromInt(@as(u32, @intCast(self.store.stmt_tags.items.len)));
        self.store.stmt_tags.append(self.allocator, .struct_def) catch return ParseError.OutOfMemory;
        self.store.stmt_locs.append(self.allocator, loc) catch return ParseError.OutOfMemory;
        self.store.stmt_data.append(self.allocator, .{
            .a = @intFromEnum(name_id),
            .b = @intCast(fields_start),
        }) catch return ParseError.OutOfMemory;

        // Queue a global instance declaration for after the struct is defined
        // This creates: let name: name (a global variable of the struct type)
        const struct_type = self.store.addNamedType(name_id) catch return ParseError.OutOfMemory;
        const global_decl = self.store.addLetDecl(name_id, struct_type, ExprIdx.null, true, loc) catch return ParseError.OutOfMemory;
        self.pending_common_globals.append(self.allocator, global_decl) catch return ParseError.OutOfMemory;

        // Return just the struct_def (at top level where lowerer can find it)
        return struct_idx;
    }

    /// Parse literal block (compile-time constants)
    /// literal
    ///     name, type, value
    ///     ...
    /// endliteral
    ///
    /// DBL literal blocks define compile-time constants.
    /// a* means the size is derived from the initializer string length.
    fn parseLiteral(self: *Self) ParseError!StmtIdx {
        const loc = self.currentLoc();
        _ = self.advance(); // consume 'literal'

        var stmts: std.ArrayListUnmanaged(StmtIdx) = .{};
        errdefer stmts.deinit(self.allocator);

        // Parse constant declarations until endliteral
        while (!self.check(.kw_endliteral) and !self.isAtEnd()) {
            if (self.check(.identifier)) {
                const const_name_tok = self.advance();
                const const_name_id = try self.intern(const_name_tok.lexeme);

                // Expect comma before type
                _ = self.match(&[_]TokenType{.comma});

                // Parse type specifier
                var const_type = TypeIdx.null;
                if (self.check(.identifier)) {
                    const type_tok = self.advance();
                    // Handle a* syntax (auto-size string) - a followed by star
                    var lower_buf: [32]u8 = undefined;
                    const type_len = @min(type_tok.lexeme.len, lower_buf.len);
                    const lower_type = std.ascii.lowerString(lower_buf[0..type_len], type_tok.lexeme[0..type_len]);
                    if (std.mem.eql(u8, lower_type, "a") and self.check(.star)) {
                        // a* syntax - auto-sized string
                        _ = self.advance(); // consume '*'
                        const_type = self.store.addPrimitiveType(.string) catch return ParseError.OutOfMemory;
                    } else if (std.mem.eql(u8, lower_type, "d") and self.check(.star)) {
                        // d* syntax - auto-sized decimal
                        _ = self.advance(); // consume '*'
                        const_type = self.store.addDecimalType(28, 10) catch return ParseError.OutOfMemory;
                    } else {
                        const type_result = self.parseDblTypeSpecWithInfo(type_tok.lexeme);
                        const_type = type_result.type_idx;
                    }
                } else {
                    const_type = self.store.addPrimitiveType(.i64) catch return ParseError.OutOfMemory;
                }

                // Expect comma before value
                _ = self.match(&[_]TokenType{.comma});

                // Parse initial value expression
                var init_val = ExprIdx.null;
                if (!self.check(.kw_endliteral) and !self.check(.identifier)) {
                    init_val = self.parseExpression() catch ExprIdx.null;
                }

                // Create const declaration
                const const_stmt = self.store.addConstDecl(const_name_id, const_type, init_val, loc) catch return ParseError.OutOfMemory;
                stmts.append(self.allocator, const_stmt) catch return ParseError.OutOfMemory;
            } else {
                _ = self.advance();
            }
        }

        _ = try self.consume(.kw_endliteral, "Expected 'endliteral'");

        // Return as a block containing all the constant declarations
        const result = self.store.addBlock(stmts.items, loc) catch return ParseError.OutOfMemory;
        stmts.deinit(self.allocator);
        return result;
    }

    /// Parse a test definition: test "name" ... endtest
    fn parseTestDef(self: *Self) ParseError!StmtIdx {
        const loc = self.currentLoc();
        _ = self.advance(); // consume 'test'

        // Expect test name as string literal
        const name_token = try self.consume(.string_literal, "Expected test name as string literal");
        const name_lexeme = name_token.lexeme;
        // Strip quotes from string literal
        const name_str = if (name_lexeme.len >= 2) name_lexeme[1 .. name_lexeme.len - 1] else name_lexeme;
        const name = try self.intern(name_str);

        // Parse body until endtest
        var stmts: std.ArrayListUnmanaged(StmtIdx) = .{};
        errdefer stmts.deinit(self.allocator);

        while (!self.check(.kw_endtest) and !self.isAtEnd()) {
            const stmt = self.parseStatement() catch |err| {
                if (err == ParseError.InvalidStatement) {
                    continue;
                }
                return err;
            };
            stmts.append(self.allocator, stmt) catch return ParseError.OutOfMemory;
        }

        _ = try self.consume(.kw_endtest, "Expected 'endtest'");

        // Wrap in block
        const body = self.store.addBlock(stmts.items, loc) catch return ParseError.OutOfMemory;
        stmts.deinit(self.allocator);

        return self.store.addTestDef(name, body, loc) catch return ParseError.OutOfMemory;
    }

    // ============================================================
    // OOP Definitions (Classes, Methods, Namespaces)
    // ============================================================

    /// Parse a declaration preceded by an access modifier
    /// Example: public class Foo ... or private method Bar ...
    fn parseAccessModifiedDecl(self: *Self) ParseError!StmtIdx {
        const access = AccessModifier.fromToken(self.peek().type);
        _ = self.advance(); // consume access modifier

        // Now parse the actual declaration
        return switch (self.peek().type) {
            .kw_class => self.parseClass(access, .{}),
            .kw_abstract => blk: {
                _ = self.advance();
                if (self.check(.kw_class)) {
                    break :blk self.parseClass(access, .{ .is_abstract = true });
                }
                self.addError("Expected 'class' after 'abstract'");
                break :blk ParseError.UnexpectedToken;
            },
            .kw_sealed => blk: {
                _ = self.advance();
                if (self.check(.kw_class)) {
                    break :blk self.parseClass(access, .{ .is_sealed = true });
                }
                self.addError("Expected 'class' after 'sealed'");
                break :blk ParseError.UnexpectedToken;
            },
            .kw_partial => blk: {
                _ = self.advance();
                if (self.check(.kw_class)) {
                    break :blk self.parseClass(access, .{ .is_partial = true });
                }
                self.addError("Expected 'class' after 'partial'");
                break :blk ParseError.UnexpectedToken;
            },
            else => {
                self.addError("Expected 'class' after access modifier");
                return ParseError.UnexpectedToken;
            },
        };
    }

    /// Parse a declaration preceded by a class modifier (abstract, sealed, partial)
    fn parseClassModifiedDecl(self: *Self) ParseError!StmtIdx {
        var modifiers = ClassModifiers{};

        // Collect all class modifiers
        while (true) {
            const tok = self.peek().type;
            if (tok == .kw_abstract) {
                modifiers.is_abstract = true;
                _ = self.advance();
            } else if (tok == .kw_sealed) {
                modifiers.is_sealed = true;
                _ = self.advance();
            } else if (tok == .kw_partial) {
                modifiers.is_partial = true;
                _ = self.advance();
            } else {
                break;
            }
        }

        // Now expect class keyword
        if (self.check(.kw_class)) {
            return self.parseClass(.private, modifiers);
        }

        self.addError("Expected 'class' after class modifiers");
        return ParseError.UnexpectedToken;
    }

    /// Parse a DBL class definition
    /// Syntax: [access] [abstract|sealed|partial] class Name [extends Base] [implements I1, I2, ...]
    ///             fields...
    ///             methods...
    ///         endclass
    ///
    /// Generates: struct_def for fields + impl_block for methods
    fn parseClass(self: *Self, access: AccessModifier, modifiers: ClassModifiers) ParseError!StmtIdx {
        _ = access; // TODO: Store access modifier in metadata
        _ = modifiers; // TODO: Store class modifiers in metadata

        const loc = self.currentLoc();
        _ = self.advance(); // consume 'class'

        const name_tok = try self.consume(.identifier, "Expected class name");
        const class_name_id = try self.intern(name_tok.lexeme);
        const class_name = name_tok.lexeme;

        // Parse optional extends clause
        // TODO: Generate base field in struct for inheritance
        if (self.match(&[_]TokenType{.kw_extends})) {
            _ = try self.consume(.identifier, "Expected base class name");
        }

        // Parse optional implements clause
        while (self.match(&[_]TokenType{.kw_implements})) {
            // Parse interface list
            _ = try self.consume(.identifier, "Expected interface name");
            while (self.match(&[_]TokenType{.comma})) {
                _ = try self.consume(.identifier, "Expected interface name");
            }
        }

        // Use scratch buffer for fields
        self.store.markScratch() catch return ParseError.OutOfMemory;
        errdefer self.store.rollbackScratch();

        // Also collect methods
        var methods: std.ArrayListUnmanaged(StmtIdx) = .{};
        errdefer methods.deinit(self.allocator);

        // Parse class body until endclass
        while (!self.check(.kw_endclass) and !self.isAtEnd()) {
            // Check for access modifiers on members
            var member_access = AccessModifier.private;
            if (self.match(&[_]TokenType{.kw_public})) {
                member_access = .public;
            } else if (self.match(&[_]TokenType{.kw_private})) {
                member_access = .private;
            } else if (self.match(&[_]TokenType{.kw_protected})) {
                member_access = .protected;
            }

            // Check for static modifier (TODO: Handle static members)
            _ = self.match(&[_]TokenType{.kw_static});

            // Check for virtual/override modifiers (TODO: Handle virtual methods)
            _ = self.match(&[_]TokenType{.kw_virtual}) or self.match(&[_]TokenType{.kw_override});

            if (self.check(.kw_method)) {
                // Parse method
                const method_stmt = try self.parseClassMethod(class_name, member_access);
                methods.append(self.allocator, method_stmt) catch return ParseError.OutOfMemory;
            } else if (self.check(.kw_property)) {
                // TODO: Parse property
                self.addError("Properties not yet supported");
                _ = self.advance();
            } else if (self.check(.identifier)) {
                // Field declaration: name, type
                const field_name_tok = self.advance();
                const field_name_id = try self.intern(field_name_tok.lexeme);

                // Expect comma before type
                _ = self.match(&[_]TokenType{.comma});

                // Parse type specifier
                var field_type = TypeIdx.null;
                if (self.check(.identifier)) {
                    const type_tok = self.advance();
                    field_type = try self.parseDblTypeSpec(type_tok.lexeme);
                } else {
                    field_type = self.store.addPrimitiveType(.i64) catch return ParseError.OutOfMemory;
                }

                // Store as [name, type] pairs in scratch
                // TODO: Store field access modifier (member_access) in metadata
                self.store.pushScratchU32(@intFromEnum(field_name_id)) catch return ParseError.OutOfMemory;
                self.store.pushScratchU32(field_type.toInt()) catch return ParseError.OutOfMemory;
            } else {
                _ = self.advance();
            }
        }

        _ = try self.consume(.kw_endclass, "Expected 'endclass'");

        // Get fields from scratch
        const fields = self.store.getScratchU32s();
        self.store.commitScratch();

        // Store struct def in extra_data
        const fields_start = self.store.extra_data.items.len;
        self.store.extra_data.append(self.allocator, @intCast(fields.len / 2)) catch return ParseError.OutOfMemory; // field count
        self.store.extra_data.append(self.allocator, 0) catch return ParseError.OutOfMemory; // type param count
        self.store.extra_data.append(self.allocator, 0) catch return ParseError.OutOfMemory; // type params start
        for (fields) |f| {
            self.store.extra_data.append(self.allocator, f) catch return ParseError.OutOfMemory;
        }

        // Create struct_def statement for the class
        const struct_idx: StmtIdx = @enumFromInt(@as(u32, @intCast(self.store.stmt_tags.items.len)));
        self.store.stmt_tags.append(self.allocator, .struct_def) catch return ParseError.OutOfMemory;
        self.store.stmt_locs.append(self.allocator, loc) catch return ParseError.OutOfMemory;
        self.store.stmt_data.append(self.allocator, .{
            .a = @intFromEnum(class_name_id),
            .b = @intCast(fields_start),
        }) catch return ParseError.OutOfMemory;

        // If there are methods, create impl_block
        if (methods.items.len > 0) {
            // Store methods in extra_data
            // Format: [method_count, trait_type, target_type, method1, method2, ...]
            const methods_start = self.store.extra_data.items.len;
            const class_type = self.store.addNamedType(class_name_id) catch return ParseError.OutOfMemory;
            self.store.extra_data.append(self.allocator, @intCast(methods.items.len)) catch return ParseError.OutOfMemory;
            self.store.extra_data.append(self.allocator, class_type.toInt()) catch return ParseError.OutOfMemory; // trait type
            self.store.extra_data.append(self.allocator, class_type.toInt()) catch return ParseError.OutOfMemory; // target type (same for inherent impl)
            for (methods.items) |m| {
                self.store.extra_data.append(self.allocator, m.toInt()) catch return ParseError.OutOfMemory;
            }

            // Create impl_block statement
            const impl_idx: StmtIdx = @enumFromInt(@as(u32, @intCast(self.store.stmt_tags.items.len)));
            self.store.stmt_tags.append(self.allocator, .impl_block) catch return ParseError.OutOfMemory;
            self.store.stmt_locs.append(self.allocator, loc) catch return ParseError.OutOfMemory;
            self.store.stmt_data.append(self.allocator, .{
                .a = class_type.toInt(), // trait type (same as target for inherent impl)
                .b = @intCast(methods_start),
            }) catch return ParseError.OutOfMemory;

            // Queue impl_block to be added to top-level statements
            self.pending_class_impl_blocks.append(self.allocator, impl_idx) catch return ParseError.OutOfMemory;
        }
        methods.deinit(self.allocator);

        return struct_idx;
    }

    /// Parse a class method
    /// Syntax: [access] [static] [virtual|override] method Name[, ReturnType]
    ///             params...
    ///             [parent(...) | this(...)]
    ///         proc
    ///             body...
    ///         endmethod
    fn parseClassMethod(self: *Self, class_name: []const u8, access: AccessModifier) ParseError!StmtIdx {
        _ = access; // TODO: Store access in method metadata
        const loc = self.currentLoc();
        _ = self.advance(); // consume 'method'

        // Check for destructor syntax: method ~ClassName
        var is_destructor = false;
        if (self.check(.op_bnot)) {
            _ = self.advance(); // consume '~'
            is_destructor = true;
        }

        const method_name_tok = try self.consume(.identifier, "Expected method name");
        const method_name = method_name_tok.lexeme;

        // Check if this is a constructor (method name == class name)
        var is_constructor = false;
        {
            var lower_method: [256]u8 = undefined;
            var lower_class: [256]u8 = undefined;
            const method_len = @min(method_name.len, lower_method.len);
            const class_len = @min(class_name.len, lower_class.len);
            const lower_m = std.ascii.lowerString(lower_method[0..method_len], method_name[0..method_len]);
            const lower_c = std.ascii.lowerString(lower_class[0..class_len], class_name[0..class_len]);
            if (std.mem.eql(u8, lower_m, lower_c)) {
                is_constructor = true;
            }
        }

        // For destructor, verify the name matches class name
        if (is_destructor) {
            var lower_method: [256]u8 = undefined;
            var lower_class: [256]u8 = undefined;
            const method_len = @min(method_name.len, lower_method.len);
            const class_len = @min(class_name.len, lower_class.len);
            const lower_m = std.ascii.lowerString(lower_method[0..method_len], method_name[0..method_len]);
            const lower_c = std.ascii.lowerString(lower_class[0..class_len], class_name[0..class_len]);
            if (!std.mem.eql(u8, lower_m, lower_c)) {
                self.addError("Destructor name must match class name");
            }
        }

        // Parse optional return type
        var return_type = TypeIdx.null;
        if (self.match(&[_]TokenType{.comma})) {
            if (self.check(.identifier)) {
                const type_tok = self.advance();
                // Check for 'void'
                var lower_buf: [32]u8 = undefined;
                const type_len = @min(type_tok.lexeme.len, lower_buf.len);
                const lower_type = std.ascii.lowerString(lower_buf[0..type_len], type_tok.lexeme[0..type_len]);
                if (!std.mem.eql(u8, lower_type, "void")) {
                    return_type = try self.parseDblTypeSpec(type_tok.lexeme);
                }
            }
        }

        // Parse parameters until proc or endmethod
        var params: std.ArrayListUnmanaged(u32) = .{};
        errdefer params.deinit(self.allocator);

        while (!self.check(.kw_proc) and !self.check(.kw_endmethod) and
            !self.check(.kw_parent) and !self.check(.kw_this) and !self.isAtEnd())
        {
            // Check for parameter direction modifiers
            var is_ref: u32 = 1; // Default to ref for DBL methods
            if (self.match(&[_]TokenType{.kw_inout})) {
                is_ref = 1;
            } else if (self.match(&[_]TokenType{.kw_out})) {
                is_ref = 1;
            } else if (self.match(&[_]TokenType{.kw_in})) {
                is_ref = 0;
            }

            // Check for required/optional modifier (TODO: Handle optional params)
            _ = self.match(&[_]TokenType{.kw_req}) or self.match(&[_]TokenType{.kw_opt});

            if (self.check(.identifier)) {
                const param_name_tok = self.advance();
                const param_name_id = try self.intern(param_name_tok.lexeme);

                // Expect comma before type
                _ = self.match(&[_]TokenType{.comma});

                // Parse type specifier
                var param_type = TypeIdx.null;
                if (self.check(.identifier)) {
                    const type_tok = self.advance();
                    param_type = try self.parseDblTypeSpec(type_tok.lexeme);
                } else {
                    param_type = self.store.addPrimitiveType(.i64) catch return ParseError.OutOfMemory;
                }

                // Store as [name, type, is_ref, default_value] quads
                params.append(self.allocator, @intFromEnum(param_name_id)) catch return ParseError.OutOfMemory;
                params.append(self.allocator, param_type.toInt()) catch return ParseError.OutOfMemory;
                params.append(self.allocator, is_ref) catch return ParseError.OutOfMemory;
                params.append(self.allocator, ExprIdx.null.toInt()) catch return ParseError.OutOfMemory; // no default
            } else {
                _ = self.advance();
            }
        }

        // Parse optional constructor initializer: parent(...) or this(...)
        if (is_constructor) {
            if (self.match(&[_]TokenType{.kw_parent})) {
                // parent(...) - call parent constructor
                if (self.match(&[_]TokenType{.lparen})) {
                    // TODO: Parse parent constructor arguments
                    while (!self.check(.rparen) and !self.isAtEnd()) {
                        _ = self.advance();
                    }
                    _ = self.match(&[_]TokenType{.rparen});
                }
            } else if (self.match(&[_]TokenType{.kw_this})) {
                // this(...) - call sibling constructor
                if (self.match(&[_]TokenType{.lparen})) {
                    // TODO: Parse sibling constructor arguments
                    while (!self.check(.rparen) and !self.isAtEnd()) {
                        _ = self.advance();
                    }
                    _ = self.match(&[_]TokenType{.rparen});
                }
            }
        }

        // Expect proc
        _ = self.match(&[_]TokenType{.kw_proc});

        // Parse method body until endmethod
        var body: std.ArrayListUnmanaged(StmtIdx) = .{};
        errdefer body.deinit(self.allocator);

        while (!self.check(.kw_endmethod) and !self.isAtEnd()) {
            const stmt = self.parseStatement() catch |err| {
                self.synchronize();
                if (err == ParseError.OutOfMemory) return err;
                continue;
            };
            body.append(self.allocator, stmt) catch return ParseError.OutOfMemory;
        }

        _ = try self.consume(.kw_endmethod, "Expected 'endmethod'");

        // Create block for body
        const block_idx = self.store.addBlock(body.items, loc) catch return ParseError.OutOfMemory;
        body.deinit(self.allocator);

        // Generate method name - for constructor use __init__, for destructor use __deinit__
        var method_name_id: base.StringId = undefined;
        if (is_constructor) {
            method_name_id = try self.intern("__init__");
        } else if (is_destructor) {
            method_name_id = try self.intern("__deinit__");
        } else {
            method_name_id = try self.intern(method_name);
        }

        // Set return type: constructors/destructors return void
        if (is_constructor or is_destructor) {
            return_type = TypeIdx.null;
        }

        // Create fn_def for the method
        const fn_stmt = self.store.addFnDef(method_name_id, params.items, return_type, block_idx, loc) catch return ParseError.OutOfMemory;
        params.deinit(self.allocator);

        return fn_stmt;
    }

    /// Parse a namespace declaration
    /// Syntax: namespace Name.SubName
    ///             classes...
    ///         endnamespace
    fn parseNamespace(self: *Self) ParseError!StmtIdx {
        const loc = self.currentLoc();
        _ = self.advance(); // consume 'namespace'

        // Parse namespace name (can be dotted like MyApp.Models)
        var namespace_parts: std.ArrayListUnmanaged([]const u8) = .{};
        errdefer namespace_parts.deinit(self.allocator);

        const first_part = try self.consume(.identifier, "Expected namespace name");
        namespace_parts.append(self.allocator, first_part.lexeme) catch return ParseError.OutOfMemory;

        while (self.match(&[_]TokenType{.period})) {
            const part = try self.consume(.identifier, "Expected namespace component");
            namespace_parts.append(self.allocator, part.lexeme) catch return ParseError.OutOfMemory;
        }
        namespace_parts.deinit(self.allocator); // TODO: Use namespace name

        // Parse namespace body
        var stmts: std.ArrayListUnmanaged(StmtIdx) = .{};
        errdefer stmts.deinit(self.allocator);

        while (!self.check(.kw_endnamespace) and !self.isAtEnd()) {
            const stmt = self.parseStatement() catch |err| {
                self.synchronize();
                if (err == ParseError.OutOfMemory) return err;
                continue;
            };
            stmts.append(self.allocator, stmt) catch return ParseError.OutOfMemory;
        }

        _ = try self.consume(.kw_endnamespace, "Expected 'endnamespace'");

        // Return namespace contents as a block
        const result = self.store.addBlock(stmts.items, loc) catch return ParseError.OutOfMemory;
        stmts.deinit(self.allocator);
        return result;
    }

    /// Parse record block (local variable declarations)
    /// record [name] [,x]
    ///     var ,type
    ///     ...
    /// endrecord
    ///
    /// DBL record blocks declare local variables with their proper types.
    /// Record overlays (,X syntax) share memory with the previous record:
    ///   record info         ; Named non-overlaid record
    ///       time    ,d6
    ///       date    ,d6
    ///   record ,x           ; Unnamed overlay - shares memory with 'info'
    ///       hr      ,d2
    ///       min     ,d2
    ///       sec     ,d2
    fn parseRecord(self: *Self) ParseError!StmtIdx {
        const loc = self.currentLoc();
        _ = self.advance(); // consume 'record'

        // Check for record overlay syntax: record ,x or record name ,x
        var is_overlay = false;
        var overlay_base: ?base.StringId = null;

        // Check for optional record name followed by optional overlay specifier
        if (self.check(.identifier)) {
            // Could be a named record, or just fields - peek ahead
            const saved_pos = self.current;
            _ = self.advance(); // tentatively consume identifier

            if (self.match(&[_]TokenType{.comma})) {
                // Check if next is 'x' or another identifier (overlay target)
                if (self.check(.identifier)) {
                    const overlay_tok = self.peek();
                    var lower_buf: [16]u8 = undefined;
                    const lower_len = @min(overlay_tok.lexeme.len, lower_buf.len);
                    const lower = std.ascii.lowerString(lower_buf[0..lower_len], overlay_tok.lexeme[0..lower_len]);
                    if (std.mem.eql(u8, lower, "x")) {
                        // record name ,x syntax - named overlay at previous record
                        is_overlay = true;
                        overlay_base = self.last_record_base_field;
                        _ = self.advance(); // consume 'x'
                    } else {
                        // record name ,type - it's a type specifier, backtrack
                        self.current = saved_pos;
                    }
                } else {
                    // Just a comma after name with no identifier - backtrack
                    self.current = saved_pos;
                }
            } else {
                // No comma - it's a named record without overlay, backtrack
                self.current = saved_pos;
            }
        } else if (self.match(&[_]TokenType{.comma})) {
            // record ,x syntax - unnamed overlay
            if (self.check(.identifier)) {
                const overlay_tok = self.peek();
                var lower_buf: [16]u8 = undefined;
                const lower_len = @min(overlay_tok.lexeme.len, lower_buf.len);
                const lower = std.ascii.lowerString(lower_buf[0..lower_len], overlay_tok.lexeme[0..lower_len]);
                if (std.mem.eql(u8, lower, "x")) {
                    is_overlay = true;
                    overlay_base = self.last_record_base_field;
                    _ = self.advance(); // consume 'x'
                } else {
                    // record ,name - overlay at named record
                    is_overlay = true;
                    overlay_base = self.intern(overlay_tok.lexeme) catch null;
                    _ = self.advance(); // consume the overlay target name
                }
            }
        }

        var stmts: std.ArrayListUnmanaged(StmtIdx) = .{};
        errdefer stmts.deinit(self.allocator);

        // Track current byte offset for overlays
        var current_offset: i32 = 0;
        var first_field_id: ?base.StringId = null;

        // Parse variable declarations until endrecord
        while (!self.check(.kw_endrecord) and !self.isAtEnd()) {
            if (self.check(.identifier)) {
                const var_name_tok = self.advance();
                const var_name_id = try self.intern(var_name_tok.lexeme);

                // Track first field for future overlays
                if (first_field_id == null) {
                    first_field_id = var_name_id;
                }

                // Expect comma before type
                _ = self.match(&[_]TokenType{.comma});

                // Parse type specifier - could be primitive (d6, a30) or struct name
                var var_type = TypeIdx.null;
                var is_struct_type = false;
                var field_size: i32 = 0;
                if (self.check(.identifier)) {
                    const type_tok = self.advance();
                    const type_result = self.parseDblTypeSpecWithInfo(type_tok.lexeme);
                    var_type = type_result.type_idx;
                    is_struct_type = type_result.is_struct;
                    field_size = self.getFieldSize(type_tok.lexeme);
                } else {
                    var_type = self.store.addPrimitiveType(.i64) catch return ParseError.OutOfMemory;
                    field_size = 8; // default i64 size
                }

                // Check for @position (field overlay) syntax
                if (self.match(&[_]TokenType{.at})) {
                    // Parse base field name
                    const base_field_tok = try self.consume(.identifier, "Expected field name after '@'");
                    const base_field_id = try self.intern(base_field_tok.lexeme);

                    // Parse optional offset: + or - followed by number
                    var offset: i32 = 0;
                    if (self.match(&[_]TokenType{.plus})) {
                        if (self.check(.integer_literal)) {
                            const offset_tok = self.advance();
                            offset = @intCast(std.fmt.parseInt(i32, offset_tok.lexeme, 10) catch 0);
                        }
                    } else if (self.match(&[_]TokenType{.minus})) {
                        if (self.check(.integer_literal)) {
                            const offset_tok = self.advance();
                            offset = -@as(i32, @intCast(std.fmt.parseInt(i32, offset_tok.lexeme, 10) catch 0));
                        }
                    }

                    // Create field_view instead of let_decl
                    const view_stmt = self.store.addFieldView(var_name_id, var_type, base_field_id, offset, loc) catch return ParseError.OutOfMemory;
                    stmts.append(self.allocator, view_stmt) catch return ParseError.OutOfMemory;
                } else if (is_overlay and overlay_base != null) {
                    // This field overlays the base record at current_offset
                    const view_stmt = self.store.addFieldView(var_name_id, var_type, overlay_base.?, current_offset, loc) catch return ParseError.OutOfMemory;
                    stmts.append(self.allocator, view_stmt) catch return ParseError.OutOfMemory;
                    current_offset += field_size;
                } else {
                    // Check for optional initial value: , value
                    // DBL syntax: name, type, initial_value
                    var init_val = ExprIdx.null;
                    if (self.match(&[_]TokenType{.comma})) {
                        // Check if next token looks like a value (not a field name)
                        // Values can be: string_literal, integer_literal, decimal_literal, true, false, or expr starting with - or (
                        if (self.check(.string_literal) or self.check(.integer_literal) or
                            self.check(.decimal_literal) or self.check(.kw_true) or
                            self.check(.kw_false) or self.check(.minus) or self.check(.lparen))
                        {
                            init_val = self.parseExpression() catch ExprIdx.null;
                        } else {
                            // It's likely the next field name, backtrack
                            self.current -= 1;
                        }
                    }

                    // If no explicit init value, use defaults
                    if (init_val == ExprIdx.null and !is_struct_type) {
                        init_val = self.store.addIntLiteral(0, loc) catch return ParseError.OutOfMemory;
                    }

                    const var_stmt = self.store.addLetDecl(var_name_id, var_type, init_val, true, loc) catch return ParseError.OutOfMemory;
                    stmts.append(self.allocator, var_stmt) catch return ParseError.OutOfMemory;
                }
            } else {
                _ = self.advance();
            }
        }

        _ = try self.consume(.kw_endrecord, "Expected 'endrecord'");

        // Update last_record_base_field for future overlays (only for non-overlay records)
        if (!is_overlay and first_field_id != null) {
            self.last_record_base_field = first_field_id;
        }

        // Return as a block containing all the variable declarations
        const result = self.store.addBlock(stmts.items, loc) catch return ParseError.OutOfMemory;
        stmts.deinit(self.allocator); // Free ArrayList backing memory after data is copied
        return result;
    }

    /// Get the byte size of a DBL type for overlay offset calculations
    fn getFieldSize(self: *Self, type_str: []const u8) i32 {
        _ = self; // Not used but consistent with other methods
        if (type_str.len == 0) return 8; // default

        var lower_buf: [32]u8 = undefined;
        const len = @min(type_str.len, lower_buf.len);
        const lower = std.ascii.lowerString(lower_buf[0..len], type_str[0..len]);

        // .NET types
        if (std.mem.eql(u8, lower, "byte") or std.mem.eql(u8, lower, "sbyte")) return 1;
        if (std.mem.eql(u8, lower, "short") or std.mem.eql(u8, lower, "ushort")) return 2;
        if (std.mem.eql(u8, lower, "int") or std.mem.eql(u8, lower, "uint") or
            std.mem.eql(u8, lower, "integer") or std.mem.eql(u8, lower, "float"))
            return 4;
        if (std.mem.eql(u8, lower, "long") or std.mem.eql(u8, lower, "ulong") or
            std.mem.eql(u8, lower, "double"))
            return 8;

        // DBL types: a, d, i followed by size
        if (lower[0] == 'a' or lower[0] == 'd') {
            // Alpha and decimal sizes are in characters/digits
            if (len > 1) {
                const size = std.fmt.parseInt(i32, lower[1..len], 10) catch 1;
                return size;
            }
            return 1;
        }
        if (lower[0] == 'i') {
            // Integer size in bytes: i1=1, i2=2, i4=4, i8=8
            if (len > 1) {
                const size = std.fmt.parseInt(i32, lower[1..len], 10) catch 1;
                return size;
            }
            return 1;
        }

        return 8; // default for unknown types
    }

    /// Result of parsing a DBL type specifier
    const TypeSpecResult = struct {
        type_idx: TypeIdx,
        is_struct: bool,
    };

    /// Parse DBL type specifier and return whether it's a struct type
    fn parseDblTypeSpecWithInfo(self: *Self, type_str: []const u8) TypeSpecResult {
        if (type_str.len == 0) {
            return .{
                .type_idx = self.store.addPrimitiveType(.i64) catch TypeIdx.null,
                .is_struct = false,
            };
        }

        // Normalize to lowercase for case-insensitive comparison
        var lower_buf: [64]u8 = undefined;
        const len = @min(type_str.len, lower_buf.len);
        const lower = std.ascii.lowerString(lower_buf[0..len], type_str[0..len]);

        // Check for C#/.NET-style types (used in DBL with comma prefix, e.g., ,string, ,int, ,boolean)
        // Reference: DBL LRM Chapter 2 - Data Types (.NET Type Aliases)
        if (std.mem.eql(u8, lower, "string")) {
            return .{
                .type_idx = self.store.addPrimitiveType(.string) catch TypeIdx.null,
                .is_struct = false,
            };
        }
        if (std.mem.eql(u8, lower, "int") or std.mem.eql(u8, lower, "integer")) {
            // DBL int/integer maps to i4 (4 bytes = 32 bits)
            return .{
                .type_idx = self.store.addPrimitiveType(.i32) catch TypeIdx.null,
                .is_struct = false,
            };
        }
        if (std.mem.eql(u8, lower, "uint")) {
            // DBL uint maps to u4 (4 bytes = 32 bits unsigned)
            return .{
                .type_idx = self.store.addPrimitiveType(.u32) catch TypeIdx.null,
                .is_struct = false,
            };
        }
        if (std.mem.eql(u8, lower, "short")) {
            // DBL short maps to i2 (2 bytes = 16 bits)
            return .{
                .type_idx = self.store.addPrimitiveType(.i16) catch TypeIdx.null,
                .is_struct = false,
            };
        }
        if (std.mem.eql(u8, lower, "ushort")) {
            // DBL ushort maps to u2 (2 bytes = 16 bits unsigned)
            return .{
                .type_idx = self.store.addPrimitiveType(.u16) catch TypeIdx.null,
                .is_struct = false,
            };
        }
        if (std.mem.eql(u8, lower, "long")) {
            // DBL long maps to i8 (8 bytes = 64 bits)
            return .{
                .type_idx = self.store.addPrimitiveType(.i64) catch TypeIdx.null,
                .is_struct = false,
            };
        }
        if (std.mem.eql(u8, lower, "ulong")) {
            // DBL ulong maps to u8 (8 bytes = 64 bits unsigned)
            return .{
                .type_idx = self.store.addPrimitiveType(.u64) catch TypeIdx.null,
                .is_struct = false,
            };
        }
        if (std.mem.eql(u8, lower, "byte")) {
            // DBL byte maps to u1 (1 byte = 8 bits unsigned)
            return .{
                .type_idx = self.store.addPrimitiveType(.u8) catch TypeIdx.null,
                .is_struct = false,
            };
        }
        if (std.mem.eql(u8, lower, "sbyte")) {
            // DBL sbyte maps to i1 (1 byte = 8 bits signed)
            return .{
                .type_idx = self.store.addPrimitiveType(.i8) catch TypeIdx.null,
                .is_struct = false,
            };
        }
        if (std.mem.eql(u8, lower, "boolean") or std.mem.eql(u8, lower, "bool")) {
            return .{
                .type_idx = self.store.addPrimitiveType(.bool) catch TypeIdx.null,
                .is_struct = false,
            };
        }
        if (std.mem.eql(u8, lower, "float")) {
            // DBL float maps to f32 (single precision)
            return .{
                .type_idx = self.store.addPrimitiveType(.f32) catch TypeIdx.null,
                .is_struct = false,
            };
        }
        if (std.mem.eql(u8, lower, "double")) {
            // DBL double maps to f64 (double precision)
            return .{
                .type_idx = self.store.addPrimitiveType(.f64) catch TypeIdx.null,
                .is_struct = false,
            };
        }
        if (std.mem.eql(u8, lower, "decimal")) {
            // DBL decimal maps to d28.10 (high-precision decimal)
            return .{
                .type_idx = self.store.addDecimalType(28, 10) catch TypeIdx.null,
                .is_struct = false,
            };
        }

        // Check for array types: <digits><type_char><digits> e.g., 10a20, 10d4
        // The pattern is: array_size (digits) + type char (a/d/i) + element_size (digits)
        if (std.ascii.isDigit(lower[0])) {
            // Find the type character (a, d, or i)
            var type_char_idx: usize = 0;
            for (lower, 0..) |c, i| {
                if (c == 'a' or c == 'd' or c == 'i') {
                    type_char_idx = i;
                    break;
                }
            }
            if (type_char_idx > 0) {
                // Parse array size from the digits before the type char
                const array_size = std.fmt.parseInt(u32, lower[0..type_char_idx], 10) catch 1;
                const type_char = lower[type_char_idx];

                // Create element type based on the type character
                const elem_type = if (type_char == 'a')
                    self.store.addPrimitiveType(.string) catch TypeIdx.null
                else
                    self.store.addPrimitiveType(.i64) catch TypeIdx.null;

                // Create array type with element type and size
                const array_type = self.store.addArrayType(elem_type, array_size) catch TypeIdx.null;
                return .{
                    .type_idx = array_type,
                    .is_struct = false,
                };
            }
        }

        // Check if it's a primitive DBL type specifier
        // DBL primitives are: d, a, i alone OR d<digits>, a<digits>, i<digits> (e.g., d6, a30, i4)
        // Also handles d<digits>.<digits> for decimal with scale (e.g., d10.2)
        // NOT struct names that happen to start with a/d/i (e.g., app_state_t, data_t)
        if (lower[0] == 'd' or lower[0] == 'a' or lower[0] == 'i') {
            // Single letter (d, a, i) is a valid DBL type
            // Or letter followed by digits (and optional .digits for decimals)
            var is_primitive = (len == 1);
            var dot_pos: ?usize = null;
            if (!is_primitive and len >= 2) {
                var valid = true;
                for (lower[1..len], 1..) |c, idx| {
                    if (c == '.' and lower[0] == 'd') {
                        // Only decimals can have a dot
                        if (dot_pos != null) {
                            valid = false; // Multiple dots
                            break;
                        }
                        dot_pos = idx;
                    } else if (!std.ascii.isDigit(c)) {
                        valid = false;
                        break;
                    }
                }
                is_primitive = valid;
            }
            if (is_primitive) {
                // It's a DBL primitive type specifier

                // DBL type distinction:
                // - 'a' (alpha): arbitrary characters, stored as [N]u8 array
                // - 'd' (decimal): numeric only, stored as decimal type (zero-padded on store)
                // - 'i' (integer): numeric only, stored as integer
                // DBL integer types use BYTE counts: i1=1byte, i2=2bytes, i4=4bytes, i8=8bytes
                // Cot/Zig uses BIT counts: i8=8bits, i16=16bits, i32=32bits, i64=64bits
                if (lower[0] == 'd') {
                    // Decimal type: numeric with precision and optional scale
                    // d = default precision (18), scale 0
                    // d10 = precision 10, scale 0
                    // d10.2 = precision 10, scale 2
                    var precision: u32 = 18; // Default precision for bare 'd'
                    var scale: u32 = 0;

                    if (len > 1) {
                        if (dot_pos) |dp| {
                            // Parse precision before dot, scale after
                            precision = std.fmt.parseInt(u32, lower[1..dp], 10) catch 1;
                            if (dp + 1 < len) {
                                scale = std.fmt.parseInt(u32, lower[dp + 1 .. len], 10) catch 0;
                            }
                        } else {
                            // Just precision, no scale
                            precision = std.fmt.parseInt(u32, lower[1..len], 10) catch 1;
                        }
                    }

                    return .{
                        .type_idx = self.store.addDecimalType(precision, scale) catch TypeIdx.null,
                        .is_struct = false,
                    };
                } else if (lower[0] == 'i') {
                    // Parse the size for integer types
                    const size: u32 = if (len > 1)
                        std.fmt.parseInt(u32, lower[1..len], 10) catch 1
                    else
                        1;
                    // Integer type: map DBL byte count to Zig bit count
                    // i1 (1 byte) -> i8, i2 (2 bytes) -> i16, i4 (4 bytes) -> i32, i8 (8 bytes) -> i64
                    const int_type: ast.types.TypeTag = switch (size) {
                        1 => .i8,
                        2 => .i16,
                        4 => .i32,
                        else => .i64, // i8 and any other size defaults to i64
                    };
                    return .{
                        .type_idx = self.store.addPrimitiveType(int_type) catch TypeIdx.null,
                        .is_struct = false,
                    };
                } else {
                    // Alpha: fixed-length string as [N]u8 array (Zig-standard)
                    const alpha_size: u32 = if (len > 1)
                        std.fmt.parseInt(u32, lower[1..len], 10) catch 1
                    else
                        1;
                    const u8_type = self.store.addPrimitiveType(.u8) catch TypeIdx.null;
                    return .{
                        .type_idx = self.store.addArrayType(u8_type, alpha_size) catch TypeIdx.null,
                        .is_struct = false,
                    };
                }
            }
        }

        // It's a struct type reference
        const type_name_id = self.intern(type_str) catch return .{ .type_idx = TypeIdx.null, .is_struct = true };
        return .{
            .type_idx = self.store.addNamedType(type_name_id) catch TypeIdx.null,
            .is_struct = true,
        };
    }

    /// Parse DBL type specifier (d6, a30, i4, etc.)
    fn parseDblTypeSpec(self: *Self, type_str: []const u8) ParseError!TypeIdx {
        const result = self.parseDblTypeSpecWithInfo(type_str);
        return result.type_idx;
    }

    // ============================================================
    // Using/Select Statement
    // ============================================================

    /// Parse using/select statement (like switch/case)
    /// using expr select
    /// (value1),
    ///     statements
    /// (value2),
    ///     statements
    /// endusing
    ///
    /// Translates to if-else chain:
    ///   if (expr == value1) { ... }
    ///   else if (expr == value2) { ... }
    fn parseUsing(self: *Self) ParseError!StmtIdx {
        const loc = self.currentLoc();
        try self.enterNesting();
        defer self.exitNesting();

        _ = self.advance(); // consume 'using'

        const switch_expr = try self.parseExpression();

        // Optional 'select'
        _ = self.match(&[_]TokenType{.kw_select});

        // Collect all cases first
        const Case = struct {
            value: ExprIdx,
            body: StmtIdx,
        };
        var cases: std.ArrayListUnmanaged(Case) = .{};
        defer cases.deinit(self.allocator);

        // Parse cases until endusing
        while (!self.check(.kw_endusing) and !self.isAtEnd()) {
            // Case format: (value),
            //     statements
            if (self.check(.lparen)) {
                _ = self.advance(); // consume '('

                // Parse case value
                var case_value = ExprIdx.null;
                if (!self.check(.rparen)) {
                    case_value = try self.parseExpression();
                }

                _ = try self.consume(.rparen, "Expected ')'");
                _ = self.match(&[_]TokenType{.comma}); // optional comma

                // Parse statements until next case or endusing
                var case_stmts: std.ArrayListUnmanaged(StmtIdx) = .{};
                errdefer case_stmts.deinit(self.allocator);

                while (!self.check(.lparen) and !self.check(.kw_endusing) and !self.isAtEnd()) {
                    if (self.parseStatement()) |stmt| {
                        case_stmts.append(self.allocator, stmt) catch return ParseError.OutOfMemory;
                    } else |err| {
                        if (err == ParseError.OutOfMemory) return err;
                        self.synchronize();
                    }
                }

                const case_body = self.store.addBlock(case_stmts.items, loc) catch return ParseError.OutOfMemory;
                case_stmts.deinit(self.allocator); // Free ArrayList backing memory after data is copied
                cases.append(self.allocator, .{
                    .value = case_value,
                    .body = case_body,
                }) catch return ParseError.OutOfMemory;
            } else {
                _ = self.advance();
            }
        }

        _ = try self.consume(.kw_endusing, "Expected 'endusing'");

        // Build if-else chain from cases (reverse order to build from innermost)
        if (cases.items.len == 0) {
            // No cases - return empty block
            return self.store.addBlock(&[_]StmtIdx{}, loc) catch return ParseError.OutOfMemory;
        }

        // Start with the last case (no else clause)
        var i: usize = cases.items.len - 1;
        var current_stmt = cases.items[i].body;

        // Build from second-to-last up to first
        while (i > 0) {
            i -= 1;
            const case = cases.items[i];
            // Create condition: switch_expr == case_value
            const condition = if (case.value == ExprIdx.null)
                // Empty case value = default/else - use true
                self.store.addBoolLiteral(true, loc) catch return ParseError.OutOfMemory
            else
                self.store.addBinary(switch_expr, .eq, case.value, loc) catch return ParseError.OutOfMemory;

            current_stmt = self.store.addIfStmt(condition, case.body, current_stmt, loc) catch return ParseError.OutOfMemory;
        }

        // Handle the first case (or only case)
        if (cases.items.len == 1) {
            const case = cases.items[0];
            const condition = if (case.value == ExprIdx.null)
                self.store.addBoolLiteral(true, loc) catch return ParseError.OutOfMemory
            else
                self.store.addBinary(switch_expr, .eq, case.value, loc) catch return ParseError.OutOfMemory;
            return self.store.addIfStmt(condition, case.body, StmtIdx.null, loc) catch return ParseError.OutOfMemory;
        }

        return current_stmt;
    }

    // ============================================================
    // Error Handling
    // ============================================================

    /// Parse onerror statement (sets up error handler)
    /// onerror label_name
    fn parseOnError(self: *Self) ParseError!StmtIdx {
        const loc = self.currentLoc();
        _ = self.advance(); // consume 'onerror'

        // Parse label name (but don't use it as an identifier expression)
        _ = try self.consume(.identifier, "Expected error label");

        // For now, emit as noop - proper error handling needs runtime support
        const zero = self.store.addIntLiteral(0, loc) catch return ParseError.OutOfMemory;
        return self.store.addExprStmt(zero, loc) catch return ParseError.OutOfMemory;
    }

    /// Parse offerror statement (clears error handler)
    fn parseOffError(self: *Self) ParseError!StmtIdx {
        const loc = self.currentLoc();
        _ = self.advance(); // consume 'offerror'

        // Emit as noop for now
        const zero = self.store.addIntLiteral(0, loc) catch return ParseError.OutOfMemory;
        return self.store.addExprStmt(zero, loc) catch return ParseError.OutOfMemory;
    }

    // ============================================================
    // Data Manipulation
    // ============================================================

    /// Parse clear statement (zeros out a variable/record)
    /// clear variable
    fn parseClear(self: *Self) ParseError!StmtIdx {
        const loc = self.currentLoc();
        _ = self.advance(); // consume 'clear'

        const var_expr = try self.parseExpression();

        // Emit as assignment to zero/empty
        // For now, assign 0 - the runtime can handle type-specific clearing
        const zero = self.store.addIntLiteral(0, loc) catch return ParseError.OutOfMemory;
        return self.store.addAssignment(var_expr, zero, loc) catch return ParseError.OutOfMemory;
    }

    // ============================================================
    // Function Calls
    // ============================================================

    fn parseXCall(self: *Self) ParseError!StmtIdx {
        const loc = self.currentLoc();
        _ = self.advance(); // consume 'xcall'

        const name_tok = try self.consume(.identifier, "Expected function name");
        const name_id = try self.intern(name_tok.lexeme);
        const callee = self.store.addIdentifier(name_id, loc) catch return ParseError.OutOfMemory;

        var args: std.ArrayListUnmanaged(ExprIdx) = .{};
        errdefer args.deinit(self.allocator);

        // Parse arguments if present
        if (self.check(.lparen)) {
            _ = self.advance();
            if (!self.check(.rparen)) {
                args.append(self.allocator, try self.parseExpression()) catch return ParseError.OutOfMemory;
                while (self.match(&[_]TokenType{.comma})) {
                    args.append(self.allocator, try self.parseExpression()) catch return ParseError.OutOfMemory;
                }
            }
            _ = try self.consume(.rparen, "Expected ')'");
        }

        const call_expr = self.store.addCall(callee, args.items, loc) catch return ParseError.OutOfMemory;
        args.deinit(self.allocator); // Free ArrayList backing memory after data is copied
        return self.store.addExprStmt(call_expr, loc) catch return ParseError.OutOfMemory;
    }

    // ============================================================
    // Identifier Statements (assignment, call, or label)
    // ============================================================

    fn parseIdentifierStatement(self: *Self) ParseError!StmtIdx {
        const loc = self.currentLoc();

        // Check for label declaration: identifier followed by comma
        // e.g., "eof_reached," - used with onerror/goto
        if (self.tokens.len > self.current + 1 and
            self.tokens[self.current].type == .identifier and
            self.tokens[self.current + 1].type == .comma)
        {
            // It's a label - consume identifier and comma, emit as noop
            _ = self.advance(); // identifier
            _ = self.advance(); // comma
            const zero = self.store.addIntLiteral(0, loc) catch return ParseError.OutOfMemory;
            return self.store.addExprStmt(zero, loc) catch return ParseError.OutOfMemory;
        }

        // Check for console.log() - transform to t_print call
        if (self.tryParseConsoleLog()) |stmt| {
            return stmt;
        } else |_| {
            // Fall through
        }

        // Check for DBL I/O statements with special mode syntax
        // e.g., open(ch, U, "file") - U is a mode specifier, not a variable
        if (self.tokens[self.current].type == .identifier) {
            if (self.tryParseDblIoStatement()) |stmt| {
                return stmt;
            } else |_| {
                // Fall through to regular expression parsing
            }
        }

        const expr = try self.parseExpression();

        // Check for assignment
        if (self.match(&[_]TokenType{.equals})) {
            const value = try self.parseExpression();
            return self.store.addAssignment(expr, value, loc) catch return ParseError.OutOfMemory;
        }

        // Otherwise it's an expression statement (function call)
        return self.store.addExprStmt(expr, loc) catch return ParseError.OutOfMemory;
    }

    /// Try to parse console.log() - transform to t_print call
    fn tryParseConsoleLog(self: *Self) ParseError!StmtIdx {
        // Check pattern: console.log(...)
        if (self.current + 3 >= self.tokens.len) return ParseError.InvalidStatement;

        const id_tok = self.tokens[self.current];
        if (id_tok.type != .identifier) return ParseError.InvalidStatement;

        // Check for "console"
        var lower_buf: [16]u8 = undefined;
        const id_len = @min(id_tok.lexeme.len, lower_buf.len);
        const id_lower = std.ascii.lowerString(lower_buf[0..id_len], id_tok.lexeme[0..id_len]);
        if (!std.mem.eql(u8, id_lower, "console")) return ParseError.InvalidStatement;

        // Check for "."
        if (self.tokens[self.current + 1].type != .period) return ParseError.InvalidStatement;

        // Check for "log"
        const member_tok = self.tokens[self.current + 2];
        if (member_tok.type != .identifier) return ParseError.InvalidStatement;
        const member_len = @min(member_tok.lexeme.len, lower_buf.len);
        const member_lower = std.ascii.lowerString(lower_buf[0..member_len], member_tok.lexeme[0..member_len]);
        if (!std.mem.eql(u8, member_lower, "log")) return ParseError.InvalidStatement;

        // Check for "("
        if (self.tokens[self.current + 3].type != .lparen) return ParseError.InvalidStatement;

        // It's console.log(...) - parse as t_print call
        const loc = self.currentLoc();
        _ = self.advance(); // consume 'console'
        _ = self.advance(); // consume '.'
        _ = self.advance(); // consume 'log'
        _ = self.advance(); // consume '('

        // Parse arguments
        var args: std.ArrayListUnmanaged(ExprIdx) = .{};
        errdefer args.deinit(self.allocator);

        if (!self.check(.rparen)) {
            args.append(self.allocator, try self.parseExpression()) catch return ParseError.OutOfMemory;
            while (self.match(&[_]TokenType{.comma})) {
                args.append(self.allocator, try self.parseExpression()) catch return ParseError.OutOfMemory;
            }
        }
        _ = try self.consume(.rparen, "Expected ')' after console.log arguments");

        // Create console.log call - the bytecode emitter recognizes "console.log"
        // and emits the console_log opcode (writes to dev pane in TUI)
        const func_name = self.intern("console.log") catch return ParseError.OutOfMemory;
        const func_expr = self.store.addIdentifier(func_name, loc) catch return ParseError.OutOfMemory;
        const call_expr = self.store.addCall(func_expr, args.items, loc) catch return ParseError.OutOfMemory;
        args.deinit(self.allocator); // Free ArrayList backing memory after data is copied
        return self.store.addExprStmt(call_expr, loc) catch return ParseError.OutOfMemory;
    }

    /// Try to parse a DBL I/O statement (open, close, store, read, write, find)
    /// These have special syntax where mode specifiers (I, O, U, A) are literals, not variables
    fn tryParseDblIoStatement(self: *Self) ParseError!StmtIdx {
        const name_tok = self.peek();

        // Normalize to lowercase
        var lower_buf: [16]u8 = undefined;
        const name_len = @min(name_tok.lexeme.len, lower_buf.len);
        const name = std.ascii.lowerString(lower_buf[0..name_len], name_tok.lexeme[0..name_len]);

        // Check if it's followed by a paren (function call syntax)
        if (self.current + 1 >= self.tokens.len or self.tokens[self.current + 1].type != .lparen) {
            return ParseError.InvalidStatement;
        }

        // Handle DBL I/O statements
        if (std.mem.eql(u8, name, "open")) {
            return self.parseDblOpen();
        } else if (std.mem.eql(u8, name, "close")) {
            return self.parseDblClose();
        } else if (std.mem.eql(u8, name, "store") or std.mem.eql(u8, name, "read") or
            std.mem.eql(u8, name, "write") or std.mem.eql(u8, name, "find"))
        {
            return self.parseDblIoOp(name);
        }

        return ParseError.InvalidStatement;
    }

    /// Parse: open(channel, mode, filename)
    /// Mode is I, O, U, or A - converted to string literal
    fn parseDblOpen(self: *Self) ParseError!StmtIdx {
        const loc = self.currentLoc();
        _ = self.advance(); // consume 'open'
        _ = try self.consume(.lparen, "Expected '('");

        // Parse channel expression
        const channel = try self.parseExpression();
        _ = try self.consume(.comma, "Expected ','");

        // Parse mode - could be I, O, U, A (as identifier) or as expression
        var mode_expr: ExprIdx = undefined;
        if (self.check(.identifier)) {
            const mode_tok = self.advance();
            // Convert mode letter to string literal
            const mode_id = try self.intern(mode_tok.lexeme);
            mode_expr = self.store.addStringLiteral(mode_id, loc) catch return ParseError.OutOfMemory;
        } else {
            mode_expr = try self.parseExpression();
        }
        _ = try self.consume(.comma, "Expected ','");

        // Parse filename expression
        const filename = try self.parseExpression();
        _ = try self.consume(.rparen, "Expected ')'");

        // Emit as xcall db_open(channel, mode, filename)
        const func_id = try self.intern("db_open");
        const callee = self.store.addIdentifier(func_id, loc) catch return ParseError.OutOfMemory;
        const args = [_]ExprIdx{ channel, mode_expr, filename };
        const call_expr = self.store.addCall(callee, &args, loc) catch return ParseError.OutOfMemory;
        return self.store.addExprStmt(call_expr, loc) catch return ParseError.OutOfMemory;
    }

    /// Parse: close(channel)
    fn parseDblClose(self: *Self) ParseError!StmtIdx {
        const loc = self.currentLoc();
        _ = self.advance(); // consume 'close'
        _ = try self.consume(.lparen, "Expected '('");

        const channel = try self.parseExpression();
        _ = try self.consume(.rparen, "Expected ')'");

        // Emit as xcall db_close(channel)
        const func_id = try self.intern("db_close");
        const callee = self.store.addIdentifier(func_id, loc) catch return ParseError.OutOfMemory;
        const args = [_]ExprIdx{channel};
        const call_expr = self.store.addCall(callee, &args, loc) catch return ParseError.OutOfMemory;
        return self.store.addExprStmt(call_expr, loc) catch return ParseError.OutOfMemory;
    }

    /// Parse: store(channel, record), read(channel, record, key), write(channel, record), find(channel, record, key)
    fn parseDblIoOp(self: *Self, op_name: []const u8) ParseError!StmtIdx {
        const loc = self.currentLoc();
        _ = self.advance(); // consume operation name
        _ = try self.consume(.lparen, "Expected '('");

        var args_list: std.ArrayListUnmanaged(ExprIdx) = .{};
        defer args_list.deinit(self.allocator);

        // Parse all arguments
        if (!self.check(.rparen)) {
            args_list.append(self.allocator, try self.parseExpression()) catch return ParseError.OutOfMemory;
            while (self.match(&[_]TokenType{.comma})) {
                args_list.append(self.allocator, try self.parseExpression()) catch return ParseError.OutOfMemory;
            }
        }
        _ = try self.consume(.rparen, "Expected ')'");

        // For read/reads: emit as assignment to record variable
        // read(channel, record, key) -> record = db_read(channel, key)
        // reads(channel, record) -> record = db_readnext(channel)
        const is_read = std.mem.eql(u8, op_name, "read");
        const is_reads = std.mem.eql(u8, op_name, "reads");

        if ((is_read or is_reads) and args_list.items.len >= 2) {
            // Second argument is the record target
            const record_target = args_list.items[1];

            // Build call args: [channel] for reads, [channel, key] for read
            var call_args: std.ArrayListUnmanaged(ExprIdx) = .{};
            defer call_args.deinit(self.allocator);
            call_args.append(self.allocator, args_list.items[0]) catch return ParseError.OutOfMemory; // channel

            if (is_read and args_list.items.len >= 3) {
                call_args.append(self.allocator, args_list.items[2]) catch return ParseError.OutOfMemory; // key
            }

            // Emit as: record = db_read/db_readnext(channel, ...)
            var func_name_buf: [32]u8 = undefined;
            const func_name = if (is_reads)
                "db_readnext"
            else
                std.fmt.bufPrint(&func_name_buf, "db_{s}", .{op_name}) catch "db_io";
            const func_id = try self.intern(func_name);
            const callee = self.store.addIdentifier(func_id, loc) catch return ParseError.OutOfMemory;
            const call_expr = self.store.addCall(callee, call_args.items, loc) catch return ParseError.OutOfMemory;

            // Create assignment: record = call_result
            return self.store.addAssignment(record_target, call_expr, loc) catch return ParseError.OutOfMemory;
        }

        // For other ops (store, write, find): emit as xcall db_<op>(args...)
        var func_name_buf: [32]u8 = undefined;
        const func_name = std.fmt.bufPrint(&func_name_buf, "db_{s}", .{op_name}) catch "db_io";
        const func_id = try self.intern(func_name);
        const callee = self.store.addIdentifier(func_id, loc) catch return ParseError.OutOfMemory;
        const call_expr = self.store.addCall(callee, args_list.items, loc) catch return ParseError.OutOfMemory;
        return self.store.addExprStmt(call_expr, loc) catch return ParseError.OutOfMemory;
    }

    // ============================================================
    // Expressions
    // ============================================================

    fn parseExpression(self: *Self) ParseError!ExprIdx {
        return self.parseNullCoalesce();
    }

    // Null coalescing: ?? (lowest precedence binary operator)
    fn parseNullCoalesce(self: *Self) ParseError!ExprIdx {
        var expr = try self.parseOr();

        while (self.match(&[_]TokenType{.op_null_coalesce})) {
            const loc = self.currentLoc();
            const right = try self.parseOr();
            // TODO: Add null_coalesce to BinaryOp, for now treat as logical or
            expr = self.store.addBinary(expr, .@"or", right, loc) catch return ParseError.OutOfMemory;
        }

        return expr;
    }

    // Logical OR: .OR. or ||
    fn parseOr(self: *Self) ParseError!ExprIdx {
        var expr = try self.parseXor();

        while (self.match(&[_]TokenType{.op_or})) {
            const loc = self.currentLoc();
            const right = try self.parseXor();
            expr = self.store.addBinary(expr, .@"or", right, loc) catch return ParseError.OutOfMemory;
        }

        return expr;
    }

    // Logical XOR: .XOR.
    fn parseXor(self: *Self) ParseError!ExprIdx {
        var expr = try self.parseAnd();

        while (self.match(&[_]TokenType{.op_xor})) {
            const loc = self.currentLoc();
            const right = try self.parseAnd();
            // XOR is implemented as (a OR b) AND NOT (a AND b), map to bit_xor for now
            expr = self.store.addBinary(expr, .bit_xor, right, loc) catch return ParseError.OutOfMemory;
        }

        return expr;
    }

    // Logical AND: .AND. or &&
    fn parseAnd(self: *Self) ParseError!ExprIdx {
        var expr = try self.parseBitwiseOr();

        while (self.match(&[_]TokenType{.op_and})) {
            const loc = self.currentLoc();
            const right = try self.parseBitwiseOr();
            expr = self.store.addBinary(expr, .@"and", right, loc) catch return ParseError.OutOfMemory;
        }

        return expr;
    }

    // Bitwise OR: .BOR. or |
    fn parseBitwiseOr(self: *Self) ParseError!ExprIdx {
        var expr = try self.parseBitwiseXor();

        while (self.match(&[_]TokenType{.op_bor})) {
            const loc = self.currentLoc();
            const right = try self.parseBitwiseXor();
            expr = self.store.addBinary(expr, .bit_or, right, loc) catch return ParseError.OutOfMemory;
        }

        return expr;
    }

    // Bitwise XOR: .BXOR. or ^
    fn parseBitwiseXor(self: *Self) ParseError!ExprIdx {
        var expr = try self.parseBitwiseAnd();

        while (self.match(&[_]TokenType{.op_bxor})) {
            const loc = self.currentLoc();
            const right = try self.parseBitwiseAnd();
            expr = self.store.addBinary(expr, .bit_xor, right, loc) catch return ParseError.OutOfMemory;
        }

        return expr;
    }

    // Bitwise AND: .BAND. or &, also .BNAND.
    fn parseBitwiseAnd(self: *Self) ParseError!ExprIdx {
        var expr = try self.parseEquality();

        while (self.match(&[_]TokenType{ .op_band, .op_bnand })) {
            const loc = self.currentLoc();
            const op_tok = self.previous();
            const right = try self.parseEquality();

            if (op_tok.type == .op_bnand) {
                // BNAND is NOT(a AND b) - create AND then wrap in NOT
                const and_expr = self.store.addBinary(expr, .bit_and, right, loc) catch return ParseError.OutOfMemory;
                expr = self.store.addUnary(.bit_not, and_expr, loc) catch return ParseError.OutOfMemory;
            } else {
                expr = self.store.addBinary(expr, .bit_and, right, loc) catch return ParseError.OutOfMemory;
            }
        }

        return expr;
    }

    // Equality: .EQ./==, .NE./!=, .EQS., .NES.
    fn parseEquality(self: *Self) ParseError!ExprIdx {
        var expr = try self.parseComparison();

        while (self.match(&[_]TokenType{ .op_eq, .op_ne, .op_eqs, .op_nes })) {
            const loc = self.currentLoc();
            const op_tok = self.previous();
            const right = try self.parseComparison();
            // .EQS./.NES. are full-length string comparison, same AST op - IR handles semantics
            const op: BinaryOp = switch (op_tok.type) {
                .op_eq, .op_eqs => .eq,
                .op_ne, .op_nes => .ne,
                else => .eq,
            };
            expr = self.store.addBinary(expr, op, right, loc) catch return ParseError.OutOfMemory;
        }

        return expr;
    }

    // Comparison: .LT./<, .LE./<=, .GT./>, .GE./>=, .GTS., .LTS., .GES., .LES.
    fn parseComparison(self: *Self) ParseError!ExprIdx {
        var expr = try self.parseShift();

        while (self.match(&[_]TokenType{ .op_lt, .op_le, .op_gt, .op_ge, .op_gts, .op_lts, .op_ges, .op_les })) {
            const loc = self.currentLoc();
            const op_tok = self.previous();
            const right = try self.parseShift();
            // String comparison operators map to same AST ops - IR handles semantics
            const op: BinaryOp = switch (op_tok.type) {
                .op_lt, .op_lts => .lt,
                .op_le, .op_les => .le,
                .op_gt, .op_gts => .gt,
                .op_ge, .op_ges => .ge,
                else => .lt,
            };
            expr = self.store.addBinary(expr, op, right, loc) catch return ParseError.OutOfMemory;
        }

        return expr;
    }

    // Bit shift: << and >>
    fn parseShift(self: *Self) ParseError!ExprIdx {
        var expr = try self.parseTerm();

        while (self.match(&[_]TokenType{ .op_shl, .op_shr })) {
            const loc = self.currentLoc();
            const op_tok = self.previous();
            const right = try self.parseTerm();
            const op: BinaryOp = if (op_tok.type == .op_shl) .shl else .shr;
            expr = self.store.addBinary(expr, op, right, loc) catch return ParseError.OutOfMemory;
        }

        return expr;
    }

    // Addition/Subtraction: +, -
    fn parseTerm(self: *Self) ParseError!ExprIdx {
        var expr = try self.parseFactor();

        while (self.match(&[_]TokenType{ .plus, .minus })) {
            const loc = self.currentLoc();
            const op_tok = self.previous();
            const right = try self.parseFactor();
            const op: BinaryOp = if (op_tok.type == .plus) .add else .sub;
            expr = self.store.addBinary(expr, op, right, loc) catch return ParseError.OutOfMemory;
        }

        return expr;
    }

    // Multiplication/Division/Modulo: *, /, .MOD./%
    fn parseFactor(self: *Self) ParseError!ExprIdx {
        var expr = try self.parseRounding();

        while (self.match(&[_]TokenType{ .star, .slash, .op_mod })) {
            const loc = self.currentLoc();
            const op_tok = self.previous();
            const right = try self.parseRounding();
            const op: BinaryOp = switch (op_tok.type) {
                .star => .mul,
                .slash => .div,
                .op_mod => .mod,
                else => .mul,
            };
            expr = self.store.addBinary(expr, op, right, loc) catch return ParseError.OutOfMemory;
        }

        return expr;
    }

    // Rounding operators: # (truncating), ## (true rounding)
    // These are DBL-specific binary operators: value # places, value ## places
    // # truncates to N decimal places, ## rounds to N decimal places
    fn parseRounding(self: *Self) ParseError!ExprIdx {
        var expr = try self.parseUnary();

        while (self.match(&[_]TokenType{ .op_round, .op_round_true })) {
            const loc = self.currentLoc();
            const op_tok = self.previous();
            const places = try self.parseUnary();

            // DBL: # is truncating round, ## is true round
            const op: BinaryOp = if (op_tok.type == .op_round_true) .round else .trunc;
            expr = self.store.addBinary(expr, op, places, loc) catch return ParseError.OutOfMemory;
        }

        return expr;
    }

    // Unary operators: -, !, ~, .NOT., .BNOT.
    fn parseUnary(self: *Self) ParseError!ExprIdx {
        if (self.match(&[_]TokenType{ .minus, .op_not, .kw_not, .op_bnot })) {
            const loc = self.currentLoc();
            const op_tok = self.previous();
            const operand = try self.parseUnary();
            const op: UnaryOp = switch (op_tok.type) {
                .minus => .neg,
                .op_not, .kw_not => .not,
                .op_bnot => .bit_not,
                else => .not,
            };
            return self.store.addUnary(op, operand, loc) catch return ParseError.OutOfMemory;
        }

        return self.parsePrimary();
    }

    fn parsePrimary(self: *Self) ParseError!ExprIdx {
        const loc = self.currentLoc();
        const token = self.peek();

        switch (token.type) {
            .integer_literal => {
                _ = self.advance();
                const value = std.fmt.parseInt(i64, token.lexeme, 10) catch 0;
                return self.store.addIntLiteral(value, loc) catch return ParseError.OutOfMemory;
            },
            .decimal_literal => {
                _ = self.advance();
                const value = std.fmt.parseFloat(f64, token.lexeme) catch 0.0;
                return self.store.addFloatLiteral(value, loc) catch return ParseError.OutOfMemory;
            },
            .string_literal => {
                _ = self.advance();
                // Remove quotes (preserve case for string literals)
                const content = if (token.lexeme.len >= 2)
                    token.lexeme[1 .. token.lexeme.len - 1]
                else
                    token.lexeme;
                const str_id = try self.internLiteral(content);
                return self.store.addStringLiteral(str_id, loc) catch return ParseError.OutOfMemory;
            },
            .kw_true => {
                _ = self.advance();
                return self.store.addBoolLiteral(true, loc) catch return ParseError.OutOfMemory;
            },
            .kw_false => {
                _ = self.advance();
                return self.store.addBoolLiteral(false, loc) catch return ParseError.OutOfMemory;
            },
            .identifier => {
                _ = self.advance();
                const name_id = try self.intern(token.lexeme);
                var expr = self.store.addIdentifier(name_id, loc) catch return ParseError.OutOfMemory;

                // Handle member access and function calls
                while (true) {
                    if (self.match(&[_]TokenType{.period})) {
                        const member_tok = try self.consume(.identifier, "Expected member name");
                        const member_id = try self.intern(member_tok.lexeme);
                        expr = self.store.addMember(expr, member_id, loc) catch return ParseError.OutOfMemory;
                    } else if (self.check(.lparen)) {
                        _ = self.advance();
                        var args: std.ArrayListUnmanaged(ExprIdx) = .{};
                        errdefer args.deinit(self.allocator);

                        if (!self.check(.rparen)) {
                            args.append(self.allocator, try self.parseExpression()) catch return ParseError.OutOfMemory;
                            while (self.match(&[_]TokenType{.comma})) {
                                args.append(self.allocator, try self.parseExpression()) catch return ParseError.OutOfMemory;
                            }
                        }
                        _ = try self.consume(.rparen, "Expected ')'");
                        expr = self.store.addCall(expr, args.items, loc) catch return ParseError.OutOfMemory;
                        args.deinit(self.allocator); // Free ArrayList backing memory after data is copied
                    } else if (self.check(.lbracket)) {
                        _ = self.advance();
                        const index = try self.parseExpression();
                        _ = try self.consume(.rbracket, "Expected ']'");
                        expr = self.store.addIndex(expr, index, loc) catch return ParseError.OutOfMemory;
                    } else {
                        break;
                    }
                }

                return expr;
            },
            .lparen => {
                _ = self.advance();
                const inner = try self.parseExpression();
                _ = try self.consume(.rparen, "Expected ')'");
                return self.store.addGrouping(inner, loc) catch return ParseError.OutOfMemory;
            },
            .percent => {
                // DBL built-in function: %string(), %len(), %trim(), etc.
                _ = self.advance(); // consume '%'
                if (self.check(.identifier)) {
                    const func_tok = self.advance();
                    const func_id = try self.intern(func_tok.lexeme);
                    var func_expr = self.store.addIdentifier(func_id, loc) catch return ParseError.OutOfMemory;

                    // Parse arguments
                    if (self.check(.lparen)) {
                        _ = self.advance();
                        var args: std.ArrayListUnmanaged(ExprIdx) = .{};
                        errdefer args.deinit(self.allocator);

                        if (!self.check(.rparen)) {
                            args.append(self.allocator, try self.parseExpression()) catch return ParseError.OutOfMemory;
                            while (self.match(&[_]TokenType{.comma})) {
                                args.append(self.allocator, try self.parseExpression()) catch return ParseError.OutOfMemory;
                            }
                        }
                        _ = try self.consume(.rparen, "Expected ')'");
                        func_expr = self.store.addCall(func_expr, args.items, loc) catch return ParseError.OutOfMemory;
                        args.deinit(self.allocator); // Free ArrayList backing memory after data is copied
                    }
                    return func_expr;
                }
                self.addError("Expected function name after '%'");
                return ParseError.InvalidExpression;
            },
            .cast_alpha, .cast_decimal, .cast_integer => {
                // DBL cast operators: ^a(), ^d(), ^i()
                // These reinterpret bytes as a different type (NOT conversion)
                const cast_tok = self.advance();
                const cast_name = switch (cast_tok.type) {
                    .cast_alpha => "cast_alpha",
                    .cast_decimal => "cast_decimal",
                    .cast_integer => "cast_integer",
                    else => unreachable,
                };
                const func_id = try self.intern(cast_name);
                var func_expr = self.store.addIdentifier(func_id, loc) catch return ParseError.OutOfMemory;

                // Parse argument (required)
                _ = try self.consume(.lparen, "Expected '(' after cast operator");
                var args: std.ArrayListUnmanaged(ExprIdx) = .{};
                errdefer args.deinit(self.allocator);

                args.append(self.allocator, try self.parseExpression()) catch return ParseError.OutOfMemory;
                while (self.match(&[_]TokenType{.comma})) {
                    args.append(self.allocator, try self.parseExpression()) catch return ParseError.OutOfMemory;
                }
                _ = try self.consume(.rparen, "Expected ')'");
                func_expr = self.store.addCall(func_expr, args.items, loc) catch return ParseError.OutOfMemory;
                args.deinit(self.allocator);
                return func_expr;
            },
            else => {
                self.addError("Expected expression");
                return ParseError.InvalidExpression;
            },
        }
    }

    // ============================================================
    // Helpers
    // ============================================================

    fn peek(self: *Self) Token {
        if (self.current >= self.tokens.len) {
            return Token{ .type = .eof, .lexeme = "", .line = 0, .column = 0 };
        }
        return self.tokens[self.current];
    }

    fn peekNext(self: *Self) Token {
        if (self.current + 1 >= self.tokens.len) {
            return Token{ .type = .eof, .lexeme = "", .line = 0, .column = 0 };
        }
        return self.tokens[self.current + 1];
    }

    fn previous(self: *Self) Token {
        if (self.current == 0) {
            return Token{ .type = .eof, .lexeme = "", .line = 0, .column = 0 };
        }
        return self.tokens[self.current - 1];
    }

    fn advance(self: *Self) Token {
        if (self.current < self.tokens.len) {
            const tok = self.tokens[self.current];
            self.current += 1;

            // Check for invalid tokens (like unterminated strings)
            if (tok.type == .invalid) {
                // Report error at the invalid token's location
                if (self.errors.items.len < MAX_ERRORS) {
                    // Check if it looks like an unterminated string
                    if (tok.lexeme.len > 0 and (tok.lexeme[0] == '"' or tok.lexeme[0] == '\'')) {
                        self.errors.append(self.allocator, .{
                            .message = "Unterminated string literal",
                            .token = tok,
                        }) catch {};
                    } else {
                        self.errors.append(self.allocator, .{
                            .message = "Invalid token",
                            .token = tok,
                        }) catch {};
                    }
                }
            }

            return tok;
        }
        return Token{ .type = .eof, .lexeme = "", .line = 0, .column = 0 };
    }

    fn check(self: *Self, token_type: TokenType) bool {
        return self.peek().type == token_type;
    }

    fn match(self: *Self, types: []const TokenType) bool {
        for (types) |t| {
            if (self.check(t)) {
                _ = self.advance();
                return true;
            }
        }
        return false;
    }

    fn consume(self: *Self, token_type: TokenType, message: []const u8) ParseError!Token {
        if (self.check(token_type)) {
            return self.advance();
        }
        self.addError(message);
        return ParseError.UnexpectedToken;
    }

    fn isAtEnd(self: *Self) bool {
        return self.peek().type == .eof;
    }

    fn synchronize(self: *Self) void {
        _ = self.advance();

        while (!self.isAtEnd()) {
            // Stop at statement boundaries
            switch (self.peek().type) {
                .kw_main, .kw_function, .kw_subroutine, .kw_if, .kw_while, .kw_for,
                .kw_begin, .kw_end, .kw_endmain, .kw_endfunction, .kw_endsubroutine,
                .kw_return, .kw_xreturn, .kw_freturn, .kw_mreturn, .kw_proc,
                .kw_structure, .kw_endstructure, .kw_record, .kw_endrecord,
                .kw_using, .kw_endusing, .kw_onerror, .kw_offerror, .kw_clear,
                .kw_endwhile,
                => return,
                else => _ = self.advance(),
            }
        }
    }
};

test "parser initialization" {
    const allocator = std.testing.allocator;

    var strings = base.StringInterner.init(allocator);
    defer strings.deinit();

    var store = ast.NodeStore.init(allocator, &strings);
    defer store.deinit();

    const tokens = [_]Token{
        .{ .type = .eof, .lexeme = "", .line = 1, .column = 1 },
    };

    var parser = Parser.init(allocator, &tokens, &store, &strings);
    defer parser.deinit();

    const stmts = try parser.parse();
    defer allocator.free(stmts);

    try std.testing.expectEqual(@as(usize, 0), stmts.len);
}
