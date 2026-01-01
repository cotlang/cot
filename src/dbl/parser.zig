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
            .kw_endclass, .kw_endnamespace, .kw_endwhile, .dir_else, .dir_end, .kw_else,
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
        // Parameters are stored as [name_id, type_idx] pairs
        var params: std.ArrayListUnmanaged(u32) = .{};
        errdefer params.deinit(self.allocator);
        var local_record_decls: std.ArrayListUnmanaged(StmtIdx) = .{};
        errdefer local_record_decls.deinit(self.allocator);

        while (!self.check(.kw_proc) and !self.check(.kw_endsubroutine) and !self.isAtEnd()) {
            if (self.check(.identifier)) {
                const param_name_tok = self.advance();
                const param_name_id = try self.intern(param_name_tok.lexeme);

                // Expect comma before type
                _ = self.match(&[_]TokenType{.comma});

                // Parse type specifier
                var param_type = TypeIdx.null;
                if (self.check(.identifier)) {
                    const type_tok = self.advance();
                    const type_result = self.parseDblTypeSpecWithInfo(type_tok.lexeme);
                    param_type = type_result.type_idx;
                } else {
                    param_type = self.store.addPrimitiveType(.i64) catch return ParseError.OutOfMemory;
                }

                // Add parameter as [name_id, type_idx] pair
                params.append(self.allocator, @intFromEnum(param_name_id)) catch return ParseError.OutOfMemory;
                params.append(self.allocator, param_type.toInt()) catch return ParseError.OutOfMemory;
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
        const return_type = self.store.addPrimitiveType(.void) catch return ParseError.OutOfMemory;

        return self.store.addFnDef(name_id, params.items, return_type, block_idx, loc) catch return ParseError.OutOfMemory;
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

            break :blk self.store.addBlock(stmts.items, loc) catch return ParseError.OutOfMemory;
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

        return self.store.addBlock(stmts.items, loc) catch return ParseError.OutOfMemory;
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

    /// Parse record block (local variable declarations)
    /// record
    ///     var ,type
    ///     ...
    /// endrecord
    ///
    /// DBL record blocks declare local variables with their proper types.
    fn parseRecord(self: *Self) ParseError!StmtIdx {
        const loc = self.currentLoc();
        _ = self.advance(); // consume 'record'

        var stmts: std.ArrayListUnmanaged(StmtIdx) = .{};
        errdefer stmts.deinit(self.allocator);

        // Parse variable declarations until endrecord
        while (!self.check(.kw_endrecord) and !self.isAtEnd()) {
            if (self.check(.identifier)) {
                const var_name_tok = self.advance();
                const var_name_id = try self.intern(var_name_tok.lexeme);

                // Expect comma before type
                _ = self.match(&[_]TokenType{.comma});

                // Parse type specifier - could be primitive (d6, a30) or struct name
                var var_type = TypeIdx.null;
                var is_struct_type = false;
                if (self.check(.identifier)) {
                    const type_tok = self.advance();
                    const type_result = self.parseDblTypeSpecWithInfo(type_tok.lexeme);
                    var_type = type_result.type_idx;
                    is_struct_type = type_result.is_struct;
                } else {
                    var_type = self.store.addPrimitiveType(.i64) catch return ParseError.OutOfMemory;
                }

                // Create variable declaration with proper type
                // For structs, don't provide init value (let IR handle default construction)
                // For primitives, initialize to 0
                const init_val = if (is_struct_type) ExprIdx.null else self.store.addIntLiteral(0, loc) catch return ParseError.OutOfMemory;
                const var_stmt = self.store.addLetDecl(var_name_id, var_type, init_val, true, loc) catch return ParseError.OutOfMemory;
                stmts.append(self.allocator, var_stmt) catch return ParseError.OutOfMemory;
            } else {
                _ = self.advance();
            }
        }

        _ = try self.consume(.kw_endrecord, "Expected 'endrecord'");

        // Return as a block containing all the variable declarations
        return self.store.addBlock(stmts.items, loc) catch return ParseError.OutOfMemory;
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
        // NOT struct names that happen to start with a/d/i (e.g., app_state_t, data_t)
        if (lower[0] == 'd' or lower[0] == 'a' or lower[0] == 'i') {
            // Single letter (d, a, i) is a valid DBL type
            // Or letter followed by all digits (d6, a30, i4)
            var is_primitive = (len == 1);
            if (!is_primitive and len >= 2) {
                var all_digits = true;
                for (lower[1..len]) |c| {
                    if (!std.ascii.isDigit(c)) {
                        all_digits = false;
                        break;
                    }
                }
                is_primitive = all_digits;
            }
            if (is_primitive) {
                // It's a DBL primitive type specifier - parse the size
                const size: u32 = if (len > 1)
                    std.fmt.parseInt(u32, lower[1..len], 10) catch 1
                else
                    1; // Default size is 1

                // For DBL, both alpha (a) and decimal (d) types are stored as fixed-length
                // byte strings in records. The difference is interpretation, not storage.
                // d6 = 6 bytes, a30 = 30 bytes, a1 = 1 byte
                return .{
                    .type_idx = self.store.addStringFixedType(size) catch TypeIdx.null,
                    .is_struct = false,
                };
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
        return self.parseOr();
    }

    fn parseOr(self: *Self) ParseError!ExprIdx {
        var expr = try self.parseAnd();

        while (self.match(&[_]TokenType{.op_or})) {
            const loc = self.currentLoc();
            const right = try self.parseAnd();
            expr = self.store.addBinary(expr, .@"or", right, loc) catch return ParseError.OutOfMemory;
        }

        return expr;
    }

    fn parseAnd(self: *Self) ParseError!ExprIdx {
        var expr = try self.parseEquality();

        while (self.match(&[_]TokenType{.op_and})) {
            const loc = self.currentLoc();
            const right = try self.parseEquality();
            expr = self.store.addBinary(expr, .@"and", right, loc) catch return ParseError.OutOfMemory;
        }

        return expr;
    }

    fn parseEquality(self: *Self) ParseError!ExprIdx {
        var expr = try self.parseComparison();

        while (self.match(&[_]TokenType{ .op_eq, .op_ne })) {
            const loc = self.currentLoc();
            const op_tok = self.previous();
            const right = try self.parseComparison();
            const op: BinaryOp = if (op_tok.type == .op_eq) .eq else .ne;
            expr = self.store.addBinary(expr, op, right, loc) catch return ParseError.OutOfMemory;
        }

        return expr;
    }

    fn parseComparison(self: *Self) ParseError!ExprIdx {
        var expr = try self.parseTerm();

        while (self.match(&[_]TokenType{ .op_lt, .op_le, .op_gt, .op_ge })) {
            const loc = self.currentLoc();
            const op_tok = self.previous();
            const right = try self.parseTerm();
            const op: BinaryOp = switch (op_tok.type) {
                .op_lt => .lt,
                .op_le => .le,
                .op_gt => .gt,
                .op_ge => .ge,
                else => .lt,
            };
            expr = self.store.addBinary(expr, op, right, loc) catch return ParseError.OutOfMemory;
        }

        return expr;
    }

    fn parseTerm(self: *Self) ParseError!ExprIdx {
        var expr = try self.parseFactor();

        // Note: hash (#) is DBL string concatenation - mapped to add, IR handles type dispatch
        while (self.match(&[_]TokenType{ .plus, .minus, .hash })) {
            const loc = self.currentLoc();
            const op_tok = self.previous();
            const right = try self.parseFactor();
            const op: BinaryOp = switch (op_tok.type) {
                .plus, .hash => .add,
                .minus => .sub,
                else => .add,
            };
            expr = self.store.addBinary(expr, op, right, loc) catch return ParseError.OutOfMemory;
        }

        return expr;
    }

    fn parseFactor(self: *Self) ParseError!ExprIdx {
        var expr = try self.parseUnary();

        while (self.match(&[_]TokenType{ .star, .slash })) {
            const loc = self.currentLoc();
            const op_tok = self.previous();
            const right = try self.parseUnary();
            const op: BinaryOp = if (op_tok.type == .star) .mul else .div;
            expr = self.store.addBinary(expr, op, right, loc) catch return ParseError.OutOfMemory;
        }

        return expr;
    }

    fn parseUnary(self: *Self) ParseError!ExprIdx {
        if (self.match(&[_]TokenType{ .minus, .op_not, .kw_not })) {
            const loc = self.currentLoc();
            const op_tok = self.previous();
            const operand = try self.parseUnary();
            const op: UnaryOp = if (op_tok.type == .minus) .neg else .not;
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
                // Remove quotes
                const content = if (token.lexeme.len >= 2)
                    token.lexeme[1 .. token.lexeme.len - 1]
                else
                    token.lexeme;
                const str_id = try self.intern(content);
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
                    }
                    return func_expr;
                }
                self.addError("Expected function name after '%'");
                return ParseError.InvalidExpression;
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
