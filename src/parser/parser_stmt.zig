//! Statement Parsing
//!
//! Extracted from parser.zig to reduce file size.
//! All functions take a pointer to the Parser as first parameter.

const std = @import("std");
const Parser = @import("parser.zig").Parser;
const ParseError = @import("parser.zig").ParseError;
const TokenType = @import("../lexer/token.zig").TokenType;

// AST types
const ast = @import("../ast/mod.zig");
const StmtIdx = ast.StmtIdx;
const ExprIdx = ast.ExprIdx;
const SourceLoc = ast.SourceLoc;
const BinaryOp = ast.BinaryOp;
const StringId = @import("../base/mod.zig").StringId;

// ============================================================================
// Statement Parsing
// ============================================================================

pub fn parseStatement(p: *Parser) ParseError!StmtIdx {
    const token = p.peek();

    return switch (token.type) {
        .kw_if => parseIf(p),
        .kw_switch => parseSwitch(p),
        .kw_for => parseFor(p),
        .kw_while => parseWhile(p),
        .kw_loop => parseLoop(p),
        .kw_break => parseBreak(p),
        .kw_continue => parseContinue(p),
        .kw_return => parseReturn(p),
        .kw_var => p.parseVarDecl(),
        .kw_const => p.parseConstDecl(),
        .kw_view => p.parseViewDecl(),
        .kw_try => parseTry(p),
        .kw_throw => parseThrow(p),
        .kw_defer => parseDefer(p),
        .kw_comptime => parseComptime(p),
        .lbrace => parseBlockStmt(p),
        else => parseExpressionStatement(p),
    };
}

pub fn parseBlockStmt(p: *Parser) ParseError!StmtIdx {
    _ = try p.consume(.lbrace, "Expected '{'");
    return parseBlock(p);
}

pub fn parseBlock(p: *Parser) ParseError!StmtIdx {
    try p.enterNesting();
    errdefer p.exitNesting();

    const loc = p.currentLoc();

    // Collect statements into scratch buffer
    try p.store.markScratch();
    errdefer p.store.rollbackScratch();

    while (!p.check(.rbrace) and !p.isAtEnd()) {
        if (parseStatement(p)) |stmt| {
            p.store.pushScratchStmt(stmt) catch return error.OutOfMemory;
        } else |_| {
            p.synchronize();
        }
    }

    _ = try p.consume(.rbrace, "Expected '}'");

    const stmts = p.store.getScratchStmts();
    const stmts_copy = p.allocator.dupe(StmtIdx, stmts) catch return error.OutOfMemory;
    p.store.commitScratch();

    p.exitNesting();
    return p.store.addBlock(stmts_copy, loc) catch return error.OutOfMemory;
}

pub fn parseIf(p: *Parser) ParseError!StmtIdx {
    try p.enterNesting();
    errdefer p.exitNesting();

    const loc = p.currentLoc();
    _ = try p.consume(.kw_if, "Expected 'if'");
    _ = try p.consume(.lparen, "Expected '(' after 'if'");
    const condition = try p.parseExpression();
    _ = try p.consume(.rparen, "Expected ')' after condition");

    _ = try p.consume(.lbrace, "Expected '{'");
    const then_body = try parseBlock(p);

    var else_body: StmtIdx = .null;
    if (p.match(&[_]TokenType{.kw_else})) {
        if (p.check(.kw_if)) {
            // else if
            else_body = try parseIf(p);
        } else {
            _ = try p.consume(.lbrace, "Expected '{'");
            else_body = try parseBlock(p);
        }
    }

    p.exitNesting();
    return p.store.addIfStmt(condition, then_body, else_body, loc) catch return error.OutOfMemory;
}

/// Parse switch statement (Zig-style): switch (expr) { ... }
pub fn parseSwitch(p: *Parser) ParseError!StmtIdx {
    try p.enterNesting();
    errdefer p.exitNesting();

    const loc = p.currentLoc();
    _ = try p.consume(.kw_switch, "Expected 'switch'");

    // Zig-style: require parentheses around scrutinee
    _ = try p.consume(.lparen, "Expected '(' after 'switch'");
    const scrutinee = try p.parseExpression();
    _ = try p.consume(.rparen, "Expected ')' after switch expression");

    return parseSwitchBody(p, scrutinee, loc);
}

/// Switch body parsing
pub fn parseSwitchBody(p: *Parser, scrutinee: ExprIdx, loc: SourceLoc) ParseError!StmtIdx {
    // Note: enterNesting already called by caller

    _ = try p.consume(.lbrace, "Expected '{'");

    // Parse match arms into scratch buffer
    try p.store.markScratch();
    errdefer p.store.rollbackScratch();

    while (!p.check(.rbrace) and !p.isAtEnd()) {
        // Parse pattern (for now, just expressions or identifiers)
        const pattern = try p.parseExpression();

        _ = try p.consume(.fat_arrow, "Expected '=>'");

        // Parse arm body
        var arm_body: StmtIdx = undefined;
        if (p.check(.lbrace)) {
            _ = p.advance();
            arm_body = try parseBlock(p);
        } else {
            // Single expression as body
            const expr = try p.parseExpression();
            arm_body = p.store.addExprStmt(expr, p.currentLoc()) catch return error.OutOfMemory;
        }

        // Store as [pattern_expr, body_stmt] pairs
        p.store.pushScratchU32(pattern.toInt()) catch return error.OutOfMemory;
        p.store.pushScratchU32(arm_body.toInt()) catch return error.OutOfMemory;

        // Optional comma
        _ = p.match(&[_]TokenType{.comma});
    }

    _ = try p.consume(.rbrace, "Expected '}'");

    const arms = p.store.getScratchU32s();
    p.store.commitScratch();

    // Store match statement
    const arms_start = p.store.extra_data.items.len;
    p.store.extra_data.append(p.allocator, @intCast(arms.len / 2)) catch return error.OutOfMemory; // arm count
    for (arms) |a| {
        p.store.extra_data.append(p.allocator, a) catch return error.OutOfMemory;
    }

    const idx: StmtIdx = @enumFromInt(@as(u32, @intCast(p.store.stmt_tags.items.len)));
    p.store.stmt_tags.append(p.allocator, .match_stmt) catch return error.OutOfMemory;
    p.store.stmt_locs.append(p.allocator, loc) catch return error.OutOfMemory;
    p.store.stmt_data.append(p.allocator, .{
        .a = scrutinee.toInt(),
        .b = @intCast(arms_start),
    }) catch return error.OutOfMemory;

    p.exitNesting();
    return idx;
}

pub fn parseFor(p: *Parser) ParseError!StmtIdx {
    try p.enterNesting();
    errdefer p.exitNesting();

    const loc = p.currentLoc();
    _ = try p.consume(.kw_for, "Expected 'for'");
    const binding_token = try p.consume(.identifier, "Expected loop variable");
    const binding = p.internString(binding_token.lexeme) catch return error.OutOfMemory;
    _ = try p.consume(.kw_in, "Expected 'in'");

    // Disable struct init parsing for iterable to avoid ambiguity with body block
    // e.g., "for x in arr { ... }" should not parse "arr { ... }" as struct init
    const prev_allow_struct_init = p.allow_struct_init;
    p.allow_struct_init = false;
    const iterable = try p.parseExpression();
    p.allow_struct_init = prev_allow_struct_init;

    _ = try p.consume(.lbrace, "Expected '{'");
    const body = try parseBlock(p);

    p.exitNesting();
    return p.store.addForStmt(binding, iterable, body, loc) catch return error.OutOfMemory;
}

pub fn parseWhile(p: *Parser) ParseError!StmtIdx {
    try p.enterNesting();
    errdefer p.exitNesting();

    const loc = p.currentLoc();
    _ = try p.consume(.kw_while, "Expected 'while'");
    _ = try p.consume(.lparen, "Expected '(' after 'while'");
    const condition = try p.parseExpression();
    _ = try p.consume(.rparen, "Expected ')' after condition");

    _ = try p.consume(.lbrace, "Expected '{'");
    const body = try parseBlock(p);

    p.exitNesting();
    return p.store.addWhileStmt(condition, body, loc) catch return error.OutOfMemory;
}

pub fn parseLoop(p: *Parser) ParseError!StmtIdx {
    try p.enterNesting();
    errdefer p.exitNesting();

    const loc = p.currentLoc();
    _ = try p.consume(.kw_loop, "Expected 'loop'");

    _ = try p.consume(.lbrace, "Expected '{'");
    const body = try parseBlock(p);

    p.exitNesting();
    return p.store.addLoopStmt(body, loc) catch return error.OutOfMemory;
}

pub fn parseBreak(p: *Parser) ParseError!StmtIdx {
    const loc = p.currentLoc();
    _ = try p.consume(.kw_break, "Expected 'break'");
    // Optionally consume semicolon
    _ = p.match(&[_]TokenType{.semicolon});
    return p.store.addBreak(loc) catch return error.OutOfMemory;
}

pub fn parseContinue(p: *Parser) ParseError!StmtIdx {
    const loc = p.currentLoc();
    _ = try p.consume(.kw_continue, "Expected 'continue'");
    // Optionally consume semicolon
    _ = p.match(&[_]TokenType{.semicolon});
    return p.store.addContinue(loc) catch return error.OutOfMemory;
}

pub fn parseReturn(p: *Parser) ParseError!StmtIdx {
    const loc = p.currentLoc();
    _ = try p.consume(.kw_return, "Expected 'return'");

    var value: ExprIdx = .null;
    if (!p.check(.rbrace) and !p.check(.semicolon) and !p.isAtEnd()) {
        // Check if there's actually an expression following
        const next = p.peek().type;
        if (next != .kw_fn and next != .kw_struct and next != .kw_const and
            next != .kw_var and next != .kw_if and next != .kw_for and
            next != .kw_while and next != .kw_loop and next != .kw_return)
        {
            value = try p.parseExpression();
        }
    }

    // Optionally consume semicolon
    _ = p.match(&[_]TokenType{.semicolon});

    return p.store.addReturn(value, loc) catch return error.OutOfMemory;
}

pub fn parseTry(p: *Parser) ParseError!StmtIdx {
    try p.enterNesting();
    errdefer p.exitNesting();

    const loc = p.currentLoc();
    _ = try p.consume(.kw_try, "Expected 'try'");

    _ = try p.consume(.lbrace, "Expected '{'");
    const try_body = try parseBlock(p);

    _ = try p.consume(.kw_catch, "Expected 'catch'");

    // Optional error binding: catch (err)
    var err_binding: StringId = .null_id;
    if (p.match(&[_]TokenType{.lparen})) {
        const err_token = try p.consume(.identifier, "Expected error variable");
        err_binding = p.internString(err_token.lexeme) catch return error.OutOfMemory;
        _ = try p.consume(.rparen, "Expected ')'");
    }

    _ = try p.consume(.lbrace, "Expected '{'");
    const catch_body = try parseBlock(p);

    // Note: finally was removed from Cot - use defer instead
    p.exitNesting();
    return p.store.addTryStmt(try_body, err_binding, catch_body, .null, loc) catch return error.OutOfMemory;
}

pub fn parseThrow(p: *Parser) ParseError!StmtIdx {
    const loc = p.currentLoc();
    _ = try p.consume(.kw_throw, "Expected 'throw'");
    const error_expr = try p.parseExpression();

    const idx: StmtIdx = @enumFromInt(@as(u32, @intCast(p.store.stmt_tags.items.len)));
    p.store.stmt_tags.append(p.allocator, .throw_stmt) catch return error.OutOfMemory;
    p.store.stmt_locs.append(p.allocator, loc) catch return error.OutOfMemory;
    p.store.stmt_data.append(p.allocator, .{
        .a = error_expr.toInt(),
        .b = 0,
    }) catch return error.OutOfMemory;

    return idx;
}

/// Parse defer statement: defer expr or defer { block }
/// The deferred code runs at scope exit (in reverse order of declaration)
pub fn parseDefer(p: *Parser) ParseError!StmtIdx {
    const loc = p.currentLoc();
    _ = try p.consume(.kw_defer, "Expected 'defer'");

    // Check if it's a block or expression
    const body = if (p.check(.lbrace)) blk: {
        _ = try p.consume(.lbrace, "Expected '{'");
        break :blk try parseBlock(p);
    } else blk: {
        // Single expression - wrap as expression statement
        const expr = try p.parseExpression();
        break :blk p.store.addExprStmt(expr, loc) catch return error.OutOfMemory;
    };

    return p.store.addDeferStmt(body, loc) catch return error.OutOfMemory;
}

pub fn parseComptime(p: *Parser) ParseError!StmtIdx {
    const loc = p.currentLoc();
    _ = try p.consume(.kw_comptime, "Expected 'comptime'");

    // Check for comptime if or comptime block
    if (p.check(.kw_if)) {
        return parseComptimeIf(p, loc);
    } else {
        return parseComptimeBlock(p, loc);
    }
}

pub fn parseComptimeIf(p: *Parser, loc: SourceLoc) ParseError!StmtIdx {
    try p.enterNesting();
    errdefer p.exitNesting();

    _ = try p.consume(.kw_if, "Expected 'if'");
    _ = try p.consume(.lparen, "Expected '(' after 'if'");
    const condition = try p.parseExpression();
    _ = try p.consume(.rparen, "Expected ')' after condition");

    _ = try p.consume(.lbrace, "Expected '{'");
    const then_body = try parseBlock(p);

    var else_body: StmtIdx = .null;
    if (p.match(&[_]TokenType{.kw_else})) {
        if (p.check(.kw_if)) {
            // comptime else if
            const else_loc = p.currentLoc();
            else_body = try parseComptimeIf(p, else_loc);
        } else {
            _ = try p.consume(.lbrace, "Expected '{'");
            else_body = try parseBlock(p);
        }
    }

    // Store comptime_if
    const extra_start = p.store.extra_data.items.len;
    p.store.extra_data.append(p.allocator, else_body.toInt()) catch return error.OutOfMemory;

    const idx: StmtIdx = @enumFromInt(@as(u32, @intCast(p.store.stmt_tags.items.len)));
    p.store.stmt_tags.append(p.allocator, .comptime_if) catch return error.OutOfMemory;
    p.store.stmt_locs.append(p.allocator, loc) catch return error.OutOfMemory;
    p.store.stmt_data.append(p.allocator, .{
        .a = condition.toInt(),
        .b = (then_body.toInt() << 16) | @as(u32, @intCast(extra_start)),
    }) catch return error.OutOfMemory;

    p.exitNesting();
    return idx;
}

pub fn parseComptimeBlock(p: *Parser, loc: SourceLoc) ParseError!StmtIdx {
    _ = try p.consume(.lbrace, "Expected '{'");
    const body = try parseBlock(p);

    const idx: StmtIdx = @enumFromInt(@as(u32, @intCast(p.store.stmt_tags.items.len)));
    p.store.stmt_tags.append(p.allocator, .comptime_block) catch return error.OutOfMemory;
    p.store.stmt_locs.append(p.allocator, loc) catch return error.OutOfMemory;
    p.store.stmt_data.append(p.allocator, .{
        .a = body.toInt(),
        .b = 0,
    }) catch return error.OutOfMemory;

    return idx;
}

pub fn parseExpressionStatement(p: *Parser) ParseError!StmtIdx {
    const loc = p.currentLoc();
    const expr = try p.parseExpression();

    // Check for assignment
    if (p.match(&[_]TokenType{.equals})) {
        const value = try p.parseExpression();
        // Optionally consume semicolon
        _ = p.match(&[_]TokenType{.semicolon});
        return p.store.addAssignment(expr, value, loc) catch return error.OutOfMemory;
    }

    // Check for compound assignment
    if (p.match(&[_]TokenType{ .plus_equals, .minus_equals, .star_equals, .slash_equals, .pipe_equals, .amp_equals })) {
        const op: BinaryOp = switch (p.previous().type) {
            .plus_equals => .add,
            .minus_equals => .sub,
            .star_equals => .mul,
            .slash_equals => .div,
            .pipe_equals => .bit_or,
            .amp_equals => .bit_and,
            else => unreachable,
        };
        const rhs = try p.parseExpression();
        // Desugar x += y to x = x + y
        const binop = p.store.addBinary(expr, op, rhs, loc) catch return error.OutOfMemory;
        // Optionally consume semicolon
        _ = p.match(&[_]TokenType{.semicolon});
        return p.store.addAssignment(expr, binop, loc) catch return error.OutOfMemory;
    }

    // Create expression statement
    const stmt = p.store.addExprStmt(expr, loc) catch return error.OutOfMemory;

    // Optionally consume semicolon (allows both `expr` and `expr;` syntax)
    _ = p.match(&[_]TokenType{.semicolon});

    return stmt;
}
