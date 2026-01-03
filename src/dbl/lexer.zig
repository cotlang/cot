//! DBL Lexer
//!
//! Tokenizes DBL source code into a stream of tokens.
//! Produces DBL-specific Token types that are converted to AST
//! for compilation through the standard cot pipeline.
//!
//! DBL syntax features:
//! - Case-insensitive keywords (PROC, proc, Proc all work)
//! - Dot operators (.EQ., .AND., .OR., etc.)
//! - Compile-time directives (.define, .if, .else, etc.)
//! - Semicolon comments
//! - DBL-style type specifiers (a50, d10, i4, etc.)

const std = @import("std");
const Token = @import("token.zig").Token;
const TokenType = @import("token.zig").TokenType;

pub const Lexer = struct {
    source: []const u8,
    position: usize,
    line: usize,
    column: usize,

    const Self = @This();

    /// Initialize lexer with DBL source code
    pub fn init(source: []const u8) Self {
        return .{
            .source = source,
            .position = 0,
            .line = 1,
            .column = 1,
        };
    }

    /// Tokenize entire source into token array
    pub fn tokenize(self: *Self, allocator: std.mem.Allocator) ![]Token {
        var tokens: std.ArrayListAligned(Token, null) = .empty;
        errdefer tokens.deinit(allocator);

        while (self.position < self.source.len) {
            const token = try self.nextToken();
            try tokens.append(allocator, token);

            if (token.type == .eof) break;
        }

        // Ensure EOF token
        if (tokens.items.len == 0 or tokens.items[tokens.items.len - 1].type != .eof) {
            try tokens.append(allocator, .{
                .type = .eof,
                .lexeme = "",
                .line = self.line,
                .column = self.column,
            });
        }

        return tokens.toOwnedSlice(allocator);
    }

    /// Get next token from source
    pub fn nextToken(self: *Self) !Token {
        self.skipWhitespaceAndComments();

        if (self.isAtEnd()) {
            return self.makeToken(.eof, "");
        }

        const start_pos = self.position;
        const start_col = self.column;
        const c = self.advance();

        // Simple single character tokens (no multi-char variants)
        const simple_single: ?TokenType = switch (c) {
            '(' => .lparen,
            ')' => .rparen,
            '[' => .lbracket,
            ']' => .rbracket,
            '{' => .lbrace,
            '}' => .rbrace,
            ',' => .comma,
            '@' => .at,
            '~' => .op_bnot,
            else => null,
        };

        if (simple_single) |tt| {
            return self.makeToken(tt, self.source[start_pos..self.position]);
        }

        // Plus: + or +=
        if (c == '+') {
            if (!self.isAtEnd() and self.peek() == '=') {
                _ = self.advance();
                return self.makeToken(.plus_equals, self.source[start_pos..self.position]);
            }
            return self.makeToken(.plus, self.source[start_pos..self.position]);
        }

        // Star: * or *=
        if (c == '*') {
            if (!self.isAtEnd() and self.peek() == '=') {
                _ = self.advance();
                return self.makeToken(.star_equals, self.source[start_pos..self.position]);
            }
            return self.makeToken(.star, self.source[start_pos..self.position]);
        }

        // Hash: # or ## (rounding operators)
        if (c == '#') {
            if (!self.isAtEnd() and self.peek() == '#') {
                _ = self.advance();
                return self.makeToken(.op_round_true, self.source[start_pos..self.position]);
            }
            return self.makeToken(.op_round, self.source[start_pos..self.position]);
        }

        // Percent: % (DBL built-in function prefix) or % (modulo operator)
        if (c == '%') {
            // If followed by an identifier, it's a built-in function call like %string()
            if (!self.isAtEnd() and std.ascii.isAlphabetic(self.peek())) {
                return self.makeToken(.percent, self.source[start_pos..self.position]);
            }
            // Otherwise it's the modulo operator
            return self.makeToken(.op_mod, self.source[start_pos..self.position]);
        }

        // Question mark: ? or ?? or ?.
        if (c == '?') {
            if (!self.isAtEnd()) {
                if (self.peek() == '?') {
                    _ = self.advance();
                    return self.makeToken(.op_null_coalesce, self.source[start_pos..self.position]);
                } else if (self.peek() == '.') {
                    _ = self.advance();
                    return self.makeToken(.op_null_cond, self.source[start_pos..self.position]);
                }
            }
            return self.makeToken(.question, self.source[start_pos..self.position]);
        }

        // Colon: : or := (walrus) or :: (double colon)
        if (c == ':') {
            if (!self.isAtEnd()) {
                if (self.peek() == '=') {
                    _ = self.advance();
                    return self.makeToken(.walrus, self.source[start_pos..self.position]);
                } else if (self.peek() == ':') {
                    _ = self.advance();
                    return self.makeToken(.double_colon, self.source[start_pos..self.position]);
                }
            }
            return self.makeToken(.colon, self.source[start_pos..self.position]);
        }

        // Caret: ^ (bitwise XOR) or ^a (cast to alpha) or ^d (cast to decimal) or ^i (cast to integer) or ^f (cast to float)
        //        or ^null (null check) or ^size (size check)
        if (c == '^') {
            if (!self.isAtEnd()) {
                const next = self.peek();
                if (next == 'a' or next == 'A') {
                    _ = self.advance();
                    return self.makeToken(.cast_alpha, self.source[start_pos..self.position]);
                } else if (next == 'd' or next == 'D') {
                    _ = self.advance();
                    return self.makeToken(.cast_decimal, self.source[start_pos..self.position]);
                } else if (next == 'i' or next == 'I') {
                    _ = self.advance();
                    return self.makeToken(.cast_integer, self.source[start_pos..self.position]);
                } else if (next == 'f' or next == 'F') {
                    _ = self.advance();
                    return self.makeToken(.cast_float, self.source[start_pos..self.position]);
                } else if (next == 'n' or next == 'N') {
                    // Check for ^null
                    const remaining = self.source[self.position..];
                    if (remaining.len >= 4 and (std.ascii.eqlIgnoreCase(remaining[0..4], "null"))) {
                        // Check it's not part of a longer identifier
                        if (remaining.len == 4 or !std.ascii.isAlphanumeric(remaining[4])) {
                            self.position += 4;
                            self.column += 4;
                            return self.makeToken(.builtin_null, self.source[start_pos..self.position]);
                        }
                    }
                } else if (next == 's' or next == 'S') {
                    // Check for ^size
                    const remaining = self.source[self.position..];
                    if (remaining.len >= 4 and (std.ascii.eqlIgnoreCase(remaining[0..4], "size"))) {
                        // Check it's not part of a longer identifier
                        if (remaining.len == 4 or !std.ascii.isAlphanumeric(remaining[4])) {
                            self.position += 4;
                            self.column += 4;
                            return self.makeToken(.builtin_size, self.source[start_pos..self.position]);
                        }
                    }
                }
            }
            // ^ alone is bitwise XOR
            return self.makeToken(.op_bxor, self.source[start_pos..self.position]);
        }

        // Minus: - or -> (arrow) or -=
        if (c == '-') {
            if (!self.isAtEnd()) {
                if (self.peek() == '>') {
                    _ = self.advance();
                    return self.makeToken(.arrow, self.source[start_pos..self.position]);
                } else if (self.peek() == '=') {
                    _ = self.advance();
                    return self.makeToken(.minus_equals, self.source[start_pos..self.position]);
                }
            }
            return self.makeToken(.minus, self.source[start_pos..self.position]);
        }

        // Slash: / or // (line comment) or /* (block comment) or /=
        if (c == '/') {
            if (!self.isAtEnd()) {
                if (self.peek() == '/') {
                    // Line comment - skip to end of line
                    while (!self.isAtEnd() and self.peek() != '\n') {
                        _ = self.advance();
                    }
                    return self.nextToken();
                } else if (self.peek() == '*') {
                    _ = self.advance();
                    self.skipBlockComment();
                    return self.nextToken();
                } else if (self.peek() == '=') {
                    _ = self.advance();
                    return self.makeToken(.slash_equals, self.source[start_pos..self.position]);
                }
            }
            return self.makeToken(.slash, self.source[start_pos..self.position]);
        }

        // Assignment (=) vs equality (==) vs fat arrow (=>)
        if (c == '=') {
            if (!self.isAtEnd()) {
                if (self.peek() == '=') {
                    _ = self.advance();
                    return self.makeToken(.op_eq, self.source[start_pos..self.position]);
                } else if (self.peek() == '>') {
                    _ = self.advance();
                    return self.makeToken(.fat_arrow, self.source[start_pos..self.position]);
                }
            }
            return self.makeToken(.equals, self.source[start_pos..self.position]);
        }

        // Not equal (!=)
        if (c == '!') {
            if (!self.isAtEnd() and self.peek() == '=') {
                _ = self.advance();
                return self.makeToken(.op_ne, self.source[start_pos..self.position]);
            }
            return self.makeToken(.op_not, self.source[start_pos..self.position]);
        }

        // Comparison and shift operators: <, >, <=, >=, <>, <<, >>
        if (c == '<') {
            if (!self.isAtEnd()) {
                const next = self.peek();
                if (next == '=') {
                    _ = self.advance();
                    return self.makeToken(.op_le, self.source[start_pos..self.position]);
                } else if (next == '>') {
                    _ = self.advance();
                    return self.makeToken(.op_ne, self.source[start_pos..self.position]);
                } else if (next == '<') {
                    _ = self.advance();
                    return self.makeToken(.op_shl, self.source[start_pos..self.position]);
                }
            }
            return self.makeToken(.op_lt, self.source[start_pos..self.position]);
        }

        if (c == '>') {
            if (!self.isAtEnd()) {
                const next = self.peek();
                if (next == '=') {
                    _ = self.advance();
                    return self.makeToken(.op_ge, self.source[start_pos..self.position]);
                } else if (next == '>') {
                    _ = self.advance();
                    return self.makeToken(.op_shr, self.source[start_pos..self.position]);
                }
            }
            return self.makeToken(.op_gt, self.source[start_pos..self.position]);
        }

        // String literals
        if (c == '"' or c == '\'') {
            return self.scanString(c, start_pos, start_col, self.line);
        }

        // Numbers
        if (std.ascii.isDigit(c)) {
            return self.scanNumber(start_pos, start_col);
        }

        // Identifiers and keywords
        if (std.ascii.isAlphabetic(c) or c == '_' or c == '$') {
            return self.scanIdentifier(start_pos, start_col);
        }

        // Dot: . or .. (range) or ..= (range inclusive) or dot operators (.EQ., .AND., etc.)
        if (c == '.') {
            // Check for range operators first
            if (!self.isAtEnd() and self.peek() == '.') {
                _ = self.advance();
                if (!self.isAtEnd() and self.peek() == '=') {
                    _ = self.advance();
                    return self.makeToken(.range_inclusive, self.source[start_pos..self.position]);
                }
                return self.makeToken(.range, self.source[start_pos..self.position]);
            }
            return self.scanDotOperator(start_pos, start_col);
        }

        // Ampersand: && (logical AND) or &= or & (bitwise AND / string concat)
        // Note: & also serves as string concatenation in DBL, so we always return it as a token
        // Line continuation is handled by skipping newlines after operators
        if (c == '&') {
            if (!self.isAtEnd()) {
                if (self.peek() == '&') {
                    _ = self.advance();
                    return self.makeToken(.op_and, self.source[start_pos..self.position]);
                } else if (self.peek() == '=') {
                    _ = self.advance();
                    return self.makeToken(.band_equals, self.source[start_pos..self.position]);
                }
            }
            return self.makeToken(.op_band, self.source[start_pos..self.position]);
        }

        // Pipe: | (bitwise OR) or || (logical OR) or |=
        if (c == '|') {
            if (!self.isAtEnd()) {
                if (self.peek() == '|') {
                    _ = self.advance();
                    return self.makeToken(.op_or, self.source[start_pos..self.position]);
                } else if (self.peek() == '=') {
                    _ = self.advance();
                    return self.makeToken(.bor_equals, self.source[start_pos..self.position]);
                }
            }
            return self.makeToken(.op_bor, self.source[start_pos..self.position]);
        }

        // Unknown character
        return self.makeToken(.invalid, self.source[start_pos..self.position]);
    }

    fn scanString(self: *Self, quote: u8, start_pos: usize, start_col: usize, start_line: usize) Token {
        while (!self.isAtEnd() and self.peek() != quote) {
            if (self.peek() == '\n') {
                self.line += 1;
                self.column = 0;
            }
            _ = self.advance();
        }

        if (self.isAtEnd()) {
            // Unterminated string - report error at the START of the string, not where we ended up
            return .{
                .type = .invalid,
                .lexeme = self.source[start_pos..self.position],
                .line = start_line,
                .column = start_col,
            };
        }

        // Consume closing quote
        _ = self.advance();

        return .{
            .type = .string_literal,
            .lexeme = self.source[start_pos..self.position],
            .line = start_line,
            .column = start_col,
        };
    }

    fn scanNumber(self: *Self, start_pos: usize, start_col: usize) Token {
        while (!self.isAtEnd() and std.ascii.isDigit(self.peek())) {
            _ = self.advance();
        }

        // Check for DBL array type specifier pattern: <digits><a|d|i><digits>
        // e.g., 10d4, 5a20, 100i4
        if (!self.isAtEnd()) {
            const next_char = std.ascii.toLower(self.peek());
            if (next_char == 'a' or next_char == 'd' or next_char == 'i') {
                // Peek ahead to see if there are more digits after the type char
                const type_char_pos = self.position;
                _ = self.advance(); // consume type char
                if (!self.isAtEnd() and std.ascii.isDigit(self.peek())) {
                    // It's a DBL type specifier - consume remaining digits
                    while (!self.isAtEnd() and std.ascii.isDigit(self.peek())) {
                        _ = self.advance();
                    }
                    return .{
                        .type = .identifier,
                        .lexeme = self.source[start_pos..self.position],
                        .line = self.line,
                        .column = start_col,
                    };
                } else {
                    // Not a type specifier, backtrack
                    self.position = type_char_pos;
                }
            }
        }

        // Check for decimal point
        if (!self.isAtEnd() and self.peek() == '.' and
            self.position + 1 < self.source.len and
            std.ascii.isDigit(self.source[self.position + 1]))
        {
            _ = self.advance(); // consume '.'
            while (!self.isAtEnd() and std.ascii.isDigit(self.peek())) {
                _ = self.advance();
            }
            return .{
                .type = .decimal_literal,
                .lexeme = self.source[start_pos..self.position],
                .line = self.line,
                .column = start_col,
            };
        }

        return .{
            .type = .integer_literal,
            .lexeme = self.source[start_pos..self.position],
            .line = self.line,
            .column = start_col,
        };
    }

    fn scanIdentifier(self: *Self, start_pos: usize, start_col: usize) Token {
        while (!self.isAtEnd() and (std.ascii.isAlphanumeric(self.peek()) or
            self.peek() == '_' or self.peek() == '$'))
        {
            _ = self.advance();
        }

        const lexeme = self.source[start_pos..self.position];
        var lower_buf: [32]u8 = undefined;
        const lower_len = @min(lexeme.len, 32);
        const lower_lexeme = std.ascii.lowerString(lower_buf[0..lower_len], lexeme[0..lower_len]);

        const token_type = keywords.get(lower_lexeme) orelse TokenType.identifier;

        return .{
            .type = token_type,
            .lexeme = lexeme,
            .line = self.line,
            .column = start_col,
        };
    }

    fn scanDotOperator(self: *Self, start_pos: usize, start_col: usize) Token {
        // Check for .EQ., .NE., .LT., .LE., .GT., .GE., .AND., .OR., .NOT., .XOR.
        // Also check for compile-time directives: .define, .if, .else, .end, .endc, .ifdef, .ifndef
        if (!self.isAtEnd() and std.ascii.isAlphabetic(self.peek())) {
            const op_start = self.position;
            while (!self.isAtEnd() and std.ascii.isAlphabetic(self.peek())) {
                _ = self.advance();
            }

            const op = self.source[op_start..self.position];
            var lower_buf: [16]u8 = undefined;
            const lower_op = std.ascii.lowerString(lower_buf[0..@min(op.len, 16)], op[0..@min(op.len, 16)]);

            // Check for compile-time directives first (no closing dot)
            const dir_type: ?TokenType = if (std.mem.eql(u8, lower_op, "define"))
                .dir_define
            else if (std.mem.eql(u8, lower_op, "if"))
                .dir_if
            else if (std.mem.eql(u8, lower_op, "else"))
                .dir_else
            else if (std.mem.eql(u8, lower_op, "end"))
                .dir_end
            else if (std.mem.eql(u8, lower_op, "endc"))
                .dir_end // .endc is alias for .end
            else if (std.mem.eql(u8, lower_op, "ifdef"))
                .dir_ifdef
            else if (std.mem.eql(u8, lower_op, "ifndef"))
                .dir_ifndef
            else
                null;

            if (dir_type) |tt| {
                return .{
                    .type = tt,
                    .lexeme = self.source[start_pos..self.position],
                    .line = self.line,
                    .column = start_col,
                };
            }

            // Check for dot operators with closing dot (.EQ., .AND., etc.)
            if (!self.isAtEnd() and self.peek() == '.') {
                _ = self.advance();

                const op_type: ?TokenType =
                    // Comparison operators
                    if (std.mem.eql(u8, lower_op, "eq"))
                    .op_eq
                else if (std.mem.eql(u8, lower_op, "ne"))
                    .op_ne
                else if (std.mem.eql(u8, lower_op, "lt"))
                    .op_lt
                else if (std.mem.eql(u8, lower_op, "le"))
                    .op_le
                else if (std.mem.eql(u8, lower_op, "gt"))
                    .op_gt
                else if (std.mem.eql(u8, lower_op, "ge"))
                    .op_ge
                    // Logical operators
                else if (std.mem.eql(u8, lower_op, "and"))
                    .op_and
                else if (std.mem.eql(u8, lower_op, "or"))
                    .op_or
                else if (std.mem.eql(u8, lower_op, "not"))
                    .op_not
                else if (std.mem.eql(u8, lower_op, "xor"))
                    .op_xor
                    // Bitwise operators
                else if (std.mem.eql(u8, lower_op, "band"))
                    .op_band
                else if (std.mem.eql(u8, lower_op, "bor"))
                    .op_bor
                else if (std.mem.eql(u8, lower_op, "bnot"))
                    .op_bnot
                else if (std.mem.eql(u8, lower_op, "bxor"))
                    .op_bxor
                else if (std.mem.eql(u8, lower_op, "bnand"))
                    .op_bnand
                    // String comparison operators (full-length)
                else if (std.mem.eql(u8, lower_op, "eqs"))
                    .op_eqs
                else if (std.mem.eql(u8, lower_op, "nes"))
                    .op_nes
                else if (std.mem.eql(u8, lower_op, "gts"))
                    .op_gts
                else if (std.mem.eql(u8, lower_op, "lts"))
                    .op_lts
                else if (std.mem.eql(u8, lower_op, "ges"))
                    .op_ges
                else if (std.mem.eql(u8, lower_op, "les"))
                    .op_les
                    // Modulo operator
                else if (std.mem.eql(u8, lower_op, "mod"))
                    .op_mod
                else
                    null;

                if (op_type) |tt| {
                    return .{
                        .type = tt,
                        .lexeme = self.source[start_pos..self.position],
                        .line = self.line,
                        .column = start_col,
                    };
                }
            }

            // Not a valid dot operator/directive, reset
            self.position = start_pos + 1;
            self.column = start_col + 1;
        }

        // Just a period
        return .{
            .type = .period,
            .lexeme = ".",
            .line = self.line,
            .column = start_col,
        };
    }

    fn skipWhitespaceAndComments(self: *Self) void {
        while (!self.isAtEnd()) {
            const c = self.peek();
            switch (c) {
                ' ', '\t', '\r' => {
                    _ = self.advance();
                },
                '\n' => {
                    _ = self.advance();
                    self.line += 1;
                    self.column = 1;
                    // Note: Legacy DBL line continuation with & at start of line
                    // is handled naturally - since newlines are whitespace and skipped,
                    // the & at the start of the next line is just tokenized normally
                    // as op_band (which also serves as string concatenation operator).
                },
                ';' => {
                    // Semicolon comment - skip to end of line
                    while (!self.isAtEnd() and self.peek() != '\n') {
                        _ = self.advance();
                    }
                },
                else => break,
            }
        }
    }

    fn skipBlockComment(self: *Self) void {
        var depth: usize = 1;
        while (!self.isAtEnd() and depth > 0) {
            if (self.peek() == '/' and self.position + 1 < self.source.len and self.source[self.position + 1] == '*') {
                _ = self.advance();
                _ = self.advance();
                depth += 1;
            } else if (self.peek() == '*' and self.position + 1 < self.source.len and self.source[self.position + 1] == '/') {
                _ = self.advance();
                _ = self.advance();
                depth -= 1;
            } else {
                if (self.peek() == '\n') {
                    self.line += 1;
                    self.column = 0;
                }
                _ = self.advance();
            }
        }
    }

    fn isAtEnd(self: *const Self) bool {
        return self.position >= self.source.len;
    }

    fn peek(self: *const Self) u8 {
        if (self.isAtEnd()) return 0;
        return self.source[self.position];
    }

    fn advance(self: *Self) u8 {
        const c = self.source[self.position];
        self.position += 1;
        self.column += 1;
        return c;
    }

    fn makeToken(self: *const Self, token_type: TokenType, lexeme: []const u8) Token {
        return .{
            .type = token_type,
            .lexeme = lexeme,
            .line = self.line,
            .column = self.column - lexeme.len,
        };
    }

    // DBL keyword lookup map (case-insensitive)
    const keywords = std.StaticStringMap(TokenType).initComptime(.{
        // Program structure
        .{ "record", .kw_record },
        .{ "endrecord", .kw_endrecord },
        .{ "proc", .kw_proc },
        .{ "end", .kw_end },
        .{ "main", .kw_main },
        .{ "endmain", .kw_endmain },
        .{ "subroutine", .kw_subroutine },
        .{ "endsubroutine", .kw_endsubroutine },
        .{ "function", .kw_function },
        .{ "endfunction", .kw_endfunction },
        .{ "group", .kw_group },
        .{ "endgroup", .kw_endgroup },
        .{ "structure", .kw_structure },
        .{ "endstructure", .kw_endstructure },
        .{ "common", .kw_common },
        .{ "endcommon", .kw_endcommon },
        .{ "global", .kw_common }, // alias
        .{ "endglobal", .kw_endcommon }, // alias
        .{ "literal", .kw_literal },
        .{ "endliteral", .kw_endliteral },

        // Control flow
        .{ "if", .kw_if },
        .{ "then", .kw_then },
        .{ "else", .kw_else },
        .{ "begin", .kw_begin },
        .{ "case", .kw_case },
        .{ "endcase", .kw_endcase },
        .{ "using", .kw_using },
        .{ "endusing", .kw_endusing },
        .{ "select", .kw_select },

        // Loops
        .{ "do", .kw_do },
        .{ "until", .kw_until },
        .{ "while", .kw_while },
        .{ "endwhile", .kw_endwhile },
        .{ "for", .kw_for },
        .{ "from", .kw_from },
        .{ "thru", .kw_thru },
        .{ "foreach", .kw_foreach },
        .{ "in", .kw_in },
        .{ "forever", .kw_forever },
        .{ "repeat", .kw_repeat },
        .{ "exitloop", .kw_exitloop },
        .{ "nextloop", .kw_nextloop },
        .{ "loop", .kw_loop },
        .{ "break", .kw_break },
        .{ "continue", .kw_continue },
        .{ "step", .kw_step },

        // Parameter direction modifiers
        .{ "out", .kw_out },
        .{ "inout", .kw_inout },

        // Parameter optionality modifiers
        .{ "req", .kw_req },
        .{ "opt", .kw_opt },

        // Subroutine/function
        .{ "xcall", .kw_xcall },
        .{ "xreturn", .kw_xreturn },
        .{ "call", .kw_call },
        .{ "return", .kw_return },
        .{ "freturn", .kw_freturn },
        .{ "mreturn", .kw_mreturn },
        .{ "goto", .kw_goto },
        .{ "stop", .kw_stop },
        .{ "exit", .kw_exit },

        // I/O keywords
        .{ "find", .kw_find },
        .{ "unlock", .kw_unlock },
        .{ "flush", .kw_flush },
        .{ "accept", .kw_accept },

        // Data manipulation
        .{ "clear", .kw_clear },
        .{ "init", .kw_init },
        .{ "incr", .kw_incr },
        .{ "decr", .kw_decr },
        .{ "locase", .kw_locase },
        .{ "upcase", .kw_upcase },
        .{ "data", .kw_data },

        // OOP
        .{ "class", .kw_class },
        .{ "endclass", .kw_endclass },
        .{ "method", .kw_method },
        .{ "endmethod", .kw_endmethod },
        .{ "property", .kw_property },
        .{ "endproperty", .kw_endproperty },
        .{ "namespace", .kw_namespace },
        .{ "endnamespace", .kw_endnamespace },
        .{ "import", .kw_import },
        .{ "extends", .kw_extends },
        .{ "implements", .kw_implements },
        .{ "interface", .kw_interface },
        .{ "endinterface", .kw_endinterface },
        .{ "public", .kw_public },
        .{ "private", .kw_private },
        .{ "protected", .kw_protected },
        .{ "static", .kw_static },
        .{ "virtual", .kw_virtual },
        .{ "override", .kw_override },
        .{ "new", .kw_new },
        .{ "parent", .kw_parent },
        .{ "this", .kw_this },
        .{ "abstract", .kw_abstract },
        .{ "sealed", .kw_sealed },
        .{ "partial", .kw_partial },

        // Exception handling
        .{ "try", .kw_try },
        .{ "catch", .kw_catch },
        .{ "finally", .kw_finally },
        .{ "endtry", .kw_endtry },
        .{ "throw", .kw_throw },
        .{ "onerror", .kw_onerror },
        .{ "offerror", .kw_offerror },

        // Compile-time
        .{ "comptime", .kw_comptime },
        .{ "const", .kw_const },
        .{ "inline", .kw_inline },
        .{ "not", .kw_not },

        // Booleans
        .{ "true", .kw_true },
        .{ "false", .kw_false },

        // Enumerations
        .{ "enum", .kw_enum },
        .{ "endenum", .kw_endenum },

        // Modern keywords (also supported in DBL mode for compatibility)
        .{ "fn", .kw_fn },
        .{ "struct", .kw_struct },
        .{ "let", .kw_let },
        .{ "mut", .kw_mut },
        .{ "self", .kw_self },
        .{ "match", .kw_match },
        .{ "impl", .kw_impl },
        .{ "type", .kw_type },

        // Testing
        .{ "test", .kw_test },
        .{ "endtest", .kw_endtest },
    });
};

// ============================================================================
// Tests
// ============================================================================

test "dbl lexer basic tokens" {
    var lexer = Lexer.init("PROC myproc");
    const tokens = try lexer.tokenize(std.testing.allocator);
    defer std.testing.allocator.free(tokens);

    try std.testing.expectEqual(TokenType.kw_proc, tokens[0].type);
    try std.testing.expectEqual(TokenType.identifier, tokens[1].type);
}

test "dbl lexer dot operators" {
    var lexer = Lexer.init("x .EQ. y .AND. z");
    const tokens = try lexer.tokenize(std.testing.allocator);
    defer std.testing.allocator.free(tokens);

    try std.testing.expectEqual(TokenType.identifier, tokens[0].type);
    try std.testing.expectEqual(TokenType.op_eq, tokens[1].type);
    try std.testing.expectEqual(TokenType.identifier, tokens[2].type);
    try std.testing.expectEqual(TokenType.op_and, tokens[3].type);
}

test "dbl lexer record definition" {
    var lexer = Lexer.init(
        \\record mydata
        \\    name, a50
        \\    amount, d10.2
        \\endrecord
    );
    const tokens = try lexer.tokenize(std.testing.allocator);
    defer std.testing.allocator.free(tokens);

    try std.testing.expectEqual(TokenType.kw_record, tokens[0].type);
    try std.testing.expectEqual(TokenType.identifier, tokens[1].type);
    try std.testing.expectEqual(TokenType.identifier, tokens[2].type); // name
    try std.testing.expectEqual(TokenType.comma, tokens[3].type);
}

test "dbl lexer compile-time directives" {
    var lexer = Lexer.init(".define DEBUG 1");
    const tokens = try lexer.tokenize(std.testing.allocator);
    defer std.testing.allocator.free(tokens);

    try std.testing.expectEqual(TokenType.dir_define, tokens[0].type);
    try std.testing.expectEqual(TokenType.identifier, tokens[1].type);
}

test "dbl lexer semicolon comments" {
    var lexer = Lexer.init("x = 1 ; this is a comment\ny = 2");
    const tokens = try lexer.tokenize(std.testing.allocator);
    defer std.testing.allocator.free(tokens);

    // Should have: x = 1 y = 2 eof (comment stripped)
    try std.testing.expectEqual(TokenType.identifier, tokens[0].type);
    try std.testing.expectEqual(TokenType.equals, tokens[1].type);
    try std.testing.expectEqual(TokenType.integer_literal, tokens[2].type);
    try std.testing.expectEqual(TokenType.identifier, tokens[3].type); // y
}

test "dbl lexer bitwise operators" {
    // Dot form
    var lexer = Lexer.init("x .BAND. y .BOR. z .BXOR. w .BNOT. v");
    const tokens = try lexer.tokenize(std.testing.allocator);
    defer std.testing.allocator.free(tokens);

    try std.testing.expectEqual(TokenType.op_band, tokens[1].type);
    try std.testing.expectEqual(TokenType.op_bor, tokens[3].type);
    try std.testing.expectEqual(TokenType.op_bxor, tokens[5].type);
    try std.testing.expectEqual(TokenType.op_bnot, tokens[7].type);
}

test "dbl lexer modern bitwise operators" {
    // Symbol form
    var lexer2 = Lexer.init("x & y | z ^ w ~ v");
    const tokens2 = try lexer2.tokenize(std.testing.allocator);
    defer std.testing.allocator.free(tokens2);

    try std.testing.expectEqual(TokenType.op_band, tokens2[1].type);
    try std.testing.expectEqual(TokenType.op_bor, tokens2[3].type);
    try std.testing.expectEqual(TokenType.op_bxor, tokens2[5].type);
    try std.testing.expectEqual(TokenType.op_bnot, tokens2[7].type);
}

test "dbl lexer bit shift operators" {
    var lexer = Lexer.init("x << 2 >> 1");
    const tokens = try lexer.tokenize(std.testing.allocator);
    defer std.testing.allocator.free(tokens);

    try std.testing.expectEqual(TokenType.identifier, tokens[0].type);
    try std.testing.expectEqual(TokenType.op_shl, tokens[1].type);
    try std.testing.expectEqual(TokenType.integer_literal, tokens[2].type);
    try std.testing.expectEqual(TokenType.op_shr, tokens[3].type);
}

test "dbl lexer string comparison operators" {
    var lexer = Lexer.init("a .EQS. b .NES. c .GTS. d");
    const tokens = try lexer.tokenize(std.testing.allocator);
    defer std.testing.allocator.free(tokens);

    try std.testing.expectEqual(TokenType.op_eqs, tokens[1].type);
    try std.testing.expectEqual(TokenType.op_nes, tokens[3].type);
    try std.testing.expectEqual(TokenType.op_gts, tokens[5].type);
}

test "dbl lexer modulo operator" {
    // Dot form
    var lexer = Lexer.init("x .MOD. y");
    const tokens = try lexer.tokenize(std.testing.allocator);
    defer std.testing.allocator.free(tokens);

    try std.testing.expectEqual(TokenType.op_mod, tokens[1].type);

    // Symbol form
    var lexer2 = Lexer.init("x % y");
    const tokens2 = try lexer2.tokenize(std.testing.allocator);
    defer std.testing.allocator.free(tokens2);

    try std.testing.expectEqual(TokenType.op_mod, tokens2[1].type);
}

test "dbl lexer rounding operators" {
    var lexer = Lexer.init("x # 2 ## 3");
    const tokens = try lexer.tokenize(std.testing.allocator);
    defer std.testing.allocator.free(tokens);

    try std.testing.expectEqual(TokenType.identifier, tokens[0].type);
    try std.testing.expectEqual(TokenType.op_round, tokens[1].type);
    try std.testing.expectEqual(TokenType.integer_literal, tokens[2].type);
    try std.testing.expectEqual(TokenType.op_round_true, tokens[3].type);
}

test "dbl lexer compound assignment" {
    var lexer = Lexer.init("x += 1 -= 2 *= 3 /= 4 &= 5 |= 6");
    const tokens = try lexer.tokenize(std.testing.allocator);
    defer std.testing.allocator.free(tokens);

    try std.testing.expectEqual(TokenType.plus_equals, tokens[1].type);
    try std.testing.expectEqual(TokenType.minus_equals, tokens[3].type);
    try std.testing.expectEqual(TokenType.star_equals, tokens[5].type);
    try std.testing.expectEqual(TokenType.slash_equals, tokens[7].type);
    try std.testing.expectEqual(TokenType.band_equals, tokens[9].type);
    try std.testing.expectEqual(TokenType.bor_equals, tokens[11].type);
}

test "dbl lexer null operators" {
    var lexer = Lexer.init("x ?? y ?. z ?");
    const tokens = try lexer.tokenize(std.testing.allocator);
    defer std.testing.allocator.free(tokens);

    try std.testing.expectEqual(TokenType.identifier, tokens[0].type);
    try std.testing.expectEqual(TokenType.op_null_coalesce, tokens[1].type);
    try std.testing.expectEqual(TokenType.identifier, tokens[2].type);
    try std.testing.expectEqual(TokenType.op_null_cond, tokens[3].type);
    try std.testing.expectEqual(TokenType.identifier, tokens[4].type);
    try std.testing.expectEqual(TokenType.question, tokens[5].type);
}

test "dbl lexer modern comparison operators" {
    // Test that modern operators work as aliases
    var lexer = Lexer.init("x == y && z || !w");
    const tokens = try lexer.tokenize(std.testing.allocator);
    defer std.testing.allocator.free(tokens);

    try std.testing.expectEqual(TokenType.op_eq, tokens[1].type);
    try std.testing.expectEqual(TokenType.op_and, tokens[3].type);
    try std.testing.expectEqual(TokenType.op_or, tokens[5].type);
    try std.testing.expectEqual(TokenType.op_not, tokens[6].type);
}

test "dbl lexer type casts" {
    var lexer = Lexer.init("^A(x) ^D(y) ^I(z) ^F(w)");
    const tokens = try lexer.tokenize(std.testing.allocator);
    defer std.testing.allocator.free(tokens);

    try std.testing.expectEqual(TokenType.cast_alpha, tokens[0].type);
    try std.testing.expectEqual(TokenType.cast_decimal, tokens[3].type);
    try std.testing.expectEqual(TokenType.cast_integer, tokens[6].type);
    try std.testing.expectEqual(TokenType.cast_float, tokens[9].type);
}
