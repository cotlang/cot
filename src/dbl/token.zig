//! Token types for DBL Lexer
//!
//! This defines all token types needed for DBL syntax.
//! DBL is a separate frontend that produces compatible AST for the cot pipeline.

const std = @import("std");

/// A token from DBL source code
pub const Token = struct {
    type: TokenType,
    lexeme: []const u8,
    line: usize,
    column: usize,

    pub fn format(
        self: Token,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        try writer.print("{s}('{s}' @ {}:{})", .{
            @tagName(self.type),
            self.lexeme,
            self.line,
            self.column,
        });
    }
};

/// Token types for DBL syntax
pub const TokenType = enum {
    // Literals
    identifier,
    string_literal,
    integer_literal,
    decimal_literal,

    // Single-character tokens
    lparen, // (
    rparen, // )
    lbracket, // [
    rbracket, // ]
    lbrace, // {
    rbrace, // }
    comma, // ,
    period, // .
    colon, // :
    semicolon, // ;
    plus, // +
    minus, // -
    star, // *
    slash, // /
    hash, // # (string concatenation)
    equals, // =
    at, // @
    percent, // %

    // Multi-character tokens
    arrow, // ->
    fat_arrow, // =>
    double_colon, // ::
    range, // ..
    range_inclusive, // ..=
    walrus, // :=

    // Comparison operators (both symbol and dot forms)
    op_eq, // == or .EQ.
    op_ne, // != or .NE. or <>
    op_lt, // < or .LT.
    op_le, // <= or .LE.
    op_gt, // > or .GT.
    op_ge, // >= or .GE.

    // Logical operators (both symbol and dot forms)
    op_and, // && or .AND.
    op_or, // || or .OR.
    op_not, // ! or .NOT.
    op_xor, // .XOR.

    // Compile-time directives
    dir_define, // .define
    dir_if, // .if
    dir_else, // .else
    dir_end, // .end / .endc
    dir_ifdef, // .ifdef
    dir_ifndef, // .ifndef

    // Program structure keywords
    kw_record,
    kw_endrecord,
    kw_proc,
    kw_end,
    kw_main,
    kw_endmain,
    kw_subroutine,
    kw_endsubroutine,
    kw_function,
    kw_endfunction,
    kw_group,
    kw_endgroup,
    kw_structure,
    kw_endstructure,
    kw_common,
    kw_endcommon,
    kw_literal,
    kw_endliteral,

    // Control flow keywords
    kw_if,
    kw_then,
    kw_else,
    kw_begin,
    kw_case,
    kw_endcase,
    kw_using,
    kw_endusing,
    kw_select,

    // Loop keywords
    kw_do,
    kw_until,
    kw_while,
    kw_endwhile,
    kw_for,
    kw_from,
    kw_thru,
    kw_foreach,
    kw_in,
    kw_forever,
    kw_repeat,
    kw_exitloop,
    kw_nextloop,
    kw_loop,
    kw_break,
    kw_continue,
    kw_step,

    // Parameter direction modifiers
    kw_out,
    kw_inout,

    // Subroutine/function keywords
    kw_xcall,
    kw_xreturn,
    kw_call,
    kw_return,
    kw_freturn,
    kw_mreturn,
    kw_goto,
    kw_stop,
    kw_exit,

    // I/O keywords
    kw_find,
    kw_unlock,
    kw_flush,
    kw_accept,

    // Data manipulation keywords
    kw_clear,
    kw_init,
    kw_incr,
    kw_decr,
    kw_locase,
    kw_upcase,
    kw_data,

    // OOP keywords
    kw_class,
    kw_endclass,
    kw_method,
    kw_endmethod,
    kw_property,
    kw_endproperty,
    kw_namespace,
    kw_endnamespace,
    kw_import,
    kw_extends,
    kw_implements,
    kw_interface,
    kw_endinterface,
    kw_public,
    kw_private,
    kw_protected,
    kw_static,
    kw_virtual,
    kw_override,
    kw_new,

    // Exception handling keywords
    kw_try,
    kw_catch,
    kw_finally,
    kw_endtry,
    kw_throw,
    kw_onerror,
    kw_offerror,

    // Compile-time keywords
    kw_comptime,
    kw_const,
    kw_inline,
    kw_not,

    // Boolean keywords
    kw_true,
    kw_false,

    // Modern keywords (for compatibility/migration)
    kw_fn,
    kw_struct,
    kw_let,
    kw_mut,
    kw_self,
    kw_match,
    kw_impl,
    kw_type,

    // Special
    eof,
    invalid,
    newline,

    /// Check if this is a keyword token
    pub fn isKeyword(self: TokenType) bool {
        return switch (self) {
            .kw_record,
            .kw_endrecord,
            .kw_proc,
            .kw_end,
            .kw_main,
            .kw_endmain,
            .kw_subroutine,
            .kw_endsubroutine,
            .kw_function,
            .kw_endfunction,
            .kw_group,
            .kw_endgroup,
            .kw_structure,
            .kw_endstructure,
            .kw_common,
            .kw_endcommon,
            .kw_literal,
            .kw_endliteral,
            .kw_if,
            .kw_then,
            .kw_else,
            .kw_begin,
            .kw_case,
            .kw_endcase,
            .kw_using,
            .kw_endusing,
            .kw_select,
            .kw_do,
            .kw_until,
            .kw_while,
            .kw_endwhile,
            .kw_for,
            .kw_from,
            .kw_thru,
            .kw_foreach,
            .kw_in,
            .kw_forever,
            .kw_repeat,
            .kw_exitloop,
            .kw_nextloop,
            .kw_loop,
            .kw_break,
            .kw_continue,
            .kw_step,
            .kw_out,
            .kw_inout,
            .kw_xcall,
            .kw_xreturn,
            .kw_call,
            .kw_return,
            .kw_freturn,
            .kw_mreturn,
            .kw_goto,
            .kw_stop,
            .kw_exit,
            .kw_find,
            .kw_unlock,
            .kw_flush,
            .kw_accept,
            .kw_clear,
            .kw_init,
            .kw_incr,
            .kw_decr,
            .kw_locase,
            .kw_upcase,
            .kw_data,
            .kw_class,
            .kw_endclass,
            .kw_method,
            .kw_endmethod,
            .kw_property,
            .kw_endproperty,
            .kw_namespace,
            .kw_endnamespace,
            .kw_import,
            .kw_extends,
            .kw_implements,
            .kw_interface,
            .kw_endinterface,
            .kw_public,
            .kw_private,
            .kw_protected,
            .kw_static,
            .kw_virtual,
            .kw_override,
            .kw_new,
            .kw_try,
            .kw_catch,
            .kw_finally,
            .kw_endtry,
            .kw_throw,
            .kw_onerror,
            .kw_offerror,
            .kw_comptime,
            .kw_const,
            .kw_inline,
            .kw_not,
            .kw_true,
            .kw_false,
            .kw_fn,
            .kw_struct,
            .kw_let,
            .kw_mut,
            .kw_self,
            .kw_match,
            .kw_impl,
            .kw_type,
            => true,
            else => false,
        };
    }

    /// Check if this is an operator token
    pub fn isOperator(self: TokenType) bool {
        return switch (self) {
            .plus,
            .minus,
            .star,
            .slash,
            .hash,
            .equals,
            .op_eq,
            .op_ne,
            .op_lt,
            .op_le,
            .op_gt,
            .op_ge,
            .op_and,
            .op_or,
            .op_not,
            .op_xor,
            => true,
            else => false,
        };
    }

    /// Check if this is a literal token
    pub fn isLiteral(self: TokenType) bool {
        return switch (self) {
            .string_literal,
            .integer_literal,
            .decimal_literal,
            => true,
            else => false,
        };
    }
};

test "dbl token format" {
    const token = Token{
        .type = .kw_proc,
        .lexeme = "PROC",
        .line = 1,
        .column = 1,
    };
    var buf: [100]u8 = undefined;
    var fbs = std.io.fixedBufferStream(&buf);
    try token.format("", .{}, fbs.writer());
    try std.testing.expectEqualStrings("kw_proc('PROC' @ 1:1)", fbs.getWritten());
}
