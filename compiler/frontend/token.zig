//! Token definitions for cot.

const std = @import("std");

pub const Token = enum(u8) {
    // Special
    illegal, eof, comment,

    // Literals (literal_beg..literal_end)
    literal_beg,
    ident, int_lit, float_lit, string_lit,
    string_interp_start, string_interp_mid, string_interp_end,
    char_lit,
    literal_end,

    // Operators (operator_beg..operator_end)
    operator_beg,
    add, sub, mul, quo, rem, // + - * / %
    @"and", @"or", xor, shl, shr, not, // & | ^ << >> ~
    add_assign, sub_assign, mul_assign, quo_assign, rem_assign, // += -= *= /= %=
    and_assign, or_assign, xor_assign, // &= |= ^=
    eql, neq, lss, leq, gtr, geq, // == != < <= > >=
    land, lor, lnot, // && || !
    assign, arrow, fat_arrow, coalesce, optional_chain, // = -> => ?? ?.
    lparen, rparen, lbrack, rbrack, lbrace, rbrace, // ( ) [ ] { }
    comma, period, period_period, period_star, period_question, // , . .. .* .?
    semicolon, colon, at, question, // ; : @ ?
    operator_end,

    // Keywords (keyword_beg..keyword_end)
    keyword_beg,
    kw_fn, kw_var, kw_let, kw_const, kw_struct, kw_impl, kw_trait, kw_where,
    kw_enum, kw_union, kw_type, kw_import, kw_extern, kw_test,
    kw_if, kw_else, kw_switch, kw_while, kw_for, kw_in,
    kw_return, kw_break, kw_continue, kw_defer, kw_try, kw_catch, kw_error,
    kw_true, kw_false, kw_null, kw_new, kw_undefined,
    kw_comptime,
    kw_and, kw_or, kw_not,
    kw_int, kw_float, kw_bool, kw_string, kw_byte, kw_void,
    kw_i8, kw_i16, kw_i32, kw_i64, kw_u8, kw_u16, kw_u32, kw_u64, kw_f32, kw_f64,
    keyword_end,

    pub fn string(self: Token) []const u8 {
        return token_strings[@intFromEnum(self)];
    }

    pub fn precedence(self: Token) u8 {
        return switch (self) {
            .coalesce => 1,
            .lor, .kw_or => 2,
            .land, .kw_and => 3,
            .eql, .neq, .lss, .leq, .gtr, .geq => 4,
            .add, .sub, .@"or", .xor => 5,
            .mul, .quo, .rem, .@"and", .shl, .shr => 6,
            else => 0,
        };
    }

    pub fn isLiteral(self: Token) bool {
        const v = @intFromEnum(self);
        return v > @intFromEnum(Token.literal_beg) and v < @intFromEnum(Token.literal_end);
    }

    pub fn isOperator(self: Token) bool {
        const v = @intFromEnum(self);
        return v > @intFromEnum(Token.operator_beg) and v < @intFromEnum(Token.operator_end);
    }

    pub fn isKeyword(self: Token) bool {
        const v = @intFromEnum(self);
        return v > @intFromEnum(Token.keyword_beg) and v < @intFromEnum(Token.keyword_end);
    }

    pub fn isTypeKeyword(self: Token) bool {
        return switch (self) {
            .kw_int, .kw_float, .kw_bool, .kw_string, .kw_byte, .kw_void,
            .kw_i8, .kw_i16, .kw_i32, .kw_i64, .kw_u8, .kw_u16, .kw_u32, .kw_u64, .kw_f32, .kw_f64,
            => true,
            else => false,
        };
    }

    pub fn isAssignment(self: Token) bool {
        return switch (self) {
            .assign, .add_assign, .sub_assign, .mul_assign, .quo_assign,
            .rem_assign, .and_assign, .or_assign, .xor_assign,
            => true,
            else => false,
        };
    }
};

const token_strings = blk: {
    var s: [std.meta.fields(Token).len][]const u8 = undefined;
    for (std.meta.fields(Token)) |f| s[f.value] = f.name;
    // Readable overrides
    s[@intFromEnum(Token.illegal)] = "ILLEGAL";
    s[@intFromEnum(Token.eof)] = "EOF";
    s[@intFromEnum(Token.comment)] = "COMMENT";
    s[@intFromEnum(Token.ident)] = "IDENT";
    s[@intFromEnum(Token.int_lit)] = "INT";
    s[@intFromEnum(Token.float_lit)] = "FLOAT";
    s[@intFromEnum(Token.string_lit)] = "STRING";
    s[@intFromEnum(Token.char_lit)] = "CHAR";
    // Operators
    s[@intFromEnum(Token.add)] = "+";
    s[@intFromEnum(Token.sub)] = "-";
    s[@intFromEnum(Token.mul)] = "*";
    s[@intFromEnum(Token.quo)] = "/";
    s[@intFromEnum(Token.rem)] = "%";
    s[@intFromEnum(Token.@"and")] = "&";
    s[@intFromEnum(Token.@"or")] = "|";
    s[@intFromEnum(Token.xor)] = "^";
    s[@intFromEnum(Token.shl)] = "<<";
    s[@intFromEnum(Token.shr)] = ">>";
    s[@intFromEnum(Token.not)] = "~";
    s[@intFromEnum(Token.add_assign)] = "+=";
    s[@intFromEnum(Token.sub_assign)] = "-=";
    s[@intFromEnum(Token.mul_assign)] = "*=";
    s[@intFromEnum(Token.quo_assign)] = "/=";
    s[@intFromEnum(Token.rem_assign)] = "%=";
    s[@intFromEnum(Token.and_assign)] = "&=";
    s[@intFromEnum(Token.or_assign)] = "|=";
    s[@intFromEnum(Token.xor_assign)] = "^=";
    s[@intFromEnum(Token.eql)] = "==";
    s[@intFromEnum(Token.neq)] = "!=";
    s[@intFromEnum(Token.lss)] = "<";
    s[@intFromEnum(Token.leq)] = "<=";
    s[@intFromEnum(Token.gtr)] = ">";
    s[@intFromEnum(Token.geq)] = ">=";
    s[@intFromEnum(Token.land)] = "&&";
    s[@intFromEnum(Token.lor)] = "||";
    s[@intFromEnum(Token.lnot)] = "!";
    s[@intFromEnum(Token.assign)] = "=";
    s[@intFromEnum(Token.arrow)] = "->";
    s[@intFromEnum(Token.fat_arrow)] = "=>";
    s[@intFromEnum(Token.coalesce)] = "??";
    s[@intFromEnum(Token.optional_chain)] = "?.";
    s[@intFromEnum(Token.lparen)] = "(";
    s[@intFromEnum(Token.rparen)] = ")";
    s[@intFromEnum(Token.lbrack)] = "[";
    s[@intFromEnum(Token.rbrack)] = "]";
    s[@intFromEnum(Token.lbrace)] = "{";
    s[@intFromEnum(Token.rbrace)] = "}";
    s[@intFromEnum(Token.comma)] = ",";
    s[@intFromEnum(Token.period)] = ".";
    s[@intFromEnum(Token.period_period)] = "..";
    s[@intFromEnum(Token.period_star)] = ".*";
    s[@intFromEnum(Token.period_question)] = ".?";
    s[@intFromEnum(Token.semicolon)] = ";";
    s[@intFromEnum(Token.colon)] = ":";
    s[@intFromEnum(Token.at)] = "@";
    s[@intFromEnum(Token.question)] = "?";
    // Keywords
    s[@intFromEnum(Token.kw_fn)] = "fn";
    s[@intFromEnum(Token.kw_var)] = "var";
    s[@intFromEnum(Token.kw_let)] = "let";
    s[@intFromEnum(Token.kw_const)] = "const";
    s[@intFromEnum(Token.kw_struct)] = "struct";
    s[@intFromEnum(Token.kw_impl)] = "impl";
    s[@intFromEnum(Token.kw_trait)] = "trait";
    s[@intFromEnum(Token.kw_where)] = "where";
    s[@intFromEnum(Token.kw_enum)] = "enum";
    s[@intFromEnum(Token.kw_union)] = "union";
    s[@intFromEnum(Token.kw_type)] = "type";
    s[@intFromEnum(Token.kw_import)] = "import";
    s[@intFromEnum(Token.kw_extern)] = "extern";
    s[@intFromEnum(Token.kw_test)] = "test";
    s[@intFromEnum(Token.kw_if)] = "if";
    s[@intFromEnum(Token.kw_else)] = "else";
    s[@intFromEnum(Token.kw_switch)] = "switch";
    s[@intFromEnum(Token.kw_while)] = "while";
    s[@intFromEnum(Token.kw_for)] = "for";
    s[@intFromEnum(Token.kw_in)] = "in";
    s[@intFromEnum(Token.kw_return)] = "return";
    s[@intFromEnum(Token.kw_break)] = "break";
    s[@intFromEnum(Token.kw_continue)] = "continue";
    s[@intFromEnum(Token.kw_defer)] = "defer";
    s[@intFromEnum(Token.kw_try)] = "try";
    s[@intFromEnum(Token.kw_catch)] = "catch";
    s[@intFromEnum(Token.kw_error)] = "error";
    s[@intFromEnum(Token.kw_true)] = "true";
    s[@intFromEnum(Token.kw_false)] = "false";
    s[@intFromEnum(Token.kw_null)] = "null";
    s[@intFromEnum(Token.kw_new)] = "new";
    s[@intFromEnum(Token.kw_undefined)] = "undefined";
    s[@intFromEnum(Token.kw_comptime)] = "comptime";
    s[@intFromEnum(Token.kw_and)] = "and";
    s[@intFromEnum(Token.kw_or)] = "or";
    s[@intFromEnum(Token.kw_not)] = "not";
    s[@intFromEnum(Token.kw_int)] = "int";
    s[@intFromEnum(Token.kw_float)] = "float";
    s[@intFromEnum(Token.kw_bool)] = "bool";
    s[@intFromEnum(Token.kw_string)] = "string";
    s[@intFromEnum(Token.kw_byte)] = "byte";
    s[@intFromEnum(Token.kw_void)] = "void";
    s[@intFromEnum(Token.kw_i8)] = "i8";
    s[@intFromEnum(Token.kw_i16)] = "i16";
    s[@intFromEnum(Token.kw_i32)] = "i32";
    s[@intFromEnum(Token.kw_i64)] = "i64";
    s[@intFromEnum(Token.kw_u8)] = "u8";
    s[@intFromEnum(Token.kw_u16)] = "u16";
    s[@intFromEnum(Token.kw_u32)] = "u32";
    s[@intFromEnum(Token.kw_u64)] = "u64";
    s[@intFromEnum(Token.kw_f32)] = "f32";
    s[@intFromEnum(Token.kw_f64)] = "f64";
    break :blk s;
};

pub const keywords = std.StaticStringMap(Token).initComptime(.{
    .{ "fn", .kw_fn }, .{ "var", .kw_var }, .{ "let", .kw_let }, .{ "const", .kw_const },
    .{ "struct", .kw_struct }, .{ "impl", .kw_impl }, .{ "trait", .kw_trait }, .{ "where", .kw_where }, .{ "enum", .kw_enum }, .{ "union", .kw_union },
    .{ "type", .kw_type }, .{ "import", .kw_import }, .{ "extern", .kw_extern }, .{ "test", .kw_test },
    .{ "if", .kw_if }, .{ "else", .kw_else }, .{ "switch", .kw_switch }, .{ "while", .kw_while },
    .{ "for", .kw_for }, .{ "in", .kw_in }, .{ "return", .kw_return }, .{ "break", .kw_break },
    .{ "continue", .kw_continue }, .{ "defer", .kw_defer },
    .{ "try", .kw_try }, .{ "catch", .kw_catch }, .{ "error", .kw_error },
    .{ "true", .kw_true }, .{ "false", .kw_false }, .{ "null", .kw_null }, .{ "new", .kw_new },
    .{ "undefined", .kw_undefined }, .{ "comptime", .kw_comptime },
    .{ "and", .kw_and }, .{ "or", .kw_or }, .{ "not", .kw_not },
    .{ "int", .kw_int }, .{ "float", .kw_float }, .{ "bool", .kw_bool }, .{ "string", .kw_string },
    .{ "byte", .kw_byte }, .{ "void", .kw_void },
    .{ "i8", .kw_i8 }, .{ "i16", .kw_i16 }, .{ "i32", .kw_i32 }, .{ "i64", .kw_i64 },
    .{ "u8", .kw_u8 }, .{ "u16", .kw_u16 }, .{ "u32", .kw_u32 }, .{ "u64", .kw_u64 },
    .{ "f32", .kw_f32 }, .{ "f64", .kw_f64 },
});

pub fn lookup(name: []const u8) Token {
    return keywords.get(name) orelse .ident;
}

// ============================================================================
// Tests
// ============================================================================

test "token string" {
    try std.testing.expectEqualStrings("+", Token.add.string());
    try std.testing.expectEqualStrings("fn", Token.kw_fn.string());
    try std.testing.expectEqualStrings("==", Token.eql.string());
    try std.testing.expectEqualStrings("EOF", Token.eof.string());
}

test "keyword lookup" {
    try std.testing.expectEqual(Token.kw_fn, lookup("fn"));
    try std.testing.expectEqual(Token.kw_var, lookup("var"));
    try std.testing.expectEqual(Token.kw_and, lookup("and"));
    try std.testing.expectEqual(Token.kw_i64, lookup("i64"));
    try std.testing.expectEqual(Token.ident, lookup("notakeyword"));
    try std.testing.expectEqual(Token.ident, lookup("main"));
}

test "precedence" {
    try std.testing.expectEqual(@as(u8, 6), Token.mul.precedence());
    try std.testing.expectEqual(@as(u8, 5), Token.add.precedence());
    try std.testing.expectEqual(@as(u8, 4), Token.eql.precedence());
    try std.testing.expectEqual(@as(u8, 3), Token.kw_and.precedence());
    try std.testing.expectEqual(@as(u8, 2), Token.kw_or.precedence());
    try std.testing.expectEqual(@as(u8, 1), Token.coalesce.precedence());
    try std.testing.expectEqual(@as(u8, 0), Token.lparen.precedence());
}

test "isLiteral" {
    try std.testing.expect(Token.ident.isLiteral());
    try std.testing.expect(Token.int_lit.isLiteral());
    try std.testing.expect(Token.string_lit.isLiteral());
    try std.testing.expect(!Token.add.isLiteral());
    try std.testing.expect(!Token.kw_fn.isLiteral());
}

test "isOperator" {
    try std.testing.expect(Token.add.isOperator());
    try std.testing.expect(Token.eql.isOperator());
    try std.testing.expect(Token.lparen.isOperator());
    try std.testing.expect(!Token.ident.isOperator());
    try std.testing.expect(!Token.kw_fn.isOperator());
}

test "isKeyword" {
    try std.testing.expect(Token.kw_fn.isKeyword());
    try std.testing.expect(Token.kw_and.isKeyword());
    try std.testing.expect(Token.kw_i64.isKeyword());
    try std.testing.expect(!Token.add.isKeyword());
    try std.testing.expect(!Token.ident.isKeyword());
}

test "isTypeKeyword" {
    try std.testing.expect(Token.kw_int.isTypeKeyword());
    try std.testing.expect(Token.kw_i64.isTypeKeyword());
    try std.testing.expect(Token.kw_string.isTypeKeyword());
    try std.testing.expect(!Token.kw_fn.isTypeKeyword());
    try std.testing.expect(!Token.kw_if.isTypeKeyword());
}

test "isAssignment" {
    try std.testing.expect(Token.assign.isAssignment());
    try std.testing.expect(Token.add_assign.isAssignment());
    try std.testing.expect(!Token.add.isAssignment());
    try std.testing.expect(!Token.eql.isAssignment());
}
