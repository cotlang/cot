//! Token definitions for TypeScript/JavaScript lexing.
//!
//! These are the token kinds produced by the libts scanner. They cover all
//! TS/JS operators, keywords, and literals. The transform layer maps these
//! to Cot AST nodes — the Cot token enum is NOT modified.

const std = @import("std");

pub const Token = enum(u8) {
    // Special
    illegal,
    eof,

    // Literals
    literal_beg,
    ident, // identifier
    private_ident, // #identifier (private field)
    int_lit, // 42, 0xFF, 0b1010, 0o777
    float_lit, // 3.14, 1e10
    bigint_lit, // 42n
    string_lit, // "..." or '...'
    regex_lit, // /pattern/flags
    template_head, // `text${
    template_middle, // }text${
    template_tail, // }text`
    template_none, // `text` (no substitutions)
    literal_end,

    // Operators
    operator_beg,
    // Arithmetic
    add, // +
    sub, // -
    mul, // *
    quo, // /
    rem, // %
    power, // **
    // Increment/Decrement
    increment, // ++
    decrement, // --
    // Bitwise
    @"and", // &
    @"or", // |
    xor, // ^
    shl, // <<
    shr, // >>
    unsigned_shr, // >>>
    not, // ~
    // Assignment
    assign, // =
    add_assign, // +=
    sub_assign, // -=
    mul_assign, // *=
    quo_assign, // /=
    rem_assign, // %=
    power_assign, // **=
    and_assign, // &=
    or_assign, // |=
    xor_assign, // ^=
    shl_assign, // <<=
    shr_assign, // >>=
    unsigned_shr_assign, // >>>=
    lor_assign, // ||=
    land_assign, // &&=
    nullish_assign, // ??=
    // Comparison
    eql, // ==
    neq, // !=
    strict_eql, // ===
    strict_neq, // !==
    lss, // <
    leq, // <=
    gtr, // >
    geq, // >=
    // Logical
    land, // &&
    lor, // ||
    lnot, // !
    nullish_coalesce, // ??
    // Special operators
    arrow, // =>
    optional_chain, // ?.
    ellipsis, // ...
    hash, // # (standalone, not part of #ident)
    // Delimiters
    lparen, // (
    rparen, // )
    lbrack, // [
    rbrack, // ]
    lbrace, // {
    rbrace, // }
    // Punctuation
    comma, // ,
    period, // .
    semicolon, // ;
    colon, // :
    question, // ?
    at, // @
    operator_end,

    // Keywords
    keyword_beg,
    // Declaration
    kw_function,
    kw_var,
    kw_let,
    kw_const,
    kw_class,
    kw_interface,
    kw_type,
    kw_enum,
    kw_namespace,
    kw_module,
    kw_import,
    kw_export,
    kw_from,
    kw_extends,
    kw_implements,
    kw_declare,
    kw_abstract,
    // Control flow
    kw_if,
    kw_else,
    kw_switch,
    kw_case,
    kw_default,
    kw_while,
    kw_do,
    kw_for,
    kw_in,
    kw_of,
    kw_return,
    kw_break,
    kw_continue,
    kw_throw,
    kw_try,
    kw_catch,
    kw_finally,
    // Values
    kw_true,
    kw_false,
    kw_null,
    kw_undefined,
    kw_new,
    kw_delete,
    kw_typeof,
    kw_instanceof,
    kw_void,
    kw_this,
    kw_super,
    // Async
    kw_async,
    kw_await,
    kw_yield,
    // Modifiers
    kw_static,
    kw_readonly,
    kw_private,
    kw_protected,
    kw_public,
    kw_override,
    kw_accessor,
    // Type keywords
    kw_number,
    kw_string,
    kw_boolean,
    kw_any,
    kw_unknown,
    kw_never,
    kw_object,
    kw_symbol,
    kw_bigint,
    kw_keyof,
    kw_infer,
    kw_satisfies,
    kw_as,
    kw_is,
    // Other
    kw_debugger,
    kw_with,
    kw_get,
    kw_set,
    keyword_end,

    pub fn string(self: Token) []const u8 {
        return token_strings[@intFromEnum(self)];
    }

    pub fn precedence(self: Token) u8 {
        return switch (self) {
            .nullish_coalesce => 1,
            .lor => 2,
            .land => 3,
            .@"or" => 4,
            .xor => 5,
            .@"and" => 6,
            .eql, .neq, .strict_eql, .strict_neq => 7,
            .lss, .leq, .gtr, .geq, .kw_in, .kw_instanceof => 8,
            .shl, .shr, .unsigned_shr => 9,
            .add, .sub => 10,
            .mul, .quo, .rem => 11,
            .power => 12,
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

    pub fn isAssignment(self: Token) bool {
        return switch (self) {
            .assign, .add_assign, .sub_assign, .mul_assign, .quo_assign,
            .rem_assign, .power_assign, .and_assign, .or_assign, .xor_assign,
            .shl_assign, .shr_assign, .unsigned_shr_assign,
            .lor_assign, .land_assign, .nullish_assign,
            => true,
            else => false,
        };
    }

    /// True for tokens that can start an expression (used for regex disambiguation).
    pub fn isExpressionStart(self: Token) bool {
        return switch (self) {
            .ident, .private_ident,
            .int_lit, .float_lit, .bigint_lit, .string_lit, .regex_lit,
            .template_head, .template_none,
            .kw_true, .kw_false, .kw_null, .kw_undefined,
            .kw_this, .kw_super, .kw_new, .kw_typeof, .kw_void, .kw_delete,
            .kw_function, .kw_class, .kw_async, .kw_await, .kw_yield,
            .lparen, .lbrack, .lbrace,
            .add, .sub, .lnot, .not, .increment, .decrement,
            => true,
            else => false,
        };
    }

    /// True if a slash after this token should be parsed as division, not regex.
    /// Based on the TS/JS spec: if the preceding token is the end of an expression,
    /// then `/` is division. Otherwise it starts a regex.
    pub fn slashIsDivision(self: Token) bool {
        return switch (self) {
            .ident, .private_ident,
            .int_lit, .float_lit, .bigint_lit, .string_lit,
            .template_tail, .template_none,
            .kw_true, .kw_false, .kw_null, .kw_undefined, .kw_this, .kw_super,
            .rparen, .rbrack, .rbrace,
            .increment, .decrement,
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
    s[@intFromEnum(Token.ident)] = "IDENT";
    s[@intFromEnum(Token.private_ident)] = "#IDENT";
    s[@intFromEnum(Token.int_lit)] = "INT";
    s[@intFromEnum(Token.float_lit)] = "FLOAT";
    s[@intFromEnum(Token.bigint_lit)] = "BIGINT";
    s[@intFromEnum(Token.string_lit)] = "STRING";
    s[@intFromEnum(Token.regex_lit)] = "REGEX";
    s[@intFromEnum(Token.template_head)] = "TEMPLATE_HEAD";
    s[@intFromEnum(Token.template_middle)] = "TEMPLATE_MIDDLE";
    s[@intFromEnum(Token.template_tail)] = "TEMPLATE_TAIL";
    s[@intFromEnum(Token.template_none)] = "TEMPLATE_NONE";
    // Operators
    s[@intFromEnum(Token.add)] = "+";
    s[@intFromEnum(Token.sub)] = "-";
    s[@intFromEnum(Token.mul)] = "*";
    s[@intFromEnum(Token.quo)] = "/";
    s[@intFromEnum(Token.rem)] = "%";
    s[@intFromEnum(Token.power)] = "**";
    s[@intFromEnum(Token.increment)] = "++";
    s[@intFromEnum(Token.decrement)] = "--";
    s[@intFromEnum(Token.@"and")] = "&";
    s[@intFromEnum(Token.@"or")] = "|";
    s[@intFromEnum(Token.xor)] = "^";
    s[@intFromEnum(Token.shl)] = "<<";
    s[@intFromEnum(Token.shr)] = ">>";
    s[@intFromEnum(Token.unsigned_shr)] = ">>>";
    s[@intFromEnum(Token.not)] = "~";
    s[@intFromEnum(Token.assign)] = "=";
    s[@intFromEnum(Token.add_assign)] = "+=";
    s[@intFromEnum(Token.sub_assign)] = "-=";
    s[@intFromEnum(Token.mul_assign)] = "*=";
    s[@intFromEnum(Token.quo_assign)] = "/=";
    s[@intFromEnum(Token.rem_assign)] = "%=";
    s[@intFromEnum(Token.power_assign)] = "**=";
    s[@intFromEnum(Token.and_assign)] = "&=";
    s[@intFromEnum(Token.or_assign)] = "|=";
    s[@intFromEnum(Token.xor_assign)] = "^=";
    s[@intFromEnum(Token.shl_assign)] = "<<=";
    s[@intFromEnum(Token.shr_assign)] = ">>=";
    s[@intFromEnum(Token.unsigned_shr_assign)] = ">>>=";
    s[@intFromEnum(Token.lor_assign)] = "||=";
    s[@intFromEnum(Token.land_assign)] = "&&=";
    s[@intFromEnum(Token.nullish_assign)] = "??=";
    s[@intFromEnum(Token.eql)] = "==";
    s[@intFromEnum(Token.neq)] = "!=";
    s[@intFromEnum(Token.strict_eql)] = "===";
    s[@intFromEnum(Token.strict_neq)] = "!==";
    s[@intFromEnum(Token.lss)] = "<";
    s[@intFromEnum(Token.leq)] = "<=";
    s[@intFromEnum(Token.gtr)] = ">";
    s[@intFromEnum(Token.geq)] = ">=";
    s[@intFromEnum(Token.land)] = "&&";
    s[@intFromEnum(Token.lor)] = "||";
    s[@intFromEnum(Token.lnot)] = "!";
    s[@intFromEnum(Token.nullish_coalesce)] = "??";
    s[@intFromEnum(Token.arrow)] = "=>";
    s[@intFromEnum(Token.optional_chain)] = "?.";
    s[@intFromEnum(Token.ellipsis)] = "...";
    s[@intFromEnum(Token.hash)] = "#";
    s[@intFromEnum(Token.lparen)] = "(";
    s[@intFromEnum(Token.rparen)] = ")";
    s[@intFromEnum(Token.lbrack)] = "[";
    s[@intFromEnum(Token.rbrack)] = "]";
    s[@intFromEnum(Token.lbrace)] = "{";
    s[@intFromEnum(Token.rbrace)] = "}";
    s[@intFromEnum(Token.comma)] = ",";
    s[@intFromEnum(Token.period)] = ".";
    s[@intFromEnum(Token.semicolon)] = ";";
    s[@intFromEnum(Token.colon)] = ":";
    s[@intFromEnum(Token.question)] = "?";
    s[@intFromEnum(Token.at)] = "@";
    // Keywords
    s[@intFromEnum(Token.kw_function)] = "function";
    s[@intFromEnum(Token.kw_var)] = "var";
    s[@intFromEnum(Token.kw_let)] = "let";
    s[@intFromEnum(Token.kw_const)] = "const";
    s[@intFromEnum(Token.kw_class)] = "class";
    s[@intFromEnum(Token.kw_interface)] = "interface";
    s[@intFromEnum(Token.kw_type)] = "type";
    s[@intFromEnum(Token.kw_enum)] = "enum";
    s[@intFromEnum(Token.kw_namespace)] = "namespace";
    s[@intFromEnum(Token.kw_module)] = "module";
    s[@intFromEnum(Token.kw_import)] = "import";
    s[@intFromEnum(Token.kw_export)] = "export";
    s[@intFromEnum(Token.kw_from)] = "from";
    s[@intFromEnum(Token.kw_extends)] = "extends";
    s[@intFromEnum(Token.kw_implements)] = "implements";
    s[@intFromEnum(Token.kw_declare)] = "declare";
    s[@intFromEnum(Token.kw_abstract)] = "abstract";
    s[@intFromEnum(Token.kw_if)] = "if";
    s[@intFromEnum(Token.kw_else)] = "else";
    s[@intFromEnum(Token.kw_switch)] = "switch";
    s[@intFromEnum(Token.kw_case)] = "case";
    s[@intFromEnum(Token.kw_default)] = "default";
    s[@intFromEnum(Token.kw_while)] = "while";
    s[@intFromEnum(Token.kw_do)] = "do";
    s[@intFromEnum(Token.kw_for)] = "for";
    s[@intFromEnum(Token.kw_in)] = "in";
    s[@intFromEnum(Token.kw_of)] = "of";
    s[@intFromEnum(Token.kw_return)] = "return";
    s[@intFromEnum(Token.kw_break)] = "break";
    s[@intFromEnum(Token.kw_continue)] = "continue";
    s[@intFromEnum(Token.kw_throw)] = "throw";
    s[@intFromEnum(Token.kw_try)] = "try";
    s[@intFromEnum(Token.kw_catch)] = "catch";
    s[@intFromEnum(Token.kw_finally)] = "finally";
    s[@intFromEnum(Token.kw_true)] = "true";
    s[@intFromEnum(Token.kw_false)] = "false";
    s[@intFromEnum(Token.kw_null)] = "null";
    s[@intFromEnum(Token.kw_undefined)] = "undefined";
    s[@intFromEnum(Token.kw_new)] = "new";
    s[@intFromEnum(Token.kw_delete)] = "delete";
    s[@intFromEnum(Token.kw_typeof)] = "typeof";
    s[@intFromEnum(Token.kw_instanceof)] = "instanceof";
    s[@intFromEnum(Token.kw_void)] = "void";
    s[@intFromEnum(Token.kw_this)] = "this";
    s[@intFromEnum(Token.kw_super)] = "super";
    s[@intFromEnum(Token.kw_async)] = "async";
    s[@intFromEnum(Token.kw_await)] = "await";
    s[@intFromEnum(Token.kw_yield)] = "yield";
    s[@intFromEnum(Token.kw_static)] = "static";
    s[@intFromEnum(Token.kw_readonly)] = "readonly";
    s[@intFromEnum(Token.kw_private)] = "private";
    s[@intFromEnum(Token.kw_protected)] = "protected";
    s[@intFromEnum(Token.kw_public)] = "public";
    s[@intFromEnum(Token.kw_override)] = "override";
    s[@intFromEnum(Token.kw_accessor)] = "accessor";
    s[@intFromEnum(Token.kw_number)] = "number";
    s[@intFromEnum(Token.kw_string)] = "string";
    s[@intFromEnum(Token.kw_boolean)] = "boolean";
    s[@intFromEnum(Token.kw_any)] = "any";
    s[@intFromEnum(Token.kw_unknown)] = "unknown";
    s[@intFromEnum(Token.kw_never)] = "never";
    s[@intFromEnum(Token.kw_object)] = "object";
    s[@intFromEnum(Token.kw_symbol)] = "symbol";
    s[@intFromEnum(Token.kw_bigint)] = "bigint";
    s[@intFromEnum(Token.kw_keyof)] = "keyof";
    s[@intFromEnum(Token.kw_infer)] = "infer";
    s[@intFromEnum(Token.kw_satisfies)] = "satisfies";
    s[@intFromEnum(Token.kw_as)] = "as";
    s[@intFromEnum(Token.kw_is)] = "is";
    s[@intFromEnum(Token.kw_debugger)] = "debugger";
    s[@intFromEnum(Token.kw_with)] = "with";
    s[@intFromEnum(Token.kw_get)] = "get";
    s[@intFromEnum(Token.kw_set)] = "set";
    break :blk s;
};

/// Reserved keywords that are always keywords (cannot be used as identifiers).
pub const reserved_keywords = std.StaticStringMap(Token).initComptime(.{
    // Declaration
    .{ "function", .kw_function },
    .{ "var", .kw_var },
    .{ "let", .kw_let },
    .{ "const", .kw_const },
    .{ "class", .kw_class },
    .{ "enum", .kw_enum },
    .{ "import", .kw_import },
    .{ "export", .kw_export },
    .{ "extends", .kw_extends },
    // Control flow
    .{ "if", .kw_if },
    .{ "else", .kw_else },
    .{ "switch", .kw_switch },
    .{ "case", .kw_case },
    .{ "default", .kw_default },
    .{ "while", .kw_while },
    .{ "do", .kw_do },
    .{ "for", .kw_for },
    .{ "in", .kw_in },
    .{ "of", .kw_of },
    .{ "return", .kw_return },
    .{ "break", .kw_break },
    .{ "continue", .kw_continue },
    .{ "throw", .kw_throw },
    .{ "try", .kw_try },
    .{ "catch", .kw_catch },
    .{ "finally", .kw_finally },
    // Values
    .{ "true", .kw_true },
    .{ "false", .kw_false },
    .{ "null", .kw_null },
    .{ "undefined", .kw_undefined },
    .{ "new", .kw_new },
    .{ "delete", .kw_delete },
    .{ "typeof", .kw_typeof },
    .{ "instanceof", .kw_instanceof },
    .{ "void", .kw_void },
    .{ "this", .kw_this },
    .{ "super", .kw_super },
    // Async
    .{ "async", .kw_async },
    .{ "await", .kw_await },
    .{ "yield", .kw_yield },
    // Other
    .{ "debugger", .kw_debugger },
    .{ "with", .kw_with },
});

/// Contextual keywords — keywords in TS that can also be used as identifiers
/// in some contexts. The parser disambiguates based on position.
pub const contextual_keywords = std.StaticStringMap(Token).initComptime(.{
    .{ "interface", .kw_interface },
    .{ "type", .kw_type },
    .{ "namespace", .kw_namespace },
    .{ "module", .kw_module },
    .{ "from", .kw_from },
    .{ "implements", .kw_implements },
    .{ "declare", .kw_declare },
    .{ "abstract", .kw_abstract },
    .{ "static", .kw_static },
    .{ "readonly", .kw_readonly },
    .{ "private", .kw_private },
    .{ "protected", .kw_protected },
    .{ "public", .kw_public },
    .{ "override", .kw_override },
    .{ "accessor", .kw_accessor },
    .{ "number", .kw_number },
    .{ "string", .kw_string },
    .{ "boolean", .kw_boolean },
    .{ "any", .kw_any },
    .{ "unknown", .kw_unknown },
    .{ "never", .kw_never },
    .{ "object", .kw_object },
    .{ "symbol", .kw_symbol },
    .{ "bigint", .kw_bigint },
    .{ "keyof", .kw_keyof },
    .{ "infer", .kw_infer },
    .{ "satisfies", .kw_satisfies },
    .{ "as", .kw_as },
    .{ "is", .kw_is },
    .{ "get", .kw_get },
    .{ "set", .kw_set },
});

/// Look up a word — check reserved keywords first, then contextual keywords.
/// Returns .ident if not a keyword.
pub fn lookup(name: []const u8) Token {
    return reserved_keywords.get(name) orelse
        contextual_keywords.get(name) orelse
        .ident;
}

// ============================================================================
// Tests
// ============================================================================

test "token string" {
    try std.testing.expectEqualStrings("+", Token.add.string());
    try std.testing.expectEqualStrings("function", Token.kw_function.string());
    try std.testing.expectEqualStrings("===", Token.strict_eql.string());
    try std.testing.expectEqualStrings("EOF", Token.eof.string());
    try std.testing.expectEqualStrings("**", Token.power.string());
    try std.testing.expectEqualStrings(">>>", Token.unsigned_shr.string());
    try std.testing.expectEqualStrings("...", Token.ellipsis.string());
    try std.testing.expectEqualStrings("??", Token.nullish_coalesce.string());
}

test "keyword lookup" {
    try std.testing.expectEqual(Token.kw_function, lookup("function"));
    try std.testing.expectEqual(Token.kw_var, lookup("var"));
    try std.testing.expectEqual(Token.kw_const, lookup("const"));
    try std.testing.expectEqual(Token.kw_class, lookup("class"));
    try std.testing.expectEqual(Token.kw_if, lookup("if"));
    try std.testing.expectEqual(Token.kw_async, lookup("async"));
    try std.testing.expectEqual(Token.kw_await, lookup("await"));
    try std.testing.expectEqual(Token.ident, lookup("notakeyword"));
    try std.testing.expectEqual(Token.ident, lookup("main"));
}

test "contextual keyword lookup" {
    try std.testing.expectEqual(Token.kw_interface, lookup("interface"));
    try std.testing.expectEqual(Token.kw_type, lookup("type"));
    try std.testing.expectEqual(Token.kw_as, lookup("as"));
    try std.testing.expectEqual(Token.kw_readonly, lookup("readonly"));
    try std.testing.expectEqual(Token.kw_any, lookup("any"));
    try std.testing.expectEqual(Token.kw_number, lookup("number"));
    try std.testing.expectEqual(Token.kw_string, lookup("string"));
    try std.testing.expectEqual(Token.kw_keyof, lookup("keyof"));
}

test "precedence" {
    try std.testing.expectEqual(@as(u8, 12), Token.power.precedence());
    try std.testing.expectEqual(@as(u8, 11), Token.mul.precedence());
    try std.testing.expectEqual(@as(u8, 10), Token.add.precedence());
    try std.testing.expectEqual(@as(u8, 9), Token.shl.precedence());
    try std.testing.expectEqual(@as(u8, 8), Token.lss.precedence());
    try std.testing.expectEqual(@as(u8, 7), Token.strict_eql.precedence());
    try std.testing.expectEqual(@as(u8, 3), Token.land.precedence());
    try std.testing.expectEqual(@as(u8, 2), Token.lor.precedence());
    try std.testing.expectEqual(@as(u8, 1), Token.nullish_coalesce.precedence());
    try std.testing.expectEqual(@as(u8, 0), Token.lparen.precedence());
}

test "isAssignment" {
    try std.testing.expect(Token.assign.isAssignment());
    try std.testing.expect(Token.add_assign.isAssignment());
    try std.testing.expect(Token.power_assign.isAssignment());
    try std.testing.expect(Token.nullish_assign.isAssignment());
    try std.testing.expect(Token.lor_assign.isAssignment());
    try std.testing.expect(Token.unsigned_shr_assign.isAssignment());
    try std.testing.expect(!Token.add.isAssignment());
    try std.testing.expect(!Token.eql.isAssignment());
}

test "slashIsDivision" {
    // After identifier/literal → division
    try std.testing.expect(Token.ident.slashIsDivision());
    try std.testing.expect(Token.int_lit.slashIsDivision());
    try std.testing.expect(Token.rparen.slashIsDivision());
    try std.testing.expect(Token.rbrack.slashIsDivision());
    // After operator → regex
    try std.testing.expect(!Token.assign.slashIsDivision());
    try std.testing.expect(!Token.add.slashIsDivision());
    try std.testing.expect(!Token.lparen.slashIsDivision());
    try std.testing.expect(!Token.comma.slashIsDivision());
}
