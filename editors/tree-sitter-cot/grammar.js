/// <reference types="tree-sitter-cli/dsl" />
// @ts-check
//
// Tree-sitter grammar for Cot.
// Reference: Zig grammar (similar syntax) + docs/COT_SYNTAX.md
//
// Design: Practical grammar for highlighting and structure, not a precise
// compiler parser. Favors simplicity over handling every edge case.

const PREC = {
  ASSIGN: 1,
  NULLISH: 2,
  OR: 3,
  AND: 4,
  COMPARE: 5,
  ADD: 6,
  MULTIPLY: 7,
  UNARY: 8,
  POSTFIX: 9,
  CALL: 10,
  MEMBER: 11,
};

module.exports = grammar({
  name: "cot",

  extras: ($) => [/\s/, $.comment, ";"],

  word: ($) => $.identifier,

  conflicts: ($) => [
    [$._type, $._expression],
    [$._expression, $.struct_init],
    [$.pointer_type, $.error_union_type],
    [$.optional_type, $.error_union_type],
    [$.slice_type, $.error_union_type],
    [$.array_type, $.error_union_type],
    [$.slice_type, $.array_literal],
    [$.array_type, $._expression],
    [$.function_type, $.error_union_type],
    [$._expression, $.payload],
    [$.if_statement, $._expression, $.if_expression],
  ],

  rules: {
    source_file: ($) => repeat($._top_level),

    _top_level: ($) =>
      choice(
        $.function_declaration,
        $.struct_declaration,
        $.enum_declaration,
        $.union_declaration,
        $.trait_declaration,
        $.impl_block,
        $.import_declaration,
        $.variable_declaration,
        $.type_alias,
        $.error_set_declaration,
        $.test_declaration,
        $.comptime_block,
        $.safe_annotation,
      ),

    // ================================================================
    // Declarations
    // ================================================================

    function_declaration: ($) =>
      prec.right(seq(
        optional("pub"),
        optional("extern"),
        "fn",
        field("name", $.identifier),
        optional(field("type_params", $.type_parameter_list)),
        field("params", $.parameter_list),
        optional(field("return_type", $._type)),
        optional(field("where_clause", $.where_clause)),
        optional(field("body", $.block)),
      )),

    type_parameter_list: ($) =>
      seq("(", commaSep1($.identifier), ")"),

    parameter_list: ($) =>
      seq("(", commaSep($.parameter), ")"),

    parameter: ($) =>
      seq(
        field("name", $.identifier),
        ":",
        field("type", $._type),
      ),

    struct_declaration: ($) =>
      seq(
        optional("pub"),
        "struct",
        field("name", $.identifier),
        optional(field("type_params", $.type_parameter_list)),
        "{",
        commaSep($.field_declaration),
        optional(","),
        "}",
      ),

    field_declaration: ($) =>
      seq(
        field("name", $.identifier),
        ":",
        field("type", $._type),
        optional(seq("=", field("default", $._expression))),
      ),

    enum_declaration: ($) =>
      seq(
        optional("pub"),
        "enum",
        field("name", $.identifier),
        "{",
        commaSep($.enum_variant),
        optional(","),
        "}",
      ),

    enum_variant: ($) =>
      seq(
        field("name", $.identifier),
        optional(seq("=", field("value", $._expression))),
      ),

    union_declaration: ($) =>
      seq(
        optional("pub"),
        "union",
        field("name", $.identifier),
        "{",
        commaSep($.union_variant),
        optional(","),
        "}",
      ),

    union_variant: ($) =>
      seq(
        field("name", $.identifier),
        optional(seq(":", field("type", $._type))),
      ),

    trait_declaration: ($) =>
      seq(
        "trait",
        field("name", $.identifier),
        "{",
        repeat($.trait_method),
        "}",
      ),

    trait_method: ($) =>
      prec.right(seq(
        "fn",
        field("name", $.identifier),
        field("params", $.parameter_list),
        optional(field("return_type", $._type)),
      )),

    impl_block: ($) =>
      seq(
        "impl",
        field("trait", optional(seq($.identifier, "for"))),
        field("type", $.identifier),
        optional(field("type_params", $.type_parameter_list)),
        "{",
        repeat($.function_declaration),
        "}",
      ),

    import_declaration: ($) =>
      seq("import", field("path", $.string)),

    variable_declaration: ($) =>
      prec.right(seq(
        field("kind", choice("const", "var", "let")),
        field("name", $.identifier),
        optional(seq(":", field("type", $._type))),
        optional(seq("=", field("value", $._expression))),
      )),

    type_alias: ($) =>
      seq(
        optional("pub"),
        "type",
        field("name", $.identifier),
        "=",
        field("type", $._type),
      ),

    error_set_declaration: ($) =>
      prec.right(seq(
        field("kind", choice("const", "var")),
        field("name", $.identifier),
        "=",
        "error",
        "{",
        commaSep($.identifier),
        optional(","),
        "}",
      )),

    test_declaration: ($) =>
      seq("test", field("name", $.string), field("body", $.block)),

    comptime_block: ($) => seq("comptime", $.block),

    safe_annotation: ($) => "@safe",

    where_clause: ($) =>
      seq("where", commaSep1($.type_constraint)),

    type_constraint: ($) =>
      seq($.identifier, ":", $.identifier),

    // ================================================================
    // Types
    // ================================================================

    _type: ($) =>
      choice(
        $.primitive_type,
        $.identifier,
        $.generic_type,
        $.pointer_type,
        $.optional_type,
        $.error_union_type,
        $.slice_type,
        $.array_type,
        $.function_type,
        $.tuple_type,
      ),

    tuple_type: ($) =>
      seq("(", $._type, ",", commaSep1($._type), optional(","), ")"),

    primitive_type: ($) =>
      choice(
        "i8", "i16", "i32", "i64",
        "u8", "u16", "u32", "u64",
        "f32", "f64",
        "int", "float", "bool", "void", "string", "byte",
      ),

    generic_type: ($) =>
      prec(-1, seq($.identifier, "(", commaSep1($._type), ")")),

    pointer_type: ($) => seq("*", $._type),

    optional_type: ($) => seq("?", $._type),

    error_union_type: ($) => prec.right(choice(
      seq($._type, "!", $._type),
      seq("!", $._type),
    )),

    slice_type: ($) => seq("[", "]", $._type),

    array_type: ($) => seq("[", $.number, "]", $._type),

    function_type: ($) =>
      seq("fn", "(", commaSep($._type), ")", "->", $._type),

    // ================================================================
    // Statements (inside blocks)
    // ================================================================

    _statement: ($) =>
      choice(
        $.variable_declaration,
        $.return_statement,
        $.if_statement,
        $.while_statement,
        $.for_statement,
        $.defer_statement,
        $.break_statement,
        $.continue_statement,
        $.assignment,
        $._expression,
      ),

    return_statement: ($) =>
      prec.right(seq("return", optional($._expression))),

    if_statement: ($) =>
      prec.right(seq(
        "if",
        "(",
        field("condition", $._expression),
        ")",
        optional($.payload),
        field("body", $.block),
        optional(seq("else", choice($.if_statement, $.block))),
      )),

    while_statement: ($) =>
      seq(
        optional(field("label", $.label)),
        "while",
        "(",
        field("condition", $._expression),
        ")",
        field("body", $.block),
      ),

    for_statement: ($) =>
      seq(
        optional(field("label", $.label)),
        "for",
        optional(seq(field("index", $.identifier), ",")),
        field("item", $.identifier),
        "in",
        field("iterable", $._expression),
        field("body", $.block),
      ),

    label: ($) => seq($.identifier, ":"),

    defer_statement: ($) =>
      prec.right(seq(choice("defer", "errdefer"), choice($.assignment, $._expression))),

    break_statement: ($) =>
      prec.right(seq("break", optional(seq(":", $.identifier)))),

    continue_statement: ($) =>
      prec.right(seq("continue", optional(seq(":", $.identifier)))),

    assignment: ($) =>
      prec.right(PREC.ASSIGN, seq(
        field("target", $._expression),
        field("operator", choice("=", "+=", "-=", "*=", "/=", "%=", "&=", "|=", "^=")),
        field("value", $._expression),
      )),

    // ================================================================
    // Expressions
    // ================================================================

    _expression: ($) =>
      choice(
        $.identifier,
        $.number,
        $.string,
        $.char,
        $.boolean,
        $.null_literal,
        $.undefined_literal,
        $.builtin_call,
        $.call_expression,
        $.member_expression,
        $.index_expression,
        $.unary_expression,
        $.binary_expression,
        $.new_expression,
        $.struct_init,
        $.array_literal,
        $.closure,
        $.switch_expression,
        $.try_expression,
        $.catch_expression,
        $.error_value,
        $.range_expression,
        $.deref_expression,
        $.unwrap_expression,
        $.paren_expression,
        $.tuple_expression,
        $.zero_init,
        $.if_expression,
        $.block,
      ),

    if_expression: ($) =>
      prec.right(-1, seq(
        "if",
        "(",
        field("condition", $._expression),
        ")",
        optional($.payload),
        field("consequence", choice($.block, $._expression)),
        optional(seq("else", field("alternative", choice($.if_expression, $.block, $._expression)))),
      )),

    paren_expression: ($) => seq("(", $._expression, ")"),

    tuple_expression: ($) =>
      seq("(", $._expression, ",", commaSep1($._expression), optional(","), ")"),

    call_expression: ($) =>
      prec(PREC.CALL, seq(
        field("function", $._expression),
        "(",
        commaSep($._expression),
        ")",
      )),

    member_expression: ($) =>
      prec(PREC.MEMBER, seq(
        field("object", $._expression),
        ".",
        field("member", choice($.identifier, $.number)),
      )),

    index_expression: ($) =>
      prec(PREC.POSTFIX, seq(
        field("object", $._expression),
        "[",
        field("index", $._expression),
        optional(seq(":", field("end", $._expression))),
        "]",
      )),

    unary_expression: ($) =>
      prec(PREC.UNARY, seq(
        field("operator", choice("-", "!", "not", "~", "&")),
        field("operand", $._expression),
      )),

    binary_expression: ($) =>
      choice(
        ...[
          ["+", PREC.ADD],
          ["-", PREC.ADD],
          ["*", PREC.MULTIPLY],
          ["/", PREC.MULTIPLY],
          ["%", PREC.MULTIPLY],
          ["&", PREC.MULTIPLY],
          ["|", PREC.ADD],
          ["^", PREC.ADD],
          ["<<", PREC.MULTIPLY],
          [">>", PREC.MULTIPLY],
          ["==", PREC.COMPARE],
          ["!=", PREC.COMPARE],
          ["<", PREC.COMPARE],
          ["<=", PREC.COMPARE],
          [">", PREC.COMPARE],
          [">=", PREC.COMPARE],
          ["and", PREC.AND],
          ["&&", PREC.AND],
          ["or", PREC.OR],
          ["||", PREC.OR],
          ["??", PREC.NULLISH],
        ].map(([op, p]) =>
          prec.left(p, seq(
            field("left", $._expression),
            field("operator", op),
            field("right", $._expression),
          )),
        ),
      ),

    new_expression: ($) =>
      seq(
        "new",
        field("type", $.identifier),
        optional(field("type_args", seq("(", commaSep1($._type), ")"))),
        choice(
          seq("{", commaSep($.new_field_init), optional(","), "}"),
          seq("(", commaSep($._expression), ")"),
        ),
      ),

    new_field_init: ($) =>
      seq(
        field("name", $.identifier),
        optional(seq(":", field("value", $._expression))),
      ),

    struct_init: ($) =>
      prec.dynamic(1, seq(
        field("type", $.identifier),
        optional(seq("(", commaSep1($._type), ")")),
        "{",
        commaSep1(choice($.struct_field_init, $.new_field_init)),
        optional(","),
        "}",
      )),

    zero_init: ($) => seq(".", "{", "}"),

    struct_field_init: ($) =>
      seq(
        ".",
        field("name", $.identifier),
        "=",
        field("value", $._expression),
      ),

    array_literal: ($) =>
      seq("[", commaSep($._expression), optional(","), "]"),

    closure: ($) =>
      prec.right(seq(
        "fn",
        "(", commaSep($.parameter), ")",
        optional(field("return_type", $._type)),
        field("body", $.block),
      )),

    switch_expression: ($) =>
      seq(
        "switch",
        field("value", $._expression),
        "{",
        commaSep($.switch_arm),
        optional(","),
        "}",
      ),

    switch_arm: ($) =>
      seq(
        field("pattern", choice(
          commaSep1($._expression),
          "else",
          "_",
        )),
        optional($.payload),
        optional(seq("if", field("guard", $._expression))),
        "=>",
        field("value", $._expression),
      ),

    try_expression: ($) =>
      prec(PREC.UNARY, seq("try", field("operand", $._expression))),

    catch_expression: ($) =>
      prec.left(PREC.NULLISH, seq(
        field("operand", $._expression),
        "catch",
        optional($.payload),
        field("fallback", $._expression),
      )),

    error_value: ($) =>
      seq("error", ".", field("name", $.identifier)),

    range_expression: ($) =>
      prec.left(seq(field("start", $._expression), "..", field("end", $._expression))),

    deref_expression: ($) =>
      prec(PREC.POSTFIX, seq(field("operand", $._expression), ".", "*")),

    unwrap_expression: ($) =>
      prec(PREC.POSTFIX, seq(field("operand", $._expression), ".", "?")),

    payload: ($) => seq("|", commaSep1($.identifier), "|"),

    builtin_call: ($) =>
      seq(
        field("name", $.builtin_identifier),
        "(",
        commaSep(choice($._type, $._expression)),
        ")",
      ),

    // ================================================================
    // Blocks
    // ================================================================

    block: ($) => seq("{", repeat($._statement), "}"),

    // ================================================================
    // Terminals
    // ================================================================

    comment: ($) =>
      choice(
        token(seq("//", /.*/)),
        token(seq("/*", /[^*]*\*+([^/*][^*]*\*+)*/, "/")),
      ),

    identifier: ($) => /[a-zA-Z_][a-zA-Z0-9_]*/,

    builtin_identifier: ($) => /@[a-zA-Z_][a-zA-Z0-9_]*/,

    number: ($) =>
      choice(
        /0[xX][0-9a-fA-F][0-9a-fA-F_]*/,
        /0[bB][01][01_]*/,
        /0[oO][0-7][0-7_]*/,
        /[0-9][0-9_]*\.[0-9][0-9_]*([eE][+-]?[0-9][0-9_]*)?/,
        /[0-9][0-9_]*[eE][+-]?[0-9][0-9_]*/,
        /[0-9][0-9_]*/,
      ),

    string: ($) =>
      seq(
        '"',
        repeat(choice(
          $.string_content,
          $.escape_sequence,
          $.string_interpolation,
        )),
        '"',
      ),

    string_content: ($) => token.immediate(prec(1, /[^"\\$]+/)),
    escape_sequence: ($) => token.immediate(seq("\\", /[\\nrt0"']/)),
    string_interpolation: ($) => seq(token.immediate("${"), $._expression, "}"),

    char: ($) => seq("'", choice(/[^'\\]/, seq("\\", /[\\nrt'0]/)), "'"),

    boolean: ($) => choice("true", "false"),

    null_literal: ($) => "null",

    undefined_literal: ($) => "undefined",
  },
});

// ================================================================
// Helpers
// ================================================================

function commaSep(rule) {
  return optional(commaSep1(rule));
}

function commaSep1(rule) {
  return seq(rule, repeat(seq(",", rule)));
}
