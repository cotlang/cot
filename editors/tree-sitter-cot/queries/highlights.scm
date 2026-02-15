; Tree-sitter highlighting queries for Cot
; Reference: Zig highlights.scm adapted for Cot syntax

; ================================================================
; Variables
; ================================================================

(identifier) @variable

; Parameters
(parameter
  name: (identifier) @variable.parameter)

; Payload captures
(payload (identifier) @variable.parameter)

; ================================================================
; Types
; ================================================================

(primitive_type) @type.builtin

; Capitalized identifiers as types
((identifier) @type
  (#match? @type "^[A-Z][a-zA-Z0-9_]*$"))

(struct_declaration name: (identifier) @type.definition)
(enum_declaration name: (identifier) @type.definition)
(union_declaration name: (identifier) @type.definition)
(trait_declaration name: (identifier) @type.definition)
(generic_type (identifier) @type)
(pointer_type "*" @operator)
(optional_type "?" @operator)

; Type parameters
(type_parameter_list (identifier) @type.parameter)

; Field declarations
(field_declaration name: (identifier) @property)

; ================================================================
; Functions
; ================================================================

(function_declaration name: (identifier) @function.definition)
(call_expression function: (identifier) @function.call)
(call_expression function: (member_expression member: (identifier) @function.method.call))
(builtin_call name: (builtin_identifier) @function.builtin)
(builtin_identifier) @function.builtin

; ================================================================
; Literals
; ================================================================

(number) @number
(string) @string
(string_content) @string
(escape_sequence) @string.escape
(string_interpolation "${" @punctuation.special)
(string_interpolation "}" @punctuation.special)
(char) @character
(boolean) @constant.builtin
(null_literal) @constant.builtin
(undefined_literal) @constant.builtin
(error_value "error" @type.builtin)
(error_value (identifier) @constant)

; ================================================================
; Keywords
; ================================================================

; Declaration keywords
[
  "fn"
  "struct"
  "enum"
  "union"
  "trait"
  "impl"
  "extern"
  "test"
] @keyword.type

; Storage keywords
[
  "const"
  "var"
  "let"
] @keyword.storage

; Control flow
[
  "if"
  "else"
  "switch"
] @keyword.conditional

[
  "while"
  "for"
  "in"
] @keyword.repeat

(break_statement) @keyword.repeat
(continue_statement) @keyword.repeat

[
  "return"
] @keyword.return

[
  "try"
  "catch"
  "error"
] @keyword.exception

[
  "defer"
  "errdefer"
] @keyword

[
  "import"
] @keyword.import

[
  "new"
  "pub"
  "comptime"
  "where"
  "type"
] @keyword

; Keyword operators
[
  "and"
  "or"
  "not"
] @keyword.operator

; Language variables
((identifier) @variable.builtin
  (#eq? @variable.builtin "self"))
((identifier) @type.builtin
  (#eq? @type.builtin "Self"))

; ================================================================
; Operators
; ================================================================

[
  "="
  "+="
  "-="
  "*="
  "/="
  "%="
  "&="
  "|="
  "^="
] @operator

[
  "+"
  "-"
  "*"
  "/"
  "%"
  "&"
  "|"
  "^"
  "~"
  "<<"
  ">>"
  "!"
] @operator

[
  "=="
  "!="
  "<"
  "<="
  ">"
  ">="
] @operator

[
  "&&"
  "||"
  "??"
] @operator

[
  "=>"
  "->"
  ".."
] @operator

; ================================================================
; Punctuation
; ================================================================

["(" ")"] @punctuation.bracket
["[" "]"] @punctuation.bracket
["{" "}"] @punctuation.bracket

[","] @punctuation.delimiter
[":"] @punctuation.delimiter
["."] @punctuation.delimiter
["|"] @punctuation.delimiter

; ================================================================
; Comments
; ================================================================

(comment) @comment

; ================================================================
; Struct / New inits
; ================================================================

(struct_init type: (identifier) @type)
(struct_field_init name: (identifier) @property)
(new_expression type: (identifier) @type)
(new_field_init name: (identifier) @property)
(type_alias name: (identifier) @type.definition)
(error_set_declaration name: (identifier) @type.definition)

; ================================================================
; Enum / Union / Switch
; ================================================================

(enum_variant name: (identifier) @constant)
(union_variant name: (identifier) @constant)

; Impl block type
(impl_block type: (identifier) @type)

; Import path
(import_declaration path: (string) @string.special)

; Test name
(test_declaration name: (string) @string.special)

; Where clause
(type_constraint (identifier) @type)

; Safe annotation
(safe_annotation) @attribute
