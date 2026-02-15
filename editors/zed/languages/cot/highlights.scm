; Zed highlighting queries for Cot
; These are the same as tree-sitter-cot/queries/highlights.scm
; but can diverge if Zed needs different capture names.

; ================================================================
; Variables
; ================================================================

(identifier) @variable

(parameter
  name: (identifier) @variable.parameter)

(payload (identifier) @variable.parameter)

; ================================================================
; Types
; ================================================================

(primitive_type) @type.builtin

((identifier) @type
  (#match? @type "^[A-Z][a-zA-Z0-9_]*$"))

(struct_declaration name: (identifier) @type.definition)
(enum_declaration name: (identifier) @type.definition)
(union_declaration name: (identifier) @type.definition)
(trait_declaration name: (identifier) @type.definition)
(generic_type (identifier) @type)
(pointer_type "*" @operator)
(optional_type "?" @operator)
(type_parameter_list (identifier) @type.parameter)
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

["fn" "struct" "enum" "union" "trait" "impl" "extern" "test"] @keyword.type
["const" "var" "let"] @keyword.storage
["if" "else" "switch"] @keyword.conditional
["while" "for" "in"] @keyword.repeat
(break_statement) @keyword.repeat
(continue_statement) @keyword.repeat
["return"] @keyword.return
["try" "catch" "error"] @keyword.exception
["defer" "errdefer" "new" "pub" "comptime" "where" "type"] @keyword
["import"] @keyword.import
["and" "or" "not"] @keyword.operator

((identifier) @variable.builtin
  (#eq? @variable.builtin "self"))
((identifier) @type.builtin
  (#eq? @type.builtin "Self"))

; ================================================================
; Operators & Punctuation
; ================================================================

["=" "+=" "-=" "*=" "/=" "%=" "&=" "|=" "^="] @operator
["+" "-" "*" "/" "%" "&" "|" "^" "~" "<<" ">>" "!"] @operator
["==" "!=" "<" "<=" ">" ">="] @operator
["&&" "||" "??"] @operator
["=>" "->" ".."] @operator

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
; Special
; ================================================================

(struct_init type: (identifier) @type)
(struct_field_init name: (identifier) @property)
(new_expression type: (identifier) @type)
(new_field_init name: (identifier) @property)
(type_alias name: (identifier) @type.definition)
(error_set_declaration name: (identifier) @type.definition)
(enum_variant name: (identifier) @constant)
(union_variant name: (identifier) @constant)
(impl_block type: (identifier) @type)
(import_declaration path: (string) @string.special)
(test_declaration name: (string) @string.special)
(type_constraint (identifier) @type)
(safe_annotation) @attribute
