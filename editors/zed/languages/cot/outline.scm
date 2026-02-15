; Code outline for the symbols panel

(function_declaration
  "pub"? @context
  "extern"? @context
  "fn" @context
  name: (identifier) @name) @item

(struct_declaration
  "pub"? @context
  "struct" @context
  name: (identifier) @name) @item

(enum_declaration
  "pub"? @context
  "enum" @context
  name: (identifier) @name) @item

(union_declaration
  "pub"? @context
  "union" @context
  name: (identifier) @name) @item

(trait_declaration
  "trait" @context
  name: (identifier) @name) @item

(impl_block
  "impl" @context
  type: (identifier) @name) @item

(variable_declaration
  kind: _ @context
  name: (identifier) @name) @item

(test_declaration
  "test" @context
  name: (string) @name) @item

(type_alias
  "type" @context
  name: (identifier) @name) @item

(error_set_declaration
  name: (identifier) @name) @item
