(block) @indent.begin
(block "}" @indent.end)

(struct_declaration "{" @indent.begin "}" @indent.end)
(enum_declaration "{" @indent.begin "}" @indent.end)
(union_declaration "{" @indent.begin "}" @indent.end)
(trait_declaration "{" @indent.begin "}" @indent.end)
(impl_block "{" @indent.begin "}" @indent.end)
(switch_expression "{" @indent.begin "}" @indent.end)

(comment) @indent.ignore
