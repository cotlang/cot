; Detect runnable code: test blocks and main functions

; Test blocks
(test_declaration
  name: (string (string_content) @_name)
) @run
(#set! tag cot-test)

; Main function
(function_declaration
  name: (identifier) @_name
  (#eq? @_name "main")
) @run
(#set! tag cot-run)
