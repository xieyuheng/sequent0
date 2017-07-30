let example_in_channel = open_in "../example/basic.scm"
let example_cs = Stream.of_channel example_in_channel
;; Lexer.print_lex_list (Lexer.lexer example_cs)
