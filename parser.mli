(* dep :: lexer *)

type sexp =
  | ROUND  of sexp list
  | SQUARE of sexp list
  | FLOWER of sexp list
  | WORD   of word
and word = bytes
and key  = word

val sexp_list_of_lex_list : Lexer.lex list -> sexp list

val print_sexp : sexp -> unit
val print_sexp_list : sexp list -> unit
