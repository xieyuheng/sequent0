(* indep *)

type lex =
  | ROUND_BRA
  | ROUND_KET
  | SQUARE_BRA
  | SQUARE_KET
  | FLOWER_BRA
  | FLOWER_KET
  | WORD of bytes

val bytes_of_char_list : char list -> bytes

val lexer : char Stream.t -> lex list

val print_lex : lex -> unit
val print_lex_list : lex list -> unit
