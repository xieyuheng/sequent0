(* dep :: lexer
   ocamlc -c parser.mli
   ocamlc -c parser.ml
*)

type sexp =
  | ROUND  of sexp list
  | SQUARE of sexp list
  | FLOWER of sexp list
  | WORD   of word
and word = bytes
and key  = word

exception MISS_MATCH of bytes

let rec sexp_list_of_lex_list xl =
  match xl with
  | [] -> []
  | h :: r ->
    match h with
    | Lexer.ROUND_BRA  -> ROUND  (left_of_ket r) :: sexp_list_of_lex_list (right_of_ket r)
    | Lexer.SQUARE_BRA -> SQUARE (left_of_ket r) :: sexp_list_of_lex_list (right_of_ket r)
    | Lexer.FLOWER_BRA -> FLOWER (left_of_ket r) :: sexp_list_of_lex_list (right_of_ket r)
    | Lexer.WORD bs    -> WORD bs :: sexp_list_of_lex_list r
    | Lexer.ROUND_KET
    | Lexer.SQUARE_KET
    | Lexer.FLOWER_KET -> []
and left_of_ket xl =
  match xl with
  | [] -> raise (MISS_MATCH "left_of_ket")
  | h :: r ->
    match h with
    | Lexer.ROUND_BRA  -> ROUND  (sexp_list_of_lex_list r) :: left_of_ket (right_of_ket r)
    | Lexer.SQUARE_BRA -> SQUARE (sexp_list_of_lex_list r) :: left_of_ket (right_of_ket r)
    | Lexer.FLOWER_BRA -> FLOWER (sexp_list_of_lex_list r) :: left_of_ket (right_of_ket r)
    | Lexer.WORD bs    -> WORD bs :: left_of_ket r
    | Lexer.ROUND_KET
    | Lexer.SQUARE_KET
    | Lexer.FLOWER_KET -> []
and right_of_ket xl =
  match xl with
  | [] -> raise (MISS_MATCH "right_of_ket")
  | h :: r ->
    match h with
    | Lexer.ROUND_BRA
    | Lexer.SQUARE_BRA
    | Lexer.FLOWER_BRA -> right_of_ket (right_of_ket r)
    | Lexer.WORD bs    -> right_of_ket r
    | Lexer.ROUND_KET
    | Lexer.SQUARE_KET
    | Lexer.FLOWER_KET -> r

let rec print_sexp_list xl =
  match xl with
  | [] -> ()
  | x :: r ->
    print_sexp x;
    print_sexp_list r;
and print_sexp x =
  match x with
  | ROUND  xl -> print_string "( "; print_sexp_list xl; print_string ") "
  | SQUARE xl -> print_string "[ "; print_sexp_list xl; print_string "] "
  | FLOWER xl -> print_string "{ "; print_sexp_list xl; print_string "} "
  | WORD bs -> print_string bs; print_string " "
