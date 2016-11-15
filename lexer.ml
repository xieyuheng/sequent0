type lex =
  | ROUND_BRA
  | ROUND_KET
  | SQUARE_BRA
  | SQUARE_KET
  | FLOWER_BRA
  | FLOWER_KET
  | WORD of bytes

let rec bytes_of_char_list__help str i cl =
  match cl with
  | [] -> str
  | c :: r
    -> (Bytes.set str i c);
       bytes_of_char_list__help str (i + 1) r

let bytes_of_char_list cl =
  (bytes_of_char_list__help
    (Bytes.make (List.length cl) ' ')
    0
    cl)

let rec lexer_word cl cs =
  match Stream.peek cs with
  | None
  | Some '(' | Some ')'
  | Some '[' | Some ']'
  | Some '{' | Some '}'
  | Some ' ' | Some '\n' | Some '\t'
    -> bytes_of_char_list (List.rev cl), cs
  | Some c
    -> Stream.junk cs; (lexer_word (c :: cl) cs)

let rec lexer cs =
  match Stream.peek cs with
  | None     -> []
  | Some '(' -> Stream.junk cs; ROUND_BRA  :: lexer cs
  | Some ')' -> Stream.junk cs; ROUND_KET  :: lexer cs
  | Some '[' -> Stream.junk cs; SQUARE_BRA :: lexer cs
  | Some ']' -> Stream.junk cs; SQUARE_KET :: lexer cs
  | Some '{' -> Stream.junk cs; FLOWER_BRA :: lexer cs
  | Some '}' -> Stream.junk cs; FLOWER_KET :: lexer cs
  | Some ' ' | Some '\n' | Some '\t'
    -> Stream.junk cs; lexer cs
  | Some c
    -> Stream.junk cs;
       match (lexer_word [c] cs) with
       | bs, cs1 -> (WORD bs) :: (lexer cs1)

let print_lex x =
  match x with
  | ROUND_BRA  -> print_string "( "
  | ROUND_KET  -> print_string ") "
  | SQUARE_BRA -> print_string "[ "
  | SQUARE_KET -> print_string "] "
  | FLOWER_BRA -> print_string "{ "
  | FLOWER_KET -> print_string "} "
  | WORD bs    -> print_string bs; print_string " "

let rec print_lex_list xl =
  match xl with
  | [] -> ()
  | x :: r ->
    print_lex x;
    print_lex_list r;
