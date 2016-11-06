let example_in_channel = open_in "example.scm" ;;
let example_cs = Stream.of_channel example_in_channel ;;

type lex =
  | ROUND_BRA
  | ROUND_KET
  | SQUARE_BRA
  | SQUARE_KET
  | FLOWER_BRA
  | FLOWER_KET
  | WORD of bytes
;;

let rec bytes_of_char_list__help str i cl =
  match cl with
  | [] -> str
  | c :: r
    -> (Bytes.set str i c);
       bytes_of_char_list__help str (i + 1) r
;;
(bytes_of_char_list__help : bytes -> int -> char list -> bytes) ;;

let bytes_of_char_list cl =
  (bytes_of_char_list__help
    (Bytes.make (List.length cl) ' ')
    0
    cl)
;;
(bytes_of_char_list : char list -> bytes) ;;

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
;;
(lexer_word : char list -> char Stream.t -> (bytes * char Stream.t)) ;;

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
;;
(lexer : char Stream.t -> lex list) ;;

lexer example_cs ;;
