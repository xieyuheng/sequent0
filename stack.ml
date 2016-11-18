(* indep *)

type 'a stack = 'a list ref

exception STACK_ERROR of bytes

let push s v =
  s := v :: !s

let push_list s l =
  s := l @ !s

let pop s =
  match !s with
  | [] -> raise (STACK_ERROR "pop meet empty stack")
  | h :: r ->
    s := r; h

let tos s =
  match !s with
  | [] -> raise (STACK_ERROR "tos meet empty stack")
  | h :: r -> h

let drop s =
  match !s with
  | [] -> raise (STACK_ERROR "drop meet empty stack")
  | h :: r ->
    s := r

let rec pop_list s n =
  match pop_list__help !s n with
  | rl, nl ->
    s := rl; nl
and pop_list__help l n =
  match l, n with
  | l, 0 -> (l, [])
  | [], n -> raise (STACK_ERROR "pop_list meet empty stack")
  | h :: r, n ->
    match pop_list__help r (n-1) with
    | rl, nl -> (rl, h :: nl)
