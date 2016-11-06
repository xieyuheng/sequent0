(* dispatching
   | jo      | data    | double data |
   |---------+---------+-------------|
   | compose | bs/deep | cover       |
   | cut     |         | unify       |
 *)

type name = bytes

type arrow = {sc: jo list; ac: jo list}
and lambda = {t: typ; al: arrow list}
and var = {id: id ; level: int}
and data =
  | DATA_VAR      of var
  | DATA_CONS     of {n: name; dl: data list}
  | DATA_ARROW    of arrow
  | DATA_LAMBDA   of lambda
  | DATA_BIND     of {d: data; sd: data}
  | DATA_TRUNK    of {t: typ; tr: trunky ref; i: int}
and id = (name * int * ls) ref
and lsp = {l: int; d: data}
and ls = lsp list
and trunky =
  | TRUNKY_TODO   of {al: arrow list; dl: data list}
  | TRUNKY_DONE   of data list
and typ =
  | TYP_ARROW     of arrow
  | TYP_JOJO      of jo list
and jo =
  | JO_VAR        of var
  | JO_CALL       of name
  | JO_ARROW      of arrow
  | JO_LAMBDA     of lambda
  | JO_APPLY
  | JO_EX_BIND    of {v: jo; jj: jo list}
  | JO_IM_BIND    of {v: jo; jj: jo list}

type pre_arrow = {sc: pre_jo list; ac: pre_jo list}
and pre_lambda = {t: pre_typ; al: pre_arrow list}
and pre_var = name
and pre_jo =
  | PRE_VAR       of pre_var
  | PRE_CALL      of name
  | PRE_ARROW     of pre_arrow
  | PRE_LAMBDA    of pre_lambda
  | PRE_APPLY
  | PRE_EX_BIND   of {v: pre_jo; jj: pre_jo list}
  | PRE_IM_BIND   of {v: pre_jo; jj: pre_jo list}
and pre_typ =
  | PRE_TYP_ARROW of arrow
  | PRE_TYP_JOJO  of pre_jo list

type meaning =
  | MEANING_TYP_CONS  of {t: pre_typ; n: name; nl: name list}
  | MEANING_DATA_CONS of {t: pre_typ; n: name; nt: name}
  | MEANING_JOJO      of {t: pre_typ; jj: pre_jo list}
  | MEANING_FUNCTION  of {t: pre_typ; al: pre_arrow list}

type nsp = name * meaning
type dsp = data
type bsp = id * ls

type counter = int
type explainer = unit -> unit
type ender = unit -> unit
type rsp = counter * explainer * ender * jo list
type gsp = counter * explainer * ender * ((data list) * (data list))

type 'a stack = 'a list ref

let ns : nsp stack = ref []
let ds : dsp stack = ref []
let bs : bsp stack = ref []
let rs : rsp stack = ref []
let gs : gsp stack = ref []

(* val push : 'a stack -> 'a -> unit *)
let push s v =
  s := v :: !s;

(* val put : 'a stack -> 'a list -> unit *)
let put s l =
  s := l @ !s;

(* val pop : 'a stack -> 'a *)
let pop s =
  match !s with
  | [] -> ><><><
  | h :: r ->
    s := r;
    h

(* val tos : 'a stack -> 'a *)
let tos s =
  match !s with
  | [] -> ><><><
  | h :: r ->
    h

(* val fetch : 'a stack -> int -> 'a list *)
let fetch s n =
  match !s with
  | [] -> ><><><
  | h :: r -> ><><><
