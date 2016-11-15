(* dispatching
   | jo      | data    | double data |
   |---------+---------+-------------|
   | compose | bs/deep | cover       |
   | cut     |         | unify       |
 *)

type name = bytes

type arrow = jo list * jo list
and lambda = typ * arrow list
and level = int
and data =
  | DATA_VAR      of id * level
  | DATA_CONS     of name * data list
  | DATA_ARROW    of arrow
  | DATA_LAMBDA   of lambda
  | DATA_BIND     of data * data
  | DATA_TRUNK    of typ * trunky ref * int
and id = (name * int * ls) ref
and ls = (level * data) list
and trunky =
  | TRUNKY_TODO   of arrow list * data list
  | TRUNKY_DONE   of data list
and typ =
  | TYP_ARROW     of arrow
  | TYP_JOJO      of jo list
and jo =
  | JO_VAR        of id * int
  | JO_CALL       of name
  | JO_ARROW      of arrow
  | JO_LAMBDA     of lambda
  | JO_APPLY
  | JO_EX_BIND    of jo * jo list
  | JO_IM_BIND    of jo * jo list

type pre_arrow = pre_jo list * pre_jo list
and pre_lambda = pre_typ * pre_arrow list
and pre_jo =
  | PRE_VAR       of name
  | PRE_CALL      of name
  | PRE_ARROW     of pre_arrow
  | PRE_LAMBDA    of pre_lambda
  | PRE_APPLY
  | PRE_EX_BIND   of pre_jo * pre_jo list
  | PRE_IM_BIND   of pre_jo * pre_jo list
and pre_typ =
  | PRE_TYP_ARROW of pre_arrow
  | PRE_TYP_JOJO  of pre_jo list

type meaning =
  | MEANING_TYP_CONS  of pre_typ * name * name list
  | MEANING_DATA_CONS of pre_typ * name * name
  | MEANING_JOJO      of pre_typ * pre_jo list
  | MEANING_FUNCTION  of pre_typ * pre_arrow list

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

(* (\* val put : 'a stack -> 'a list -> unit *\)
 * let put s l =
 *   s := l @ !s;
 *
 * (\* val pop : 'a stack -> 'a *\)
 * let pop s =
 *   match !s with
 *   | [] -> ><><><
 *   | h :: r ->
 *     s := r;
 *     h
 *
 * (\* val tos : 'a stack -> 'a *\)
 * let tos s =
 *   match !s with
 *   | [] -> ><><><
 *   | h :: r ->
 *     h
 *
 * (\* val fetch : 'a stack -> int -> 'a list *\)
 * let fetch s n =
 *   match !s with
 *   | [] -> ><><><
 *   | h :: r -> ><><>< *)
