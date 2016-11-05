(*
 * dispatching
   | jo      | data    | double data |
   |---------+---------+-------------|
   | compose | bs/deep | cover       |
   | cut     |         | unify       |
 *)

open List ;;
open Array ;;

type name = string ;;

type arrow = jo list * jo list
and level = int
and data =
  | DATA_VAR    of id * level
  | DATA_CONS   of name * data list
  | DATA_ARROW  of arrow
  | DATA_LAMBDA of typ * arrow list
  | DATA_BIND   of data * sub_data
  | DATA_TRUNK  of typ * trunky ref * int
and id = (name * int * ls) ref
and ls = (level * data) list
and sub_data = data
and trunky =
  | TRUNKY_TODO of arrow list * data list
  | TRUNKY_DONE of data list
and typ =
  | TYP_ARROW   of jo list * jo list
  | TYP_JOJO    of jo list
and jo =
  | JO_VAR      of id * int
  | JO_CALL     of name
  | JO_ARROW    of jo list * jo list
  | JO_LAMBDA   of typ * arrow list
  | JO_APPLY
  | JO_EX_BIND  of jo * jo list
  | JO_IM_BIND  of jo * jo list
;;

type pre_arrow = pre_jo list * pre_jo list
and pre_jo =
  | PRE_VAR      of name
  | PRE_CALL     of name
  | PRE_ARROW    of pre_jo list * pre_jo list
  | PRE_LAMBDA   of pre_typ * pre_arrow list
  | PRE_APPLY
  | PRE_EX_BIND  of pre_jo * pre_jo list
  | PRE_IM_BIND  of pre_jo * pre_jo list
and pre_typ =
  | PRE_TYP_ARROW   of pre_jo list * pre_jo list
  | PRE_TYP_JOJO    of pre_jo list
;;

type meaning =
  | MEANING_TYP_CONS  of pre_typ * name * name list
  | MEANING_DATA_CONS of pre_typ * name * name
  | MEANING_JOJO      of pre_typ * pre_jo list
  | MEANING_FUNCTION  of pre_typ * pre_arrow list
;;

let ns = ref [] ;;
let ds = ref [] ;;
let bs = ref [] ;;
let rs = ref [] ;;
let gs = ref [] ;;
(* (name * meaning) list
 * data list
 * (id * ls) list
 * (counter * explainer * ender * jojo) list
 * (counter * explainer * ender * ((data list) * (data list))) list
 *)
