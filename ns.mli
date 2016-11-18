(* dep :: stack parser *)

(* 'compiler' does not output files
   it simply binds 'meaning' to name in name_stack (in memory)
 *)

type name = bytes

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
val ns : nsp Stack.stack (* let ns = ref [] *)
val define : sexp -> unit (* effect ns *)
val uni_copy_pre_jojo : pre_jojo -> scope -> (jojo -> scope)
