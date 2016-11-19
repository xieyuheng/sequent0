
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

type dsp = data
let ds : dsp stack = ref []

type bsp = id * ls
let bs : bsp stack = ref []

type counter = int
type explainer = unit -> unit
type ender = unit -> unit

type rsp = counter * explainer * ender * jo list
let rs : rsp stack = ref []

type gsp = counter * explainer * ender * ((data list) * (data list))
let gs : gsp stack = ref []
