(* indep *)

type 'a stack = 'a list ref

exception STACK_ERROR of bytes

val push : 'a stack -> 'a -> unit
val push_list : 'a stack -> 'a list -> unit

val pop : 'a stack -> 'a
val tos : 'a stack -> 'a
val drop : 'a stack -> unit
val pop_list : 'a stack -> int -> 'a list
