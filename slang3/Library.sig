

exception Missing of int; 
type 'a env; 
val lookup : (int * 'a) env * int -> 'a; 
val update : (int * 'a) env * int * 'a -> (int * 'a) env; 
val empty_env : 'a env ; 

exception InternalError of string ; 

val internal_error : string -> 'a ; 

val n_spaces     : int -> string; 

val new_counter : unit -> (unit -> int) 

val resettable_counter : unit -> (unit -> int) * (unit -> unit) 

val intersects : ''a list * ''a list -> bool

val mk_pp : (ppstream -> 'a -> unit) -> 'a -> unit

val mapPair : ('a * 'b -> 'c) -> ('a list * 'b list) -> 'c list

