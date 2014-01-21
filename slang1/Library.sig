

exception Missing of string; 
type 'a env; 
val lookup : (string * 'a) env * string -> 'a; 
val update : (string * 'a) env * string * 'a -> (string * 'a) env; 
val empty_env : 'a env ; 

exception InternalError of string ; 

val internal_error : string -> 'a ; 

val n_spaces     : int -> string; 

val new_counter : unit -> (unit -> int) 

val resettable_counter : unit -> (unit -> int) * (unit -> unit) 

val intersects : ''a list * ''a list -> bool

val mk_pp : (ppstream -> 'a -> unit) -> 'a -> unit

