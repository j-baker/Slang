

datatype targets = VRM | VSM        

val verbose = ref false   (* default *) 

val target = ref VSM      (* default *) 

val next_label_count = Library.new_counter(); 
fun new_label () = "_L" ^ (Int.toString (next_label_count ())) 

val next_loc_count = Library.new_counter(); 
fun new_loc () = "_X" ^ (Int.toString (next_loc_count ())) 

val freshVar =
	let val counter = ref 0 in
	fn (v) => (
		counter := !counter + 1;
		("_" ^ (Int.toString (!counter)) ^ "_" ^ v )
	)
	end





