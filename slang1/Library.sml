structure Library :> Library = struct 

(* Surprise: environments are implemented as trees from FoCS *) 

(**** From Foundations of Computer Science, LECTURE 7 ****)

datatype 'a tree = Lf
                 | Br of 'a * 'a tree * 'a tree;

type 'a env = 'a tree; 

(**** From Foundations of Computer Science, LECTURE 8 ****)

exception Missing of string;

fun lookup (Br ((a,x),t1,t2), b) =
      if      b < a then  lookup(t1, b)
      else if a < b then  lookup(t2, b)
      else x
  | lookup (Lf, b) = raise Missing b;

fun update (Lf, b: string, y) = Br((b,y), Lf, Lf)
  | update (Br((a,x),t1,t2), b, y) =
      if b<a then  Br ((a,x),  update(t1,b,y),  t2)
      else 
      if a<b then  Br ((a,x),  t1,  update(t2,b,y))
      else (*a=b*) Br ((a,y),t1,t2);

val empty_env = Lf 

(* now, a few new things ... *) 

(* construct a string of n spaces .... *) 
fun n_spaces n = 
    let fun aux carry m = if (m = n) then carry else aux (" " ^ carry) (m + 1)
    in aux "" 0 end 

exception InternalError of string 

fun internal_error s = raise (InternalError s) 

(* new_counter : unit -> (unit -> int) 

   let val next = new_counter () 
   ... 
        next()    ( gets 0 ) 
   ... 
        next()    ( gets 1 ) 
   ... 
        next()    ( gets 2 ) 
   ... 

Try to understand this function because we 
will write one like it in a later version
of Slang.  Can r be stack allocated? No! 
A "new r" must be allocated as a part of 
each returned function's closure .... 

*) 
fun new_counter () = 
    let val r = ref 0 
    in let fun next () = let val n = !r val _ = r := 1 + !r in n end 
       in next end 
    end

fun resettable_counter () = 
    let val r = ref 0 
    in let fun next () = let val n = !r val _ = r := 1 + !r in n end 
       and  reset () = r := 0 
       in (next, reset) end 
    end


(* 
   Treat two lists are representing sets. 
   return true only when there is at least 
   one element common to both. 
*) 
fun intersects (set1, set2) = 
    let fun find x = List.exists (fn z => x = z) set2 
    in List.exists find set1 end 



(* make a pretty printer 

   mk_pp : (ppstream -> 'a -> unit) -> 'a -> unit

*) 
fun mk_pp f x = 
    let val ppstrm = PP.mk_ppstream {
                consumer  = fn s => TextIO.output(TextIO.stdOut, s), 
                linewidth = 80,               
                flush     = fn () => TextIO.flushOut TextIO.stdOut}
    in 
        PP.begin_block ppstrm PP.CONSISTENT 0; 
        f ppstrm x ;
        PP.end_block ppstrm; 
        PP.flush_ppstream ppstrm 
    end 


end (* of structure *)     