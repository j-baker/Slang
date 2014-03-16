open AST_L1; 
open AST_IR1;

fun contains _ [] = false
  | contains x (y::ys) = x=y orelse contains x ys

fun find_all_assignments (UnaryOp (_, e)) = find_all_assignments e
  | find_all_assignments (Op(e1, _, e2)) = (find_all_assignments e1) @ (find_all_assignments e2)
  | find_all_assignments (Assign(l, e)) = ((l, e)::(find_all_assignments e))
  | find_all_assignments (If(e1, e2, e3)) = (find_all_assignments e1)@(find_all_assignments e2)@(find_all_assignments e3)
  | find_all_assignments (Seq(e1, e2)) = (find_all_assignments e1)@(find_all_assignments e2)
  | find_all_assignments (While(e1, e2)) = (find_all_assignments e1)@(find_all_assignments e2)
  | find_all_assignments (Print e) = find_all_assignments e
  | find_all_assignments _ = []

fun remove_assignments to_remove (UnaryOp (oper,e)) =
        UnaryOp (oper,
		 remove_assignments to_remove e)
  | remove_assignments to_remove (Op(e1, oper, e2)) =
        Op(remove_assignments to_remove e1,
	   oper,
	   remove_assignments to_remove e2)
  | remove_assignments to_remove (If(e1, e2, e3)) =
        If(remove_assignments to_remove e1,
	   remove_assignments to_remove e2,
	   remove_assignments to_remove e3)
  | remove_assignments to_remove (Seq(e1, e2)) =
        Seq(remove_assignments to_remove e1, remove_assignments to_remove e2)
  | remove_assignments to_remove (While(e1, e2)) =
        While(remove_assignments to_remove e1, remove_assignments to_remove e2)
  | remove_assignments to_remove (Print e) =
        Print (remove_assignments to_remove e)
  | remove_assignments to_remove (Assign(l, e)) =
        if to_remove = Assign(l, e) then Skip else Assign(l, remove_assignments to_remove e)
  | remove_assignments _ x = x

datatype io_log = Read of loc | Write of loc

(* Essentially we are fine with all but write after read. *)

fun not_allowed_write _ [] = true
  | not_allowed_write loc (Write(x)::log) = not(loc = x) andalso not_allowed_write loc log
  | not_allowed_write loc (Read(x)::log) = not_allowed_write loc log

fun acceptable_assignment _ [] = true
  | acceptable_assignment loc (Read(x)::log) = if loc = x then (not_allowed_write loc log) else acceptable_assignment loc log
  | acceptable_assignment loc (Write(x)::log) = acceptable_assignment loc log

(* Todo: Optimisation does not occur for ifs. *)
fun safe_assignments loop =
    let val log = ref []
        fun find_safe (Assign(l, e)) = (find_safe e; (log:=(Write(l)::(!log))))
	  | find_safe (Deref(l)) = (log:= (Read(l)::(!log)))
	  | find_safe (UnaryOp(_, e)) = find_safe e
	  | find_safe (Op(e1,_,e2))  = ((find_safe e1);(find_safe e2))
	  | find_safe (If(e1, e2, e3)) = ((find_safe e1);(find_safe e2);(find_safe e3))
	  | find_safe (Seq(e1, e2)) = ((find_safe e1);(find_safe e2))
	  | find_safe (While(e1, e2)) = ((find_safe e1);(find_safe e2))
	  | find_safe (Print e) = find_safe e
	  | find_safe _ = ()
    in
	find_safe loop;
	List.filter (fn (loc, _) => acceptable_assignment loc (rev (!log))) (find_all_assignments loop)
    end

fun remove_constant_exprs (e1, e2) =
    let val constant_assignment = safe_assignments (While(e1, e2))
   in
	case constant_assignment of
            (x::_) =>
		remove_constant_exprs (e1, remove_assignments (Assign(x)) e2)
          | [] => e2
    end

fun subtract [] [] = []
  | subtract [] _ = []
  | subtract (x::xs) ys = if (List.find (fn y => y=x) ys) = NONE then x::(subtract xs ys) else (subtract xs ys)

fun extract_constant_exprs (e1, e2) =
    let val constant_assignment = safe_assignments (While(e1, e2))
    in
	case constant_assignment of
            (x::_) =>
	    (Assign x)::(extract_constant_exprs (e1, remove_assignments (Assign(x)) e2))
          | [] => []
    end

fun assignment_list_to_seq [] = Skip
  | assignment_list_to_seq (x::xs) = Seq(x, assignment_list_to_seq xs)

fun optimise_whiles (UnaryOp (oper,e)) = UnaryOp (oper, optimise_whiles e)
  | optimise_whiles (Op(e1, oper, e2)) = Op(optimise_whiles e1, oper, optimise_whiles e2)
  | optimise_whiles (If(e1, e2, e3)) = If(optimise_whiles e1, optimise_whiles e2, optimise_whiles e3)
  | optimise_whiles (Seq(e1, e2)) = Seq(optimise_whiles e1, optimise_whiles e2)
  | optimise_whiles (While(e1, e2)) = 
	Seq(assignment_list_to_seq (extract_constant_exprs (e1, e2)), While(optimise_whiles e1, optimise_whiles(remove_constant_exprs (e1, e2))))
  | optimise_whiles (Print e) = Print (optimise_whiles e)
  | optimise_whiles (Assign(l, e)) = Assign(l, optimise_whiles e)
  | optimise_whiles x = x

fun optimise e = optimise_whiles e
