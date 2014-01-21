
open AST_L1; 

exception TypeError of string 

datatype type_expr =
         TEint
       | TEunit
       | TEbool

fun type_expr_to_string TEint = "int" 
  | type_expr_to_string TEunit = "unit" 
  | type_expr_to_string TEbool = "bool" 

fun type_error s = raise (TypeError s) 

(* 
   If ccs e = (t, e') then e has type t (an exception is raised in case of 
   ill-typed terms).  expression e' is e annotated with type informaton. 
   For L1 e = e'. Later versions will be more interesting. 
*) 
fun css (Boolean b) = (TEbool, Boolean b)  
  | css (Integer n) = 
    (* 
      In vrm.0, constants must be in range –128 to +127  in order to fit into one (signed) byte. 
      However, the negation symbol "-" is a part of the Slang language, so any "naked" integer 
      constant will be between 0 and 127. 
    *) 
    if  0 <= n andalso n <= 127 
    then (TEint, Integer n)  
    else type_error("vrm.0 target requires all integer constants to be in range 0 to 127") 
  | css (UnaryOp (Neg,Integer n)) = 
    if  0 <= n andalso n <= 128
    then (TEint, UnaryOp (Neg,Integer n))  
    else type_error("vrm.0 target requires all negated integer constants to be in range 0 to 128") 
  | css (UnaryOp (Neg,e)) = 
     let val (t, e') = css e
     in 
        if t = TEint 
        then (t, UnaryOp (Neg,e')) 
        else type_error ("negative applied to expression of type " ^ (type_expr_to_string t))
     end 
  | css (UnaryOp (Not,e)) = 
     let val (t, e') = css e
     in 
        if t = TEbool 
        then (t, UnaryOp (Not,e')) 
        else type_error ("negation applied to expression of type " ^ (type_expr_to_string t))
     end 
  | css (Op (e1,GTEQ,e2)) = 
     let val (t1, e1') = css e1 
         val (t2, e2') = css e2
     in 
        if t1 = TEint 
        then if t2 = TEint 
             then (TEbool, Op (e1',GTEQ,e2'))
             else type_error ("second expression of >= has type " ^ (type_expr_to_string t1))
        else type_error ("first expression of >= has type " ^ (type_expr_to_string t2))
     end 
  | css (Op (e1,opr,e2)) =
     let val (t1, e1') = css e1 
         val (t2, e2') = css e2
     in 
        if t1 = TEint 
        then if t2 = TEint 
             then (TEint, Op (e1', opr, e2'))
             else type_error ("second expression of " ^ (op_to_string opr) ^ " has type " ^ (type_expr_to_string t2))
        else type_error ("first expression of " ^ (op_to_string opr) ^ " has type " ^ (type_expr_to_string t1))
     end 
  | css (If (e1,e2,e3)) = 
     let val (t1, e1') = css e1 
         val (t2, e2') = css e2
         val (t3, e3') = css e3
     in 
        if t1 = TEbool 
        then if t2 = t3 
             then (t2, If (e1', e2', e3'))
             else type_error ("then branch of type " ^ (type_expr_to_string t3) ^ " while else branch of type " ^ (type_expr_to_string t3))
        else type_error ("condition of 'if' has type " ^ (type_expr_to_string t1))
     end 
  | css (Deref l) = (TEint, Deref l) 
  | css (Assign (l, e)) = 
     let val (t, e') = css e 
     in 
        if t = TEint (* L1 allows only integer assignment *) 
        then (TEunit, Assign (l, e'))
        else type_error ("right-hand side of assignment has type " ^ (type_expr_to_string t))
     end 
  | css (Skip) = (TEunit, Skip) 
  | css (Seq (e1,e2)) = 
     let val (t1, e1') = css e1 
         val (t2, e2') = css e2
     in 
        if t1 = TEunit
        then (t2, Seq(e1', e2'))
        else type_error ("sequence expression of has type " ^ (type_expr_to_string t1))
     end 
  | css (While (e1,e2)) = 
     let val (t1, e1') = css e1 
         val (t2, e2') = css e2
     in 
        if t1 = TEbool
        then if t2 = TEunit
             then (TEunit, While(e1', e2'))
             else type_error ("body of while has type " ^ (type_expr_to_string t2))
        else type_error ("condition of 'while' has type " ^ (type_expr_to_string t1))
     end 
  | css (Print e) = 
     let val (t, e') = css e 
     in if t = TEint 
        then (TEunit, Print e') 
        else type_error ("argument of print must be of type int, found " ^ (type_expr_to_string t))
     end 

fun check_types e = 
    let val (_, result) = css e 
        val _ = if !Global.verbose
                then (print "\n\n Program is type correct!"; 
                      print "\nL1 AST = \n"; 
                      pp_expr result)
                else () 
    in result end 

