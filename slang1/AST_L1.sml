(*
This is essentially the (typed) L1 lagnuage from Semantics of 
Programming Languages (SPL). The small differences are 

  --- unary operations are added to the language (Neg and Not)
  --- "*" (Mult) and "-" (Subtr) are added as binary operations 
  --- a print expression is added 
*) 
type loc = string

datatype oper = Plus | Mult | Subt | GTEQ

datatype unary_oper = Neg | Not  

datatype expr = 
         Skip
       | Integer of int
       | Boolean of bool
       | UnaryOp of unary_oper * expr
       | Op of expr * oper * expr
       | Assign of loc * expr
       | Deref of loc
       | Seq of expr * expr
       | If of expr * expr * expr
       | While of expr * expr
       | Print of expr


(* pretty printing using Mosml's PP module *) 

fun unary_to_string Neg = "-"
  | unary_to_string Not = "~"

fun op_to_string Plus = "+"
  | op_to_string Mult = "*"
  | op_to_string Subt = "-"
  | op_to_string GTEQ = ">="
                         
fun ppe pps (Integer n) = PP.add_string pps (Int.toString n)
  | ppe pps (Boolean b) = PP.add_string pps (Bool.toString b)
  | ppe pps (UnaryOp (uopr,e)) = 
      (
       PP.add_string pps "("; 
       PP.add_string pps (unary_to_string uopr); 
       ppe pps e;  
       PP.add_string pps ")"
      )
  | ppe pps (Op (e1,opr,e2)) = 
      (
       PP.add_string pps "("; 
       ppe pps e1; 
       PP.add_string pps (" " ^ (op_to_string opr) ^ " "); 
       ppe pps e2;  
       PP.add_string pps ")"
      )
  | ppe pps (If (e1, e2, e3)) = 
      (PP.begin_block pps PP.CONSISTENT 0; 
      PP.add_string pps "if "; 
      ppe pps e1; 
      PP.add_break pps (0, 0); 
      PP.add_string pps " then "; 
      ppe pps e2; 
      PP.add_break pps (0, 0); 
      PP.add_string pps " else "; 
      ppe pps e3;
      PP.end_block pps)
  | ppe pps (Deref l) = PP.add_string pps ("!" ^ l)
  | ppe pps (Assign (l, e)) =  
       (
        PP.add_string pps l; 
        PP.add_string pps " := "; 
	ppe pps e
       )
  | ppe pps (Skip) = PP.add_string pps "skip"	
  | ppe pps (Seq (e1,e2)) =  
       (PP.begin_block pps PP.CONSISTENT 0; 
        PP.add_string pps "("; 		 
        ppe pps e1; 
        PP.add_string pps "; "; 
        PP.add_break pps (0, 0); 	
	ppe pps e2;
        PP.add_string pps ")"; 		 
        PP.end_block pps)
  | ppe pps (While (e1,e2)) = 
      (PP.begin_block pps PP.CONSISTENT 0; 
       PP.add_string pps "while "; 
       ppe pps e1; 
       PP.add_string pps " do "; 
       PP.add_break pps (0, 0); 
       PP.begin_block pps PP.CONSISTENT 3;		
       PP.add_string pps "("; 
       ppe pps e2; 
       PP.add_string pps ")"; 
       PP.end_block pps;
       PP.end_block pps) 
  | ppe pps (Print e) = 
      (
       PP.add_string pps "print("; 
       ppe pps e; 
       PP.add_string pps ")" 
      ) 

val pp_expr = Library.mk_pp ppe 


                   

