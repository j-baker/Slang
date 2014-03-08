type oper = AST_L3.oper
type unary_oper = AST_L3.unary_oper
type var = string 
type label = string 

(* This language is very low-level, but still could easily 
   be compiled further to either a register-oriented machine 
   or a stack-oriented machine.  
*) 

(* the "var" component is really no longer required, 
   but retained for verbose mode and debugging 
*)

type index = int
type range = int
type value_loc = index * range  
 
datatype ir2_var_kind = 
       IR2_ArgVar of var * value_loc        
     | IR2_EnvVar of var * value_loc  * int 
     | IR2_LocalVar of var * value_loc      

datatype ir2_expr =
          IR2_Comment of string 
        | IR2_Label of label 
	| IR2_Skip
        | IR2_Halt  
	| IR2_Var of ir2_var_kind 
	| IR2_KnownFun of var 
	| IR2_Integer of int
	| IR2_Boolean of bool
	| IR2_UnaryOp of unary_oper * (ir2_expr list) 
	| IR2_Op of (ir2_expr list) * oper * (ir2_expr list)
        | IR2_Assign of (ir2_expr list) * (ir2_expr list) 
        | IR2_StoreLocal of (ir2_expr list) * int 
	| IR2_Deref of (ir2_expr list) 
	| IR2_Ref of (ir2_expr list) 
	| IR2_Discard of (ir2_expr list) 
	| IR2_App of (ir2_expr list) * (ir2_expr list list) 
        | IR2_Jump of label 
        | IR2_Fjump of (ir2_expr list) * label 
        | IR2_Closure of var * (ir2_var_kind list) 

(* IR2_FunLabel(f, number_of_locals, number_of_args, body) *) 
datatype ir2_function = IR2_FunLabel of label * int * int * (ir2_expr list)   

type program = (ir2_expr list) * (ir2_function list)


(* pretty priting using Mosml's PP module *) 

val unary_to_string = AST_L3.unary_to_string 
val op_to_string    = AST_L3.op_to_string 

fun print_list sep printer pps []  = ()
  | print_list sep printer pps [x] = printer pps x
  | print_list sep printer pps (x::rest) = 
      (printer pps x; PP.add_string pps sep; print_list sep printer pps rest)

fun ppvk pps (IR2_ArgVar (v, (ind, n)))    = 
       PP.add_string pps ("ARG[" ^ v ^ ", " ^ (Int.toString ind) ^ ", " ^ (Int.toString n) ^ "]")
  | ppvk pps (IR2_LocalVar (v, (ind, n)))  = 
       PP.add_string pps ("LOCAL[" ^ v ^ ", " ^ (Int.toString ind) ^ ", " ^ (Int.toString n) ^ "]")
  | ppvk pps (IR2_EnvVar (v, (ind, n), i)) = 
       PP.add_string pps ("ENV[" ^ v ^ ", " ^ (Int.toString ind) ^  ", " ^ (Int.toString n) ^ ", " ^ (Int.toString i) ^ "]")

val ppvkl = print_list ", " ppvk 

fun ppe pps (IR2_Comment s) = PP.add_string pps ("% " ^ s) 
  | ppe pps (IR2_Label l) = PP.add_string pps ("LABEL(" ^ l ^ ")") 
  | ppe pps (IR2_Skip) = PP.add_string pps "skip"	
  | ppe pps (IR2_Halt) = PP.add_string pps "halt"	
  | ppe pps (IR2_KnownFun v) = PP.add_string pps ("KNOWN(" ^ v ^ ")")
  | ppe pps (IR2_Var vk)     = ppvk pps vk 
  | ppe pps (IR2_Integer n) = PP.add_string pps (Int.toString n)
  | ppe pps (IR2_Boolean b) = PP.add_string pps (Bool.toString b)
  | ppe pps (IR2_UnaryOp (uopr,el)) = (PP.add_string pps (unary_to_string uopr); ppes pps el)
  | ppe pps (IR2_Op (el1,opr,el2)) = 
      (PP.add_string pps "("; 
       ppes pps el1; 
       PP.add_string pps (" " ^ (op_to_string opr) ^ " "); 
       ppes pps el2;  
       PP.add_string pps ")")
  | ppe pps (IR2_StoreLocal (el, n)) =  
       (PP.begin_block pps PP.CONSISTENT 0; 
        PP.add_string pps ("LOCAL[" ^ (Int.toString n) ^ "] := "); 
        PP.add_break pps (0, 0); 
        ppes pps el; 
        PP.end_block pps)
  | ppe pps (IR2_Assign (el1, el2)) =  (ppes pps el1;  PP.add_string pps " := "; ppes pps el2)
  | ppe pps (IR2_Deref el) = (PP.add_string pps "DEREF "; ppes pps el)
  | ppe pps (IR2_Discard el) = (PP.add_string pps "DISCARD "; ppes pps el)
  | ppe pps (IR2_Ref el) = (PP.add_string pps "REF "; ppes pps el)
  | ppe pps (IR2_App (el, ell)) = (ppes pps el; ppargs pps ell) 
  | ppe pps (IR2_Jump l) = PP.add_string pps ("JUMP " ^ l) 
  | ppe pps (IR2_Fjump (el, l)) = (PP.add_string pps "FJUMP "; ppes pps el; PP.add_string pps (" " ^ l))
  | ppe pps (IR2_Closure (v, vkl)) =
     (PP.add_string pps ("CLOSURE(" ^ v ^ ", ["); 
      ppvkl pps vkl;  
      PP.add_string pps "]") 		 

and ppes pps [e] =  ppe pps e 
  | ppes pps l = 
    (PP.add_string pps "{"; 
     pp_seq pps l; 
     PP.add_string pps "}" 
    ) 
and ppargs pps l =  (PP.add_string pps " ("; (print_list ", " ppes) pps l ; PP.add_string pps ")")

and pp_seq pps [] =  () 
  | pp_seq pps [e] =  ppe pps e 
  | pp_seq pps (e :: rest)=  
       (PP.begin_block pps PP.CONSISTENT 0; 
        ppe pps e; 
        PP.add_string pps "; "; 
        PP.add_break pps (0, 0); 	
	pp_seq pps rest;
        PP.end_block pps)

fun ppf pps (IR2_FunLabel (f,num_locals, num_args, el)) = 
    (PP.add_string pps 
       ("FUNLABEL(" ^ f ^ ", " 
                   ^ (Int.toString num_locals) ^ ", " 
                   ^ (Int.toString num_args) ^ ")"); 
     ppes pps el)

fun ppfs pps [] =  () 
  | ppfs pps (f::rest) =  
       (PP.begin_block pps PP.CONSISTENT 0; 
        ppf pps f; 
        PP.add_string pps "; "; 
        PP.add_break pps (0, 0); 	
	ppfs pps rest;
        PP.end_block pps)

fun pp_prog pps (el, fl) = (pp_seq pps el; ppfs pps fl)

val pp_ir2_program = Library.mk_pp pp_prog 
