
type var = AST_L3.var 
type oper = AST_L3.oper
type unary_oper = AST_L3.unary_oper

datatype var_kind = 
       IR1_ArgVar of var 
     | IR1_LetVar of var 
     | IR1_EnvVar of var * int 

datatype expr = 
         IR1_Skip
       | IR1_Var of var_kind 
       | IR1_KnownFun of var
       | IR1_Integer of int
       | IR1_Boolean of bool
       | IR1_UnaryOp of unary_oper * expr
       | IR1_Op of expr * oper * expr
       | IR1_Assign of expr * expr 
       | IR1_Deref of expr              
       | IR1_Ref of expr 
       | IR1_Seq of expr * expr 
       | IR1_If of expr * expr * expr
       | IR1_While of expr * expr
       | IR1_App of expr * expr list
       | IR1_Closure of var * (var_kind list) 
       | IR1_Let of var * expr * expr  

datatype ir1_function = IR1_Letrec of var * (var list) * expr 

type program = expr * (ir1_function list) 

(* pretty printing using Mosml's PP module *) 

fun ppvars pps [] = ()
  | ppvars pps [v] = PP.add_string pps v
  | ppvars pps (v::rest) = (PP.add_string pps v; PP.add_string pps ", "; ppvars pps rest)

fun ppvk pps (IR1_ArgVar v)      = PP.add_string pps v
  | ppvk pps (IR1_LetVar v)      = PP.add_string pps v
  | ppvk pps (IR1_EnvVar (v, n)) = PP.add_string pps (v ^ "[" ^ (Int.toString n) ^ "]")

fun ppvkl pps [] = ()
  | ppvkl pps [v] = ppvk pps v 
  | ppvkl pps (v::vkl) = (ppvk pps v; PP.add_string pps ", "; ppvkl pps vkl)

fun ppe pps (IR1_Skip)      = PP.add_string pps "skip"	
  | ppe pps (IR1_Var vk)    = ppvk pps vk 
  | ppe pps (IR1_KnownFun v) = PP.add_string pps ("KNOWN(" ^ v ^ ")")
  | ppe pps (IR1_Integer n) = PP.add_string pps (Int.toString n)
  | ppe pps (IR1_Boolean b) = PP.add_string pps (Bool.toString b)
  | ppe pps (IR1_UnaryOp (uopr,e)) = 
      (
       PP.add_string pps "("; 
       PP.add_string pps (AST_L3.unary_to_string uopr); 
       ppe pps e;  
       PP.add_string pps ")"
      )
  | ppe pps (IR1_Op (e1,opr,e2)) = 
      (
       PP.add_string pps "("; 
       ppe pps e1; 
       PP.add_string pps (" " ^ (AST_L3.op_to_string opr) ^ " "); 
       ppe pps e2;  
       PP.add_string pps ")"
      )
  | ppe pps (IR1_Assign (e1, e2)) =  
       (
        ppe pps e1; 
        PP.add_string pps " := "; 
        ppe pps e2
       )
  | ppe pps (IR1_Deref e) = 
      (
       PP.add_string pps "!"; 
       ppe pps e
      )
  | ppe pps (IR1_Ref e) = 
      (
       PP.add_string pps "ref "; 
       ppe pps e
      )
  | ppe pps (IR1_Seq (e1,e2)) =  
       (PP.begin_block pps PP.CONSISTENT 0; 
        PP.add_string pps "("; 		 
        ppe pps e1; 
        PP.add_string pps "; "; 
        PP.add_break pps (0, 0); 	
	ppe pps e2;
        PP.add_string pps ")"; 		 
        PP.end_block pps)
  | ppe pps (IR1_If (e1, e2, e3)) = 
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
  | ppe pps (IR1_While (e1,e2)) = 
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
  | ppe pps (IR1_App (e, es)) =
    (ppe pps e; 
    PP.add_string pps " (";
    ppes pps es;
    PP.add_string pps ")")
  | ppe pps (IR1_Closure (v, vkl)) =
     (PP.add_string pps ("CLOSURE(" ^ v ^ ", ["); 
      ppvkl pps vkl;  
      PP.add_string pps "])") 		 
  | ppe pps (IR1_Let (v, e1, e2)) =
    (PP.begin_block pps PP.CONSISTENT 0;
    PP.add_string pps "let ";
    PP.add_string pps v;
    PP.add_string pps " = ";
       PP.add_break pps (0, 3); 	
       ppe pps e1;
    PP.add_break pps (0, 0); 	
    PP.add_string pps " in ";
       PP.add_break pps (0, 3); 	
       ppe pps e2;
    PP.add_break pps (0, 0); 	
    PP.add_string pps " end"; 
    PP.end_block pps)


and ppes pps [] = ()
  | ppes pps [e] = ppe pps e
  | ppes pps (e::es) =
    (ppe pps e; PP.add_string pps ", "; ppes pps es)


fun ppf pps (IR1_Letrec (v, vts, body)) =
    (PP.begin_block pps PP.CONSISTENT 0;
    PP.add_string pps "fun ";
    PP.add_string pps v;
    PP.add_string pps " (";
    ppvars pps vts;
    PP.add_string pps " ) = ";
       PP.add_break pps (0, 3); 	
       ppe pps body;
    PP.end_block pps)

fun ppfs pps [] =  () 
  | ppfs pps (f::rest) =  
       (PP.begin_block pps PP.CONSISTENT 0; 
        ppf pps f; 
        PP.add_string pps "; "; 
        PP.add_break pps (0, 0); 	
	ppfs pps rest;
        PP.end_block pps)

fun ppp pps (e, fl) = 
       (PP.begin_block pps PP.CONSISTENT 0; 
        ppe pps e; 
        PP.add_break pps (0, 0); 	
	ppfs pps fl;
        PP.end_block pps)

val pp_program  = Library.mk_pp ppp 



