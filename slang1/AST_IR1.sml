type unary_oper = AST_L1.unary_oper
type oper       = AST_L1.oper
type loc        = AST_L1.loc

datatype ir1_expr = 
         IR1_ESkip 
       | IR1_Integer of int
       | IR1_Boolean of bool
       | IR1_UnaryOp of unary_oper * ir1_expr
       | IR1_Op of ir1_expr * oper * ir1_expr
       | IR1_Deref of loc
       | IR1_EIf of ir1_expr * ir1_expr * ir1_expr

datatype ir1_stm = 
         IR1_Expr of ir1_expr  
       | IR1_Assign of loc * ir1_expr
       | IR1_Seq of ir1_stm list 
       | IR1_SIf of ir1_expr * ir1_stm * ir1_stm
       | IR1_While of ir1_expr * ir1_stm 
       | IR1_Print of ir1_expr 

(* pretty printing with Mosml's PP module *) 

val unary_to_string = AST_L1.unary_to_string 
val op_to_string    = AST_L1.op_to_string 

fun ppe pps (IR1_ESkip) = PP.add_string pps "skip"	
  | ppe pps (IR1_Integer n) = PP.add_string pps (Int.toString n)
  | ppe pps (IR1_Boolean b) = PP.add_string pps (Bool.toString b)
  | ppe pps (IR1_UnaryOp (uopr,e)) = 
      (
       PP.add_string pps "("; 
       PP.add_string pps (unary_to_string uopr); 
       ppe pps e;  
       PP.add_string pps ")"
      )
  | ppe pps (IR1_Op (e1,opr,e2)) = 
      (
       PP.add_string pps "("; 
       ppe pps e1; 
       PP.add_string pps (" " ^ (op_to_string opr) ^ " "); 
       ppe pps e2;  
       PP.add_string pps ")"
      )
  | ppe pps (IR1_Deref l) = PP.add_string pps ("!" ^ l)
  | ppe pps (IR1_EIf (e1, e2, e3)) = 
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

fun pp_stm pps (IR1_Expr e) =  ppe pps e 
  | pp_stm pps (IR1_SIf (e, ir1_stm1, ir1_stm2)) = 
      (PP.begin_block pps PP.CONSISTENT 0; 
      PP.add_string pps "if "; 
      ppe pps e; 
      PP.add_break pps (0, 0); 
      PP.add_string pps " then "; 
      pp_stm pps ir1_stm1; 
      PP.add_break pps (0, 0); 
      PP.add_string pps " else "; 
      pp_stm pps ir1_stm2;
      PP.end_block pps)
  | pp_stm pps (IR1_Assign (l, e)) =  
       (
        PP.add_string pps l; 
        PP.add_string pps " := "; 
	ppe pps e
       )
  | pp_stm pps (IR1_Seq []) = () 
  | pp_stm pps (IR1_Seq [ir1_stm]) = pp_stm pps ir1_stm
  | pp_stm pps (IR1_Seq (ir1_stm :: sl)) = 
       (PP.begin_block pps PP.CONSISTENT 0; 
        PP.add_string pps "("; 		 
        pp_stm pps ir1_stm; 
        PP.add_string pps "; "; 
        PP.add_break pps (0, 0); 	
	pp_stm pps (IR1_Seq sl);
        PP.add_string pps ")"; 		 
        PP.end_block pps)
  | pp_stm pps (IR1_While (e, ir1_stm)) = 
      (PP.begin_block pps PP.CONSISTENT 0; 
       PP.add_string pps "while "; 
       ppe pps e; 
       PP.add_string pps " do "; 
       PP.add_break pps (0, 0); 
       PP.begin_block pps PP.CONSISTENT 3;		
       PP.add_string pps "("; 
       pp_stm pps ir1_stm; 
       PP.add_string pps ")"; 
       PP.end_block pps;
       PP.end_block pps) 
  | pp_stm pps (IR1_Print e) = 
      (
       PP.add_string pps "print("; 
       ppe pps e; 
       PP.add_string pps ")" 
      ) 

val pp_ir1_expr = Library.mk_pp ppe 
val pp_ir1_stm = Library.mk_pp pp_stm


