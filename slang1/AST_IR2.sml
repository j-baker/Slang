type loc  = AST_L1.loc 
type label = string 

type ir2_expr = AST_IR1.ir1_expr 

datatype ir2_stm = 
         IR2_Halt 
       | IR2_Label of label 
       | IR2_Expr of ir2_expr  
       | IR2_Assign of loc * ir2_expr
       | IR2_Jump of label 
       | IR2_Fjump of ir2_expr * label 
       | IR2_Print of ir2_expr 

type program = ir2_stm list 

(* pretty priting using Mosml's PP module *) 

val ppe   = AST_IR1.ppe

fun ppm pps IR2_Halt = PP.add_string pps "halt" 
  | ppm pps (IR2_Label lab) = PP.add_string pps (lab ^ " : ")
  | ppm pps (IR2_Expr e)    = ppe pps e 
  | ppm pps (IR2_Assign (l, e)) =  
       (
        PP.add_string pps l; 
        PP.add_string pps " := "; 
	ppe pps e
       )
  | ppm pps (IR2_Jump l) = 
       (
        PP.add_string pps "Jump "; 
        PP.add_string pps l
       )
  | ppm pps (IR2_Fjump (e, l)) = 
       (
        PP.add_string pps "Fjump "; 
	ppe pps e; 
        PP.add_string pps " "; 
        PP.add_string pps l
       )
  | ppm pps (IR2_Print e) = 
      (
       PP.add_string pps "print("; 
       ppe pps e; 
       PP.add_string pps ")" 
      ) 

fun ppm_list pps [] = () 
  | ppm_list pps [m] = ppm pps m 
  | ppm_list pps ((IR2_Label lab) :: rest) = 
      (PP.add_string pps (lab ^ " : "); ppm_list pps rest)
  | ppm_list pps (m :: rest) = 
      (
       ppm pps m; 
       PP.add_string pps "; "; 
       PP.add_newline pps; 
       ppm_list pps rest
      ) 

val pp_program = Library.mk_pp ppm_list 
