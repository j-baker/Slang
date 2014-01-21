open AST_IR1; (* for expressions *) 
open AST_IR2; 
open AST_vsm_assembler; 

val new_offset = Library.new_counter(); 

fun find a [] = NONE 
  | find a ((b, v) :: rest) = if a = b then SOME v else find a rest 

fun visit_vsm_data_loc env d = 
    case find d env 
    of NONE => (d, new_offset()) :: env 
     | SOME _ => env 

val vsm_unit_value = 0 
val vsm_true_value = 1 
val vsm_false_value = 0
 
fun expr_to_vsm_code_list env IR1_ESkip            = 
   (* note that currently the language has no operation to "consume" this value! *) 
       (env, [VSM_Push vsm_unit_value])  
  | expr_to_vsm_code_list env (IR1_Integer n)     = 
       (env, [VSM_Push n])
  | expr_to_vsm_code_list env (IR1_Boolean true)  = 
       (env, [VSM_Push vsm_true_value])
  | expr_to_vsm_code_list env (IR1_Boolean false) = 
       (env, [VSM_Push vsm_false_value])
  | expr_to_vsm_code_list env (IR1_UnaryOp (AST_L1.Neg, e)) = 
       let val (env1, cl) = expr_to_vsm_code_list env e
       in 
          (env1, [VSM_Comment "start neg ...", VSM_Push 0] 
                 @ cl 
                 @ [VSM_Comment "... end neg", VSM_Sub])
       end 
  | expr_to_vsm_code_list env (IR1_UnaryOp (AST_L1.Not, e)) = 
       let val (env1, cl) = expr_to_vsm_code_list env e
           and l_true = Global.new_label() 
           and l_end = Global.new_label() 
       in 
          (env1, 
           [VSM_Comment "start boolean negation ... "] 
           @ cl 
           @ [VSM_Ifz l_true, 
              VSM_Comment "push false", 
              VSM_Push vsm_false_value,          (* Ifz consumed top-of-stack! *) 
              VSM_Jmp l_end, 
              VSM_Label l_true, 
              VSM_Comment "push true", 
              VSM_Push vsm_true_value, 
              VSM_Comment " ... end negation", 
              VSM_Label l_end
             ])
      end 
  | expr_to_vsm_code_list env (IR1_Op (e1, AST_L1.Plus, e2)) = 
       let val (env1, cl1) = expr_to_vsm_code_list env e1
           val (env2, cl2) = expr_to_vsm_code_list env1 e2
       in 
          (env2, cl1 @ cl2 @ [VSM_Add]) 
       end 
  | expr_to_vsm_code_list env (IR1_Op (e1, AST_L1.Mult, e2)) = 
       let val (env1, cl1) = expr_to_vsm_code_list env e1
           val (env2, cl2) = expr_to_vsm_code_list env1 e2
       in 
          (env2, cl1 @ cl2 @ [VSM_Mul]) 
       end 
  (* note that "sub" semantics is "below-top-of-stack - top-of-stack", so this works! *) 
  | expr_to_vsm_code_list env (IR1_Op (e1, AST_L1.Subt, e2)) =     
       let val (env1, cl1) = expr_to_vsm_code_list env e1
           val (env2, cl2) = expr_to_vsm_code_list env1 e2
       in 
          (env2, cl1 @ cl2 @ [VSM_Sub]) 
       end 
  | expr_to_vsm_code_list env (IR1_Op (e1, AST_L1.GTEQ, e2)) = 
       let val (env1, cl1) = expr_to_vsm_code_list env e1
           val (env2, cl2) = expr_to_vsm_code_list env1 e2
           val l_false = Global.new_label() 
           val l_end = Global.new_label() 
      in  (env2, 
           [VSM_Comment "start >= ... "] 
           @ cl1
           @ cl2 
           @ [VSM_Sub,                (* "e1 - e2 now on top of stack *) 
              VSM_Ifn l_false, 
              VSM_Comment "push true", 
              VSM_Push vsm_true_value, 
              VSM_Jmp l_end, 
              VSM_Label l_false, 
              VSM_Comment "push false", 
              VSM_Push vsm_false_value, 
              VSM_Comment " ... end >=", 
              VSM_Label l_end
             ])
      end 
  | expr_to_vsm_code_list env (IR1_Deref l) = 
       let val env2 = visit_vsm_data_loc env l 
       in case find l env2
          of SOME offset => (env2, [VSM_Comment ("load " ^ l), VSM_Load offset])
           | NONE => Library.internal_error ("expr_to_vsm_code_list : " ^ l ^ " not found!")
       end
  | expr_to_vsm_code_list env (IR1_EIf(e0, e1, e2))        =  
    let val (env0, cl0) = expr_to_vsm_code_list env e0 
        val (env1, cl1) = expr_to_vsm_code_list env0 e1 
        val (env2, cl2) = expr_to_vsm_code_list env1 e2 
        val l_else = Global.new_label() 
        val l_end = Global.new_label() 
    in 
           (env2, [VSM_Comment " start EIf ..."]
                  @ cl0 
                  @ [VSM_Comment "EIf test", 
                     VSM_Ifz l_else] 
                  @ cl1 
                  @ [VSM_Jmp l_end, 
                     VSM_Comment "else case", 
                     VSM_Label l_else]
                  @ cl2 
                  @ [VSM_Comment "... end EIf", 
                     VSM_Label l_end])
    end  

fun stm_to_vsm_code_list env IR2_Halt = (env, [VSM_Comment " that's all folks!", VSM_Hlt])
  | stm_to_vsm_code_list env (IR2_Label l) = (env, [VSM_Label l])
  | stm_to_vsm_code_list env (IR2_Expr e)  = expr_to_vsm_code_list env e
  | stm_to_vsm_code_list env (IR2_Assign(l, e)) = 
       let val (env1, cl) = expr_to_vsm_code_list env e
           val env2 = visit_vsm_data_loc env1 l 
       in case find l env2
            of SOME offset => 
	        (env2, cl 
                       @ [VSM_Comment ("store " ^ l), 
                          VSM_Store offset
                          (* NOTE: the "unit" value is not pushed. this is 
                             a "mini-optimisation" 
                           *) 
                         ]
               ) 
             | NONE => Library.internal_error "expr_to_vsm_code_list(1) : This cannot happen!"
       end 
  | stm_to_vsm_code_list env (IR2_Jump l) = (env, [VSM_Jmp l])
  | stm_to_vsm_code_list env (IR2_Fjump (e, l)) = 
    let val (env1, cl1) = expr_to_vsm_code_list env e
    in 
        (env1, cl1 @ [VSM_Ifz l])
    end  
  | stm_to_vsm_code_list env (IR2_Print e)  = 
   (* NOTE: the "unit" value is not pushed. this is 
     a "mini-optimisation" *) 
    let val (env1, cl) = expr_to_vsm_code_list env e
    in 
       (env1, cl @ [VSM_Pri])
    end 

fun stm_list_to_vsm_code_list env [] = (env, []) 
  | stm_list_to_vsm_code_list env (stm :: rest) = 
  (* NOTE: the "unit" value is not pushed and popped. This is 
     a "mini-optimisation" 
  *) 
    let val (env1, cl1) = stm_to_vsm_code_list env stm 
        val (env2, cl2) = stm_list_to_vsm_code_list env1 rest 
    in 
       (env2, cl1 @ cl2) 
    end 

fun env_to_init_code env = 
    let fun aux [] = [] 
          | aux ((l, _) :: rest ) = 
            (VSM_Comment ("slot for " ^ l)) ::  (VSM_Push 0) :: (aux rest) 
    in aux (List.rev env) end 

fun vsm_code_gen sl = 
    let val (env, cl) = stm_list_to_vsm_code_list [] sl
        val result = (env_to_init_code env) @ cl 
        val _ = if !Global.verbose
                then (print "\n\n VSM assember : \n"; 
                      print ((vsm_program_to_string result) ^ "\n"))
                else () 
    in 
	result 
    end 


