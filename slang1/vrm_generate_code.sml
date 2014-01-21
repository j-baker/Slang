open AST_IR1; (* for expressions *) 
open AST_IR2; 
open AST_vrm_assembler; 

val next_temp_loc_count = Library.new_counter(); 
fun new_temp_loc () = "_TEMP_" ^ (Int.toString (next_temp_loc_count ())) 

val new_label = Global.new_label

val zero_loc    = "_Zero"
val one_loc    = "_One"

fun expr_to_vrm_code_list IR1_ESkip            = 
    let val l = new_temp_loc () in ([VRM_Set(l, 0)], l) end  
  | expr_to_vrm_code_list (IR1_Integer n)     = 
    let val l = new_temp_loc () in ([VRM_Set(l, n)], l) end  
  | expr_to_vrm_code_list (IR1_Boolean true)  = 
    let val l = new_temp_loc () in ([VRM_Set(l, 1)], l) end  
  | expr_to_vrm_code_list (IR1_Boolean false)  = 
    let val l = new_temp_loc () in ([VRM_Set(l, 0)], l) end   
  | expr_to_vrm_code_list (IR1_UnaryOp (AST_L1.Neg, e)) = 
    let val (cl, l) = expr_to_vrm_code_list e 
        and l' = new_temp_loc ()  
    in 
        (cl @ [VRM_Sub(l', zero_loc, l)], l') 
    end 
  | expr_to_vrm_code_list (IR1_UnaryOp (AST_L1.Not, e)) = 
    let val (cl, l) = expr_to_vrm_code_list e 
        and l' = new_temp_loc ()  
    in 
        (cl @ [VRM_Mul(l', one_loc, l)], l') 
    end 
  | expr_to_vrm_code_list (IR1_Op (e1, AST_L1.Plus, e2)) = 
    let val (cl1, l1) = expr_to_vrm_code_list e1 
        and (cl2, l2) = expr_to_vrm_code_list e2 
        and l3 = new_temp_loc ()  
    in 
        (cl1 @ cl2 @ [VRM_Add(l3, l1, l2)], l3) 
    end 
  | expr_to_vrm_code_list (IR1_Op (e1, AST_L1.Mult, e2)) = 
    let val (cl1, l1) = expr_to_vrm_code_list e1 
        and (cl2, l2) = expr_to_vrm_code_list e2 
        and l3 = new_temp_loc ()  
    in 
        (cl1 @ cl2 @ [VRM_Mul(l3, l1, l2)], l3) 
    end 
  | expr_to_vrm_code_list (IR1_Op (e1, AST_L1.Subt, e2)) =     
    let val (cl1, l1) = expr_to_vrm_code_list e1 
        and (cl2, l2) = expr_to_vrm_code_list e2 
        and l3 = new_temp_loc ()  
    in 
        (cl1 @ cl2 @ [VRM_Sub(l3, l1, l2)], l3) 
    end 
  | expr_to_vrm_code_list (IR1_Op (e1, AST_L1.GTEQ, e2)) = 
    let val (cl1, l1) = expr_to_vrm_code_list e1 
        and (cl2, l2) = expr_to_vrm_code_list e2 
        and false_label = new_label ()  
        and end_label = new_label ()  
        and result_loc = new_temp_loc ()  
        and tmp_loc = new_temp_loc ()  
    in 
        (cl1 @ cl2  
         @ [VRM_Comment "start >= ...", 
            VRM_Sub(tmp_loc, l1, l2), 
            VRM_Ifn(tmp_loc, false_label), 
            VRM_Set(result_loc, 1), 
            VRM_Jmp end_label, 
            VRM_Label false_label,
            VRM_Set(result_loc, 0), 
            VRM_Comment "end >=", 
            VRM_Label end_label], 
         result_loc)
     end 
  | expr_to_vrm_code_list (IR1_Deref l) = ([], l) 
  | expr_to_vrm_code_list (IR1_EIf(e1, e2, e3)) =  
    let val (cl1, l1) = expr_to_vrm_code_list e1 
        and (cl2, l2) = expr_to_vrm_code_list e2 
        and (cl3, l3) = expr_to_vrm_code_list e3 
        and else_label = new_label ()  
        and end_label = new_label ()  
        and result_loc = new_temp_loc ()  
    in 
        ([VRM_Comment "Start EIf ..."]
         @ cl1 
         @ [VRM_Ifz(l1, else_label)]
         @ cl2 
         @  [VRM_Mov(result_loc, l2), 
             VRM_Jmp(end_label), 
             VRM_Label else_label]
         @ cl3 
         @ [VRM_Mov(result_loc, l3), 
            VRM_Comment "end EIf", 
            VRM_Label end_label], 
         result_loc) 
     end 

fun stm_to_vrm_code_list IR2_Halt = [VRM_Comment " that's all folks!", VRM_Hlt]  
  | stm_to_vrm_code_list (IR2_Label l) = [VRM_Label l]
  | stm_to_vrm_code_list (IR2_Expr e)  = [] (* min-optimisation. correct? *) 
  | stm_to_vrm_code_list (IR2_Jump l) = [VRM_Jmp l]
  | stm_to_vrm_code_list (IR2_Assign(l, e)) = 
    let val (cl, l') = expr_to_vrm_code_list e 
    in 
        cl @ [VRM_Mov (l, l')]
    end 
  | stm_to_vrm_code_list (IR2_Fjump (e, l)) = 
    let val (cl, l') = expr_to_vrm_code_list e 
    in 
        cl @ [VRM_Ifz(l', l)]
    end 
  | stm_to_vrm_code_list (IR2_Print e)  = 
    let val (cl, l) = expr_to_vrm_code_list e 
    in 
        cl @ [VRM_Pri l]
    end 

val constants_code = [
     VRM_Comment "used for zero, false, and ESkip constants", 
     VRM_Set(zero_loc, 0), 
     VRM_Comment "used for one and true constants", 
     VRM_Set(one_loc, 1)
    ] 

fun vrm_code_gen cl = 
    let val result = constants_code 
                     @ (List.concat (List.map stm_to_vrm_code_list cl))
        val _ = if !Global.verbose
                then (print "\n\n VRM code : \n"; 
                      print ((vrm_program_to_string result) ^ "\n"))
                else () 
    in 
	result 
    end 


