open AST_IR1; 
open AST_IR2; 

fun flatten (IR1_Expr e) = [IR2_Expr e] 
  | flatten (IR1_Assign(l, e)) = [IR2_Assign(l, e)] 
  | flatten (IR1_Seq sl) = List.concat (List.map flatten sl) 
  | flatten (IR1_SIf(e, stm1, stm2)) = 
    let val sl1 = flatten stm1 
        and sl2 = flatten stm2 
        and else_label = Global.new_label () 
        and end_label = Global.new_label () 
    in 
       (IR2_Fjump (e, else_label)) :: 
         (sl1 @ 
           ((IR2_Jump end_label) :: 
             ((IR2_Label else_label) :: 
               (sl2 @ 
                 [IR2_Label end_label]))))
    end 
  | flatten (IR1_While(e, stm)) = 
    let val sl = flatten stm
        and start_label = Global.new_label () 
        and end_label = Global.new_label () 
    in 
       (IR2_Label start_label) :: 
         ((IR2_Fjump (e, end_label)) :: 
           (sl @ 
             [IR2_Jump start_label, 
              IR2_Label end_label]))
    end 
  | flatten (IR1_Print e) = [IR2_Print e]

fun translate_ir1_to_ir2 stm = 
    let val result = (flatten stm) @ [IR2_Halt] 
        val _ = if !Global.verbose
                then (print "\n\n IR2 AST = \n"; 
                      pp_program result)
                else () 
    in result end 
