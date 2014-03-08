open AST_IR2; 
open AST_vsm_assembler; 

val new_label = Global.new_label

val vsm_unit_value  = 0
val vsm_true_value  = 1 
val vsm_false_value = 0

fun vk_to_vsm (IR2_ArgVar (v, (ind, n)))    = [VSM_Arg_Load ((n - ind) + 1)] 
  | vk_to_vsm (IR2_LocalVar (v, (ind, n)))  = [VSM_Load ind] 
  | vk_to_vsm (IR2_EnvVar (v, (ind, n), i)) = [VSM_Arg_Load ((n - ind) + 1), VSM_Deref (i + 1)] 

fun ir2_to_vsm (IR2_Comment s)         = [VSM_Comment s]
  | ir2_to_vsm (IR2_Label l)           = [VSM_Label l]
  | ir2_to_vsm IR2_Skip                = [VSM_Push vsm_unit_value]
  | ir2_to_vsm IR2_Halt                = [VSM_Hlt]
  | ir2_to_vsm (IR2_Var vk)            = vk_to_vsm vk 
  | ir2_to_vsm (IR2_KnownFun v)        = [VSM_PushFun v]
  | ir2_to_vsm (IR2_Integer n)         = [VSM_Push n]
  | ir2_to_vsm (IR2_Boolean true)      = [VSM_Push vsm_true_value]
  | ir2_to_vsm (IR2_Boolean false)     = [VSM_Push vsm_false_value]
  | ir2_to_vsm (IR2_UnaryOp (AST_L3.Neg, e)) =
      ((ir2_list_to_vsm e) @ [VSM_Push 0, VSM_Sub])
  | ir2_to_vsm (IR2_UnaryOp (AST_L3.Not, e)) =
      (* 1 - 0(false) is 1(true) and 1 - 1(true) is 0(false) *)
      ((ir2_list_to_vsm e) @ [VSM_Push 1, VSM_Sub])
  | ir2_to_vsm (IR2_Op (e1, oper, e2)) =
    let val b1 = ir2_list_to_vsm e1
        val b2 = ir2_list_to_vsm e2
    in
      case oper of
           AST_L3.Plus => b1 @ b2 @ [VSM_Add]
         | AST_L3.Mult => b1 @ b2 @ [VSM_Mul]
         | AST_L3.Subt => b2 @ b1 @ [VSM_Sub]
         | AST_L3.GTEQ =>
             let val l_true = new_label()
                 val l_end = new_label()
             in
                b2 @ b1 @ [VSM_Sub,
                (* b1 - b2 now on top of stack 
                   b1 >= b2 iff b1 -b2 >= 0 
                *) 
                VSM_Ifp l_true,
                VSM_Push vsm_false_value,
                VSM_Jmp l_end,
                VSM_Label l_true,
                VSM_Push vsm_true_value,
                VSM_Label l_end
                ]
             end
         | AST_L3.EQ =>
             let val l_true = new_label()
                 val l_end = new_label()
             in
                b1 @ b2 @ [VSM_Sub,
                VSM_Ifz l_true,
                VSM_Push vsm_false_value,
                VSM_Jmp l_end,
                VSM_Label l_true,
                VSM_Push vsm_true_value,
                VSM_Label l_end
                ]
             end
    end
  | ir2_to_vsm (IR2_StoreLocal (e, n)) = (ir2_list_to_vsm e) @  [VSM_Store n]
  | ir2_to_vsm (IR2_Assign (e1, e2)) = (ir2_list_to_vsm e1) @ (ir2_list_to_vsm e2) @[VSM_Assign, VSM_Push vsm_unit_value]
  | ir2_to_vsm (IR2_Deref e)           = (ir2_list_to_vsm e) @ [VSM_Deref 1]
  | ir2_to_vsm (IR2_Ref e)             = (ir2_list_to_vsm e) @ [VSM_Ref]
  | ir2_to_vsm (IR2_Discard e)         = (ir2_list_to_vsm e) @ [VSM_Pop]
  | ir2_to_vsm (IR2_App ([IR2_KnownFun "print"], [el])) = 
       (ir2_list_to_vsm el) @ [VSM_Pri, VSM_Push vsm_unit_value]
  | ir2_to_vsm (IR2_App ([IR2_KnownFun "read"], [])) = 
        [VSM_Rdi]
  | ir2_to_vsm (IR2_App ([IR2_KnownFun v], ell))      = 
      (List.concat (List.map ir2_list_to_vsm ell)) @ [VSM_Call v]
  | ir2_to_vsm (IR2_App (el, ell))                 =
      (ir2_list_to_vsm el)   
      @ (List.concat (List.map ir2_list_to_vsm ell))
      @ [VSM_Call_Closure (1 + List.length ell)]
  | ir2_to_vsm (IR2_Jump l)          = [VSM_Jmp l]
  | ir2_to_vsm (IR2_Fjump (e, l))    = (ir2_list_to_vsm e) @ [VSM_Ifz l]
  | ir2_to_vsm (IR2_Closure (f, vkl)) = 
       (List.concat (List.map vk_to_vsm vkl)) @ [VSM_Closure (f, List.length vkl)]
    
and ir2_list_to_vsm el = List.concat (List.map ir2_to_vsm el) 

fun ir2_fun_to_vsm (IR2_FunLabel(f, 0, num_args, el)) = 
    (VSM_Label f) :: (ir2_list_to_vsm el) @ [VSM_Return num_args]  
  | ir2_fun_to_vsm (IR2_FunLabel(f, num_locals, num_args, el)) = 
    [VSM_Label f, VSM_AllocateLocals num_locals] @ (ir2_list_to_vsm el) @ [VSM_Return num_args]  

fun ir2_fun_list_to_vsm fl = List.concat (List.map ir2_fun_to_vsm fl) 

fun ir2_to_vsm (el, fl) = 
    let val result = (ir2_list_to_vsm el) @ (ir2_fun_list_to_vsm fl) 
        val _ = if !Global.verbose 
                then (print "\n\n VSM ASSEMBLER =\n";
                      print (AST_vsm_assembler.vsm_program_to_string result))
                else () 
    in 
        result 
    end 
