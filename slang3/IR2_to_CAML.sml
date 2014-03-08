open AST_camlvm_assembler;

fun ir2_to_caml (IR2_Comment s) =
  | ir2_to_caml (IR2_Label l) =
  | ir2_to_caml IR2_Skip =
  | ir2_to_caml IR2_Halt = 
  | ir2_to_caml (IR2_Var vk) =
  | ir2_to_caml (IR2_KnownFun v) =
  | ir2_to_caml (IR2_Integer n) = 
  | ir2_to_caml (IR2_Boolean true) = 
  | ir2_to_caml (IR2_Boolean false) =
  | ir2_to_caml (IR2_UnaryOp (AST_L3.Neg, e)) =
  | ir2_to_caml (IR2_UnaryOp (AST_L3.Not, e)) = 
  | ir2_to_caml (IR2_Op (e1, oper, e2)) =
  | ir2_to_caml (IR2_StoreLocal (e, n)) = 
  | ir2_to_caml (IR2_Assign (e1, e2)) =
  | ir2_to_caml (IR2_Deref e) = 
  | ir2_to_caml (IR2_Ref e) =
  | ir2_to_caml (IR2_Discard e) = 
  | ir2_to_caml (IR2_App ([IR2_KnownFun "print"], [el])) =
  | ir2_to_caml (IR2_App ([IR2_KnownFun "read"], [])) =
  | ir2_to_caml (IR2_App ([IR2_KnownFun v], ell)) =
  | ir2_to_caml (IR2_App (el, ell)) =
  | ir2_to_caml (IR2_Jump l) =
  | ir2_to_caml (IR2_Fjump (e,l)) =
  | ir2_to_caml (IR2_Closure (f, vkl)) = 

and ir2_list_to_caml el = List.concat (List.map ir2_to_caml el)

fun ir2_fun_to_caml (IR2_FunLabel(f, 0, num_args, el)) =
  | ir2_fun_to_caml (IR2_FunLabel(f, num_locals, num_args, el)) = 

fun ir2_fn_list_to_caml fl = List.concat (List.map ir2_fun_list_to_caml fl)

fun ir2_to_caml (el, fl) =
    let val result = (ir2_list_to_caml el) @ (ir2_fun_list_to_vsml fl)
        val _ = if !Global.verbose
                then (print "\n\n CAML ASSEMBLER =\n";
                      print (AST_caml_assembler.caml_program_to_string result))
                else ()
    in
	result
    end
