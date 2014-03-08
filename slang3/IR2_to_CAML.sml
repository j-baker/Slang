open AST_camlvm_assembler;

val caml_true_value = 1
val caml_false_value = 0

(* Make the assumption that the accumulator can be destroyed at the
   end of each operation *)
fun ir2_to_caml (IR2_Comment s) = [] (* We do not care about comments *)
  | ir2_to_caml (IR2_Label l) =
  | ir2_to_caml IR2_Skip = [] (* Why do we push unit value in IR2? *)
  | ir2_to_caml IR2_Halt = [STOP]
  | ir2_to_caml (IR2_Var vk) =
  | ir2_to_caml (IR2_KnownFun v) =
  | ir2_to_caml (IR2_Integer n) = [CONSTINT n, PUSH]
  | ir2_to_caml (IR2_Boolean true) = [CONSTINT caml_true_value, PUSH]
  | ir2_to_caml (IR2_Boolean false) = [CONSTINT caml_false_value, PUSH]
  | ir2_to_caml (IR2_UnaryOp (AST_L3.Neg, e)) = [NEGINT]
  | ir2_to_caml (IR2_UnaryOp (AST_L3.Not, e)) = [BOOLNOT]
  | ir2_to_caml (IR2_Op (e1, oper, e2)) =
    let val b1 = ir2_list_to_caml e1
        val b2 = ir2_list_to_caml e2
    in
	case oper of
	    AST_L3.Plus => b1 @ b2 @ [ACC 0, POP 1, ADDINT, PUSH]
	    AST_L3.Mult => b1 @ b2 @ [ACC 0, POP 1, MULINT, PUSH]
            AST_L3.Subt => b2 @ b1 @ [ACC 0, POP 1, SUBINT, PUSH]
            AST_L3.GTEQ => b1 @ b2 @ [ACC 0, POP 1, GEINT, PUSH]
            AST_L3.EQ => b1 @ b2 @ [ACC 0, POP 1, EQ, PUSH]
    end
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
