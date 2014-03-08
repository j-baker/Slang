open AST_IR1;
open AST_IR2;

val new_label = Global.new_label

val let_depth = ref 0  (* let-depth of current function body *) 

fun find [] a = Library.internal_error ("IR1_to_IR2 : " ^ a ^ " not bound!") 
  | find ((b, v) :: rest) a = if a = b then v else find rest a 

fun ir1_to_ir2 next_let_index var_map IR1_Skip                   = [IR2_Skip]
  | ir1_to_ir2 next_let_index var_map (IR1_Var vk)               = [IR2_Var(var_kind_to_ir2 var_map vk)] 
  | ir1_to_ir2 next_let_index var_map (IR1_KnownFun f)           = [IR2_KnownFun f] 
  | ir1_to_ir2 next_let_index var_map (IR1_Integer n)            = [IR2_Integer n]
  | ir1_to_ir2 next_let_index var_map (IR1_Boolean b)            = [IR2_Boolean b] 
  | ir1_to_ir2 next_let_index var_map (IR1_UnaryOp (uop, e))     = [IR2_UnaryOp(uop, ir1_to_ir2 next_let_index var_map e)]
  | ir1_to_ir2 next_let_index var_map (IR1_Op (e1, bop, e2))     = [IR2_Op(ir1_to_ir2 next_let_index var_map e1, bop, ir1_to_ir2 next_let_index var_map e2)] 
  | ir1_to_ir2 next_let_index var_map (IR1_Ref e)                = [IR2_Ref(ir1_to_ir2 next_let_index var_map e)]
  | ir1_to_ir2 next_let_index var_map (IR1_Assign (e1, e2))      = [IR2_Assign(ir1_to_ir2 next_let_index var_map e1, ir1_to_ir2 next_let_index var_map e2)] 
  | ir1_to_ir2 next_let_index var_map (IR1_Deref e)              = [IR2_Deref(ir1_to_ir2 next_let_index var_map e)]
  | ir1_to_ir2 next_let_index var_map (IR1_Seq (e1, e2))         = (IR2_Discard(ir1_to_ir2 next_let_index var_map e1)) :: (ir1_to_ir2 next_let_index var_map e2)
  | ir1_to_ir2 next_let_index var_map (IR1_App (e, args))        = [IR2_App(ir1_to_ir2 next_let_index var_map e, List.map (ir1_to_ir2 next_let_index var_map) args)] 
  | ir1_to_ir2 next_let_index var_map (IR1_Closure(f, vkl) )     = [IR2_Closure(f, List.map (var_kind_to_ir2 var_map) vkl)]
  | ir1_to_ir2 next_let_index var_map (IR1_Let(v, e1, e2) )      = 
    let 
        val next = next_let_index + 1 
        val el1 = ir1_to_ir2 next var_map e1
        val new_var_map = (v, (next_let_index, !let_depth)) :: var_map
        val el2 = ir1_to_ir2 next new_var_map e2
    in 
       (IR2_StoreLocal (el1, next_let_index)) :: el2 
    end 
  | ir1_to_ir2 next_let_index var_map (IR1_If(e1, e2, e3))       =
    let val else_label = new_label () 
        val end_label  = new_label () 
    in 
       (IR2_Fjump (ir1_to_ir2 next_let_index var_map e1, else_label)) :: 
       (ir1_to_ir2 next_let_index var_map e2)
       @ [IR2_Jump end_label, IR2_Label else_label] 
       @ (ir1_to_ir2 next_let_index var_map e3)
       @ [IR2_Label end_label]
    end 
  | ir1_to_ir2 next_let_index var_map (IR1_While (e1, e2))       =
    let val start_label = new_label () 
        val end_label =  new_label () 
    in 
       [IR2_Label start_label, IR2_Fjump (ir1_to_ir2 next_let_index var_map e1, end_label)] 
       @ [IR2_Discard(ir1_to_ir2 next_let_index var_map e2), 
          IR2_Jump start_label, 
          IR2_Label end_label, 
         IR2_Skip]
    end

and var_kind_to_ir2 var_map (IR1_ArgVar v)      = IR2_ArgVar (v, find var_map v)
  | var_kind_to_ir2 var_map (IR1_LetVar v)      = IR2_LocalVar (v, find var_map v)
  | var_kind_to_ir2 var_map (IR1_EnvVar (v, k)) = IR2_EnvVar (v, find var_map v, k)

fun max_let_depth m (IR1_UnaryOp (uop, e))     = max_let_depth m e
  | max_let_depth m (IR1_Op (e1, bop, e2))     = Int.max (max_let_depth m e1, max_let_depth m e2)
  | max_let_depth m (IR1_Ref e)                = max_let_depth m e
  | max_let_depth m (IR1_Assign (e1, e2))      = Int.max (max_let_depth m e1, max_let_depth m e2)
  | max_let_depth m (IR1_Deref e)              = max_let_depth m e
  | max_let_depth m (IR1_Seq (e1, e2))         = Int.max (max_let_depth m e1, max_let_depth m e2)
  | max_let_depth m (IR1_App (e, args))        = Int.max (max_let_depth m e,  max_let_depth_list m args)
  | max_let_depth m (IR1_If(e1, e2, e3))       = Int.max (Int.max (max_let_depth m e1, max_let_depth m e2), max_let_depth m e3)
  | max_let_depth m (IR1_While (e1, e2))       = Int.max (max_let_depth m e1, max_let_depth m e2)
  | max_let_depth m (IR1_Let(v, e1, e2) )      = Int.max (max_let_depth m e1, max_let_depth (m + 1) e2)
  | max_let_depth m _                          = m 

and max_let_depth_list m [] = m 
  | max_let_depth_list m (e::rest) = Int.max (max_let_depth m e,  max_let_depth_list m rest)

fun make_var_map _ _ [] = [] 
  | make_var_map n next (a::rest) = (a, (next, n)) :: (make_var_map n (next + 1) rest)

fun ir1_function_to_ir2 (IR1_Letrec(f, vl, e)) = 
    let val _ = (let_depth := max_let_depth 0 e)   (* record let-depth *) 
        val num_args = List.length vl 
        val var_map = make_var_map num_args 1 vl
    in
      IR2_FunLabel(f, !let_depth, num_args, (ir1_to_ir2 1 var_map e))
    end

fun translate (e, fl)  =
    let val main_var = "_Main"
        val main = IR1_Letrec(main_var, [], e)
        val ir2_fun_list = List.map ir1_function_to_ir2 (main::fl)
        val apply_main = [IR2_App([IR2_KnownFun main_var], [[]]), IR2_Halt] 
        val result = (apply_main, ir2_fun_list)
        val _ = if !Global.verbose 
                then (print "\n\n IR2 AST =\n"; AST_IR2.pp_ir2_program result)
                else () 
    in
        result 
    end





