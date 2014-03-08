open AST_L3;

val freshVar = Global.freshVar 

fun find a [] = Library.internal_error ("Alpha.sml: variable " ^ a ^ " not bound") 
  | find a ((b, v) :: rest) = if a = b then v else find a rest 

fun alpha var_map Skip               = Skip
  | alpha var_map (Integer n)        = Integer n
  | alpha var_map (Boolean b)        = Boolean b
  | alpha var_map (UnaryOp (uop, e)) = UnaryOp(uop, alpha var_map e)
  | alpha var_map (Op (e1, bop, e2)) = Op(alpha var_map e1, bop, alpha var_map e2)
  | alpha var_map (Ref e)            = Ref(alpha var_map e)
  | alpha var_map (Assign (e1, e2))  = Assign(alpha var_map e1, alpha var_map e2)
  | alpha var_map (Deref e)          = Deref(alpha var_map e)
  | alpha var_map (Seq (e1, e2))     = Seq(alpha var_map e1, alpha var_map e2)
  | alpha var_map (If(e1, e2, e3))   = If(alpha var_map e1, alpha var_map e2,alpha var_map e3)
  | alpha var_map (While (e1, e2))   = While(alpha var_map e1, alpha var_map e2)
  | alpha var_map (Var v)            = Var (find v var_map)
  | alpha var_map (Fn (v, t, e, ot)) = 
    let val fv = freshVar(v) 
    in 
        Fn (fv, t, alpha ((v, fv)::var_map) e, ot) 
    end 
  | alpha var_map (App (e, args)) = App (alpha var_map e, List.map (alpha var_map) args)
        
  | alpha var_map (Let (v, t, e1, e2)) =
    let val fv = freshVar(v) 
    in 
        Let(fv, t, alpha var_map e1, alpha ((v, fv)::var_map) e2) 
    end
  | alpha var_map (Letrec (f, params, t, body, e)) =
         let val fv = freshVar (f)
             val var_map_params = List.map (fn (x, _) => (x, freshVar (x))) params
             val new_var_map = (f, fv) :: var_map_params  @ var_map 
             val new_params = List.map (fn (x, t) => (find x var_map_params, t)) params 
         in
           Letrec(fv, new_params, t, alpha new_var_map body, 
                  alpha ((f, fv) :: var_map) e)
         end

val init_var_map = [("print", "print"), ("read", "read")] 

fun convert e = 
    let val result = alpha init_var_map e 
        val _ = if !Global.verbose
                then (print "\n\nAfter Alpha Conversion : \n";
                      AST_L3.pp_expr result
                     )
                else () 
    in 
	result 
    end 
 


