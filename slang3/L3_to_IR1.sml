open AST_L3;
open AST_IR1;

val freshVar = Global.freshVar 


fun in_list v [] = false 
  | in_list v (w::rest) = (v = w) orelse (in_list v rest)

fun list_union l [] = l
  | list_union l (a::rest) = if in_list a l then list_union l rest else list_union (a::l) rest

fun lists_union [] = [] 
  | lists_union (l::rest) = list_union l (lists_union rest)

fun freevars bound_vars (UnaryOp(_, e)) = freevars bound_vars e
  | freevars bound_vars (Op(e1, _, e2)) = list_union (freevars bound_vars e1) (freevars bound_vars e2)
  | freevars bound_vars (If(e1, e2, e3)) = list_union (list_union (freevars bound_vars e1) (freevars bound_vars e2)) (freevars bound_vars e3)
  | freevars bound_vars (Seq (e1, e2)) =   list_union (freevars bound_vars e1) (freevars bound_vars e2)
  | freevars bound_vars (While (e1, e2)) = list_union (freevars bound_vars e1) (freevars bound_vars e2)
  | freevars bound_vars (Var v) = if in_list v bound_vars then [] else [v] 
  | freevars bound_vars (Fn (v, _, e, _)) = freevars (v :: bound_vars) e
  | freevars bound_vars (Let (x,_, e1, e2))    = list_union (freevars bound_vars e1) (freevars (x :: bound_vars) e2)
  | freevars bound_vars (Letrec (f, args, _ ,e1, e2)) = list_union (freevars ((List.map (fn (x, _) => x) args) @ (f :: bound_vars)) e1) (freevars (f :: bound_vars) e2)
  | freevars bound_vars (App(e, el)) = list_union (freevars bound_vars e) (lists_union (List.map (freevars bound_vars) el))
  | freevars bound_vars (Assign(e1, e2)) = list_union (freevars bound_vars e1) (freevars bound_vars e2)
  | freevars bound_vars (Deref e) = freevars bound_vars e
  | freevars bound_vars (Ref e) = freevars bound_vars e
  | freevars bound_vars _ = [] 

fun find [] a = Library.internal_error ("L3_to_IR1 : " ^ a ^ " not bound!") 
  | find ((b, v) :: rest) a = if a = b then v else find rest a 

fun mk_env_var_map p l = 
    let fun aux carry n [] = carry 
          | aux carry n (x :: rest) = aux ((x, IR1_EnvVar(p, n)):: carry) (n + 1) rest
    in aux [] 1 l end 


datatype fun_type = IR1_KnownFunction 
                  | IR1_ClosedFunction of var 

fun l2_to_ir1 fun_map var_map Skip               = (IR1_Skip, [])
  | l2_to_ir1 fun_map var_map (Integer n)        = (IR1_Integer n, [])
  | l2_to_ir1 fun_map var_map (Boolean b)        = (IR1_Boolean b, [])
  | l2_to_ir1 fun_map var_map (UnaryOp (uop, e)) = 
    let val (e', fl) = l2_to_ir1 fun_map var_map e
    in 
       (IR1_UnaryOp(uop, e'), fl)
    end 
  | l2_to_ir1 fun_map var_map (Op (e1, bop, e2)) = 
    let val (e1', fl1) = l2_to_ir1 fun_map var_map e1
        val (e2', fl2) = l2_to_ir1 fun_map var_map e2
    in 
        (IR1_Op(e1', bop, e2'), fl1 @ fl2) 
    end 
  | l2_to_ir1 fun_map var_map (Ref e)            = 
    let val (e', fl) = l2_to_ir1 fun_map var_map e
    in 
       (IR1_Ref e', fl)
    end 
  | l2_to_ir1 fun_map var_map (Assign (e1, e2))  = 
    let val (e1', fl1) = l2_to_ir1 fun_map var_map e1
        val (e2', fl2) = l2_to_ir1 fun_map var_map e2
    in 
        (IR1_Assign(e1', e2'), fl1 @ fl2) 
    end 
  | l2_to_ir1 fun_map var_map (Deref e)          = 
    let val (e', fl) = l2_to_ir1 fun_map var_map e
    in 
       (IR1_Deref e', fl)
    end 
  | l2_to_ir1 fun_map var_map (Seq (e1, e2))     = 
    let val (e1', fl1) = l2_to_ir1 fun_map var_map e1
        val (e2', fl2) = l2_to_ir1 fun_map var_map e2
    in 
        (IR1_Seq(e1', e2'), fl1 @ fl2) 
    end 
  | l2_to_ir1 fun_map var_map (If(e1, e2, e3))   = 
    let val (e1', fl1) = l2_to_ir1 fun_map var_map e1
        val (e2', fl2) = l2_to_ir1 fun_map var_map e2
        val (e3', fl3) = l2_to_ir1 fun_map var_map e3
    in 
        (IR1_If(e1', e2', e3'), fl1 @ fl2 @ fl3) 
    end 
  | l2_to_ir1 fun_map var_map (While (e1, e2))   = 
    let val (e1', fl1) = l2_to_ir1 fun_map var_map e1
        val (e2', fl2) = l2_to_ir1 fun_map var_map e2
    in 
        (IR1_While(e1', e2'), fl1 @ fl2) 
    end 
  | l2_to_ir1 fun_map var_map (Fn (v, t, e, NONE)) = 
       Library.internal_error "L3_to_ir1 Fun_map : body of fn must be annotated with type!"
  | l2_to_ir1 fun_map var_map (Fn (v, t1, e, SOME t2)) = 
    let val fv = freshVar("FN") 
    in 
        l2_to_ir1 fun_map var_map (Letrec (fv, [(v, t1)], t2, e, Var fv))
    end 
  | l2_to_ir1 fun_map var_map (Var v) = 
       ((IR1_Var(find var_map v), []) 
        handle _ => 
          (case find fun_map v of 
             IR1_KnownFunction => (IR1_KnownFun v, [])
           | IR1_ClosedFunction _ => 
               Library.internal_error "L3_to_IR1 : var to closed function should never happen!"
          ))
  | l2_to_ir1 fun_map var_map (App (Var f, args)) = 
    let val (args', fl) = l2_list_to_ir1 fun_map var_map args 
    in 
       (case find fun_map f of 
          IR1_KnownFunction => (IR1_App(IR1_KnownFun f, args'), fl)
        | IR1_ClosedFunction p => (IR1_App(IR1_KnownFun f, (IR1_Var(IR1_ArgVar p)) :: args'), fl)
        ) handle _ => (IR1_App(IR1_Var (find var_map f), args'), fl)
    end 
  | l2_to_ir1 fun_map var_map (App (e, args)) = 
    let val (e', fl1) = l2_to_ir1 fun_map var_map e
        val (args', fl2) = l2_list_to_ir1 fun_map var_map args 
    in 
        (IR1_App(e', args'), fl1 @ fl2) 
    end 
  | l2_to_ir1 fun_map var_map (Let (v, _, e1, e2)) = 
    let val (e1', fl1) = l2_to_ir1 fun_map var_map e1
        val (e2', fl2) = l2_to_ir1 fun_map ((v, IR1_LetVar v) :: var_map) e2
    in 
        (IR1_Let (v, e1', e2'), fl1 @ fl2) 
    end 
  | l2_to_ir1 fun_map var_map (Letrec (f, params, _, body, e)) =
    let val param_vars = List.map (fn (x, _) => x) params
        val f_vars = List.map (fn (x, _) => x) fun_map 
        val env_vars = freevars (f :: f_vars @ param_vars) body 
    in 
       if 0 = List.length env_vars
       then 
            let val var_map_params = List.map (fn (x, _) => (x, IR1_ArgVar x)) params
                val new_fun_map = (f, IR1_KnownFunction) :: fun_map
                val (body', fl') = l2_to_ir1 new_fun_map (var_map_params @ var_map) body 
                val fl1 = (IR1_Letrec(f, param_vars, body')) :: fl'
                val (e', fl2) = l2_to_ir1 new_fun_map var_map e
            in 
		(e', fl1 @ fl2) 
            end
       else (* 
             fun f (x) = body in e end 
                
             ==> let g = IR1_Closure(f, [a1, a2 ... ak]) 
                 in e'[f <- IR1_ClosureVar g] end 
 
                 with 

                 fun f(p, x) = body'[ai <- p.i, f(e2) <- f(p, e2')] 
         *) 
         let val p = freshVar "ENV"
             val g = freshVar "CL"
             val var_map_params = List.map (fn x => (x, (IR1_ArgVar x))) param_vars
             val env_var_map = mk_env_var_map p env_vars
             val new_var_map =  (f, IR1_LetVar g) :: var_map_params  @ env_var_map
             val (body', fl1) = l2_to_ir1 ((f, IR1_ClosedFunction p) :: fun_map) new_var_map body 
             val (e', fl2) = l2_to_ir1 fun_map ((f, IR1_LetVar g) :: var_map) e
             val func = IR1_Letrec(f, p :: param_vars, body') 
         in
           (IR1_Let(g, IR1_Closure(f, List.map (find (var_map_params @ var_map)) env_vars), e'), 
            func :: fl1 @ fl2) 
         end
    end  

and l2_list_to_ir1 fun_map var_map [] = ([], [])  
  | l2_list_to_ir1 fun_map var_map [e] = 
    let val (e', fl) = l2_to_ir1 fun_map var_map e
    in 
	([e'], fl)
    end 
  | l2_list_to_ir1 fun_map var_map (e :: rest) = 
    let val (e', fl1) = l2_to_ir1 fun_map var_map e
        val (rest', fl2) = l2_list_to_ir1 fun_map var_map rest 
    in 
	(e' :: rest', fl1 @ fl2)
    end 

val init_var_map = [] 

val init_fun_map = [("print", IR1_KnownFunction), ("read", IR1_KnownFunction)]

fun translate e = 
    let val result = l2_to_ir1 init_fun_map init_var_map e
        val _ = if !Global.verbose then
                  (print "\n\nIR1 AST :\n";
                   pp_program result)
                else () 
    in
        result 
    end

