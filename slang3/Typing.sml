
open AST_L3; 

exception TypeError of string 

fun type_error s = raise (TypeError s) 

fun lookup [] n = type_error ("variable " ^ n ^ " not bound")
  | lookup ((m, t) :: rest) n = if n = m then t else lookup rest n 

(* 
   If ccs env e = (t, e') then e has type t (an exception is raised in case of 
   ill-typed terms).  expression e' is e annotated with type informaton. 
   For L2,  e = e'. Later versions might be more interesting. 
*) 
fun css env Skip = (TEunit, Skip) 
  | css env (Boolean b) = (TEbool, Boolean b)  
  | css env (Integer n) = 
    (* 
      In vrm.0, constants must be in range –128 to +127  in order to fit into one (signed) byte. 
      However, the negation symbol "-" is a part of the Slang language, so any "naked" integer 
      constant will be between 0 and 127. 
    *) 
    if  0 <= n andalso n <= 127 
    then (TEint, Integer n)  
    else type_error("vm targets requires all integer constants to be in range 0 to 127") 
  | css env (UnaryOp (Neg,Integer n)) = 
    if  0 <= n andalso n <= 128
    then (TEint, UnaryOp (Neg,Integer n))  
    else type_error("vrm.0 target requires all negated integer constants to be in range 0 to 128") 
  | css env (UnaryOp (Neg,e)) = 
     let val (t, e') = css env e
     in 
        if t = TEint 
        then (t, UnaryOp (Neg,e')) 
        else type_error ("negative applied to expression of type " ^ (type_expr_to_string t))
     end 
  | css env (UnaryOp (Not,e)) = 
     let val (t, e') = css env e
     in 
        if t = TEbool 
        then (t, UnaryOp (Not,e')) 
        else type_error ("negation applied to expression of type " ^ (type_expr_to_string t))
     end 
  | css env (Op (e1,GTEQ,e2)) = 
     let val (t1, e1') = css env e1 
         val (t2, e2') = css env e2
     in 
        if t1 = TEint 
        then if t2 = TEint 
             then (TEbool, Op (e1',GTEQ,e2'))
             else type_error ("second expression of >= has type " ^ (type_expr_to_string t2))
        else type_error ("first expression of >= has type " ^ (type_expr_to_string t1))
     end 
  | css env (Op (e1,EQ,e2)) = 
     let val (t1, e1') = css env e1 
         val (t2, e2') = css env e2
     in 
        if t1 = TEint 
        then if t2 = TEint 
             then (TEbool, Op (e1',EQ, e2'))
             else type_error ("second expression of = has type " ^ (type_expr_to_string t2))
        else type_error ("first expression of = has type " ^ (type_expr_to_string t1))
     end 
  | css env (Op (e1,opr,e2)) =
     let val (t1, e1') = css env e1
         val (t2, e2') = css env e2 
     in 
        if t1 = TEint 
        then if t2 = TEint 
             then (TEint, Op (e1', opr, e2'))
             else type_error ("second expression of " ^ (op_to_string opr) ^ " has type " ^ (type_expr_to_string t2))
        else type_error ("first expression of " ^ (op_to_string opr) ^ " has type " ^ (type_expr_to_string t1))
     end 
  | css env (If (e1,e2,e3)) = 
     let val (t1, e1') = css env e1
         val (t2, e2') = css env e2
         val (t3, e3') = css env e3
     in 
        if t1 = TEbool 
        then if t2 = t3 
             then (t2, If (e1', e2', e3'))
             else type_error ("then branch of type " ^ (type_expr_to_string t2) ^ " while else branch of type " ^ (type_expr_to_string t3))
        else type_error ("condition of 'if' has type " ^ (type_expr_to_string t1))
     end 
  | css env (Deref e) = 
        let val (t, e') = css env e 
        in case t of 
             TEref t' => (t', Deref e')
           | _ => type_error ("ref expects a ref type, but found type " ^ (type_expr_to_string t))
        end 
  | css env (Ref e) = 
        let val (t, e') = css env e 
        in 
           (TEref t, Ref e')
       end 
  | css env (Assign (e1, e2)) = 
        let val (t1, e1') = css env e1 
            val (t2, e2') = css env e2 
        in 
          case t1 of 
             TEref t1' => 
               if t2 = t1' 
               then (TEunit, Assign (e1', e2'))
               else type_error ("right-hand side of assignment has type " 
                                ^ (type_expr_to_string t2) ^ 
                                ", but expecting type " 
                                ^ (type_expr_to_string t1')
                                )
            | _ => type_error ("left-hand side of := should have ref type, found " 
                             ^ (type_expr_to_string t1))
       end 
  | css env (Seq (e1,e2)) = 
     let val (t1, e1') = css env e1 
         val (t2, e2') = css env e2 
     in 
        if t1 = TEunit
        then (t2, Seq(e1', e2'))
        else type_error ("sequence expression of has type " ^ (type_expr_to_string t1))
     end 
  | css env (While (e1,e2)) = 
     let val (t1, e1') = css env e1
         val (t2, e2') = css env e2
     in 
         (TEunit, While(e1', e2'))
     end 
  (* new to L2 *) 
  | css env (Var v) = (lookup env v, Var v)
  | css env (Fn (v, t, e, _)) = 
    let val (t', e') = css ((v, t) :: env) e
    in 
        (TEfunc([t], t'), Fn (v, t,  e', SOME t'))
    end 
  | css env (App (e, el)) = 
    let val l = List.map (css env) el 
        val (t', e') = css env e
    in case t' of 
        TEfunc (argl, t) => 
         let fun match_arg_types [] [] = [] 
               | match_arg_types _  [] = type_error ("function applied to too few arguments") 
               | match_arg_types [] _  = type_error ("function applied to too many arguments") 
               | match_arg_types (tf :: tl) ((ta, e) :: al) = 
                 if tf = ta 
                 then e :: (match_arg_types tl al) 
                 else type_error ("application"
                                  ^ " expecting argument of type " 
                                  ^ (type_expr_to_string (tf)) 
                                  ^ " but found argument is of type " 
                                  ^ (type_expr_to_string ta))
         in 
            (* the "unit type" here is something of a hack *) 
            if argl = [TEunit] 
            then if l = [] then (t, App (e', [])) else type_error ("function application expecting ()")
            else (t, App (e', match_arg_types argl l))
         end 
      | _ => type_error ("non-functional expression applied")
    end 
  | css env (Let (x, td, e1, e2)) = 
       let val new_env = (x, td) :: env 
       in let val (ta, e1') = css env e1
              and (t, e2') = css new_env e2
          in 
              if td = ta 
              then (t, Let (x, td, e1', e2'))
              else type_error ("variable " ^ x 
                                  ^ " delared with type " 
                                  ^ (type_expr_to_string (td)) 
                                  ^ " but is of type " 
                                  ^ (type_expr_to_string ta))
          end 
       end 
  | css env (Letrec (f, argl, tr, e1, e2)) = 
    let fun complain_of_duplicates [] = () 
          | complain_of_duplicates (x ::rest) = 
            if List.exists (fn y => x = y) rest 
            then type_error ("in declaration of function " ^ f ^ " the variable " ^ x ^ " is duplicated")
            else complain_of_duplicates rest
    in let val _ = complain_of_duplicates (f :: (List.map (fn (x, _) => x) argl))
           val args_type = if argl = [] then [TEunit] else List.map (fn (_, y) => y) argl 
           val new_env = (f, TEfunc(args_type , tr)) :: (List.map (fn (x, t) => (x, t)) argl) @ env
           val (ta, e1') = css new_env e1 (* functions can be recursive *) 
                 and (t, e2')   = css new_env e2
       in 
                 if tr = ta 
                 then (t, Letrec (f, argl, tr, e1', e2'))
                 else type_error ("return of function " ^ f
                                  ^ " delared as type" 
                                  ^ (type_expr_to_string (tr)) 
                                  ^ " but is of type " 
                                  ^ (type_expr_to_string ta))
       end 
   end 

val initial_env = [("print", TEfunc([TEint],TEunit)), 
                   ("read",  TEfunc([TEunit],TEint))
                  ]

fun check_types e = 
    let val (_, result) = css initial_env e 
        val _ = if !Global.verbose
                then (print "\nProgram is type correct!\n"; 
                      print "\nL2 AST :\n";
                      AST_L3.pp_expr result
                     )
                else () 
    in result end 

