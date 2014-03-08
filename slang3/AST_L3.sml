datatype type_expr =
         TEint
       | TEref of type_expr  
       | TEunit
       | TEbool
       | TEfunc of (type_expr list) * type_expr

type var = string

datatype oper = Plus | Mult | Subt | GTEQ | EQ 

datatype unary_oper = Neg | Not

datatype expr = 
         Skip
       | Integer of int
       | Boolean of bool
       | UnaryOp of unary_oper * expr
       | Op of expr * oper * expr
       | Assign of expr * expr 
       | Deref of expr              
       | Ref of expr 
       | Seq of expr * expr
       | If of expr * expr * expr
       | While of expr * expr
       | Var of var
       (* return type is added as an option *) 
       | Fn of var * type_expr * expr * (type_expr option) 
       | App of expr * expr list
       (* return type is added as an option *) 
       | Let of var * type_expr * expr * expr 
       (* an expression such as 

          Letrec (f, [(x1, t1), ...., (xn, tn)], t, e1, e2) 
           
           represents something like ML's 

           let fun f (x1 : t1, ..., xn : tn) : t = e1
           in e2 end 

        *) 
       | Letrec of var * (var * type_expr) list * type_expr * expr * expr

type program = expr

(* pretty printing using Mosml's PP module *) 

fun unary_to_string Neg = "-"
  | unary_to_string Not = "~"

fun op_to_string Plus = "+"
  | op_to_string Mult = "*"
  | op_to_string Subt = "-"
  | op_to_string GTEQ = ">="
  | op_to_string EQ = "="

fun type_expr_to_string (TEint) = "int"
  | type_expr_to_string (TEref t) = (type_expr_to_string t) ^ " ref"
  | type_expr_to_string (TEbool) = "bool"
  | type_expr_to_string (TEunit) = "unit"
  | type_expr_to_string (TEfunc ([t1], t2)) = 
     (type_expr_to_string_with_parens t1) ^ " -> " ^ (type_expr_to_string t2)
  | type_expr_to_string (TEfunc (tl, t)) = 
     "(" ^ (product_type_to_string tl) ^ ") -> " ^ (type_expr_to_string t)

and product_type_to_string [] = "" 
  | product_type_to_string  [t] = type_expr_to_string_with_parens t 
  | product_type_to_string  (t :: rest) = 
    (type_expr_to_string_with_parens t) ^ " * " ^ (product_type_to_string  rest)

and type_expr_to_string_with_parens (TEref t) = "(" ^ (type_expr_to_string t) ^ " ref)"
  | type_expr_to_string_with_parens (TEfunc (tl, t)) = 
     "((" ^ (product_type_to_string tl) ^ ") -> " ^ (type_expr_to_string t) ^ ")"
  | type_expr_to_string_with_parens t = type_expr_to_string t 

fun ppte pps t = PP.add_string pps (type_expr_to_string t)
                         
fun ppe pps (Integer n) = PP.add_string pps (Int.toString n)
  | ppe pps (Boolean b) = PP.add_string pps (Bool.toString b)
  | ppe pps (UnaryOp (uopr,e)) = 
      (
       PP.add_string pps "("; 
       PP.add_string pps (unary_to_string uopr); 
       ppe pps e;  
       PP.add_string pps ")"
      )
  | ppe pps (Op (e1,opr,e2)) = 
      (
       PP.add_string pps "("; 
       ppe pps e1; 
       PP.add_string pps (" " ^ (op_to_string opr) ^ " "); 
       ppe pps e2;  
       PP.add_string pps ")"
      )
  | ppe pps (If (e1, e2, e3)) = 
      (PP.begin_block pps PP.CONSISTENT 0; 
      PP.add_string pps "if "; 
      ppe pps e1; 
      PP.add_break pps (0, 0); 
      PP.add_string pps " then "; 
      ppe pps e2; 
      PP.add_break pps (0, 0); 
      PP.add_string pps " else "; 
      ppe pps e3;
      PP.end_block pps)
  | ppe pps (Deref e) = 
      (
       PP.add_string pps "!"; 
       ppe pps e
      )
  | ppe pps (Ref e) = 
      (
       PP.add_string pps "ref "; 
       ppe pps e
      )

  | ppe pps (Assign (e1, e2)) =  
       (
        ppe pps e1; 
        PP.add_string pps " := "; 
        ppe pps e2
       )
  | ppe pps (Skip) = PP.add_string pps "skip"	
  | ppe pps (Seq (e1,e2)) =  
       (PP.begin_block pps PP.CONSISTENT 0; 
        PP.add_string pps "("; 		 
        ppe pps e1; 
        PP.add_string pps "; "; 
        PP.add_break pps (0, 0); 	
	ppe pps e2;
        PP.add_string pps ")"; 		 
        PP.end_block pps)
  | ppe pps (While (e1,e2)) = 
      (PP.begin_block pps PP.CONSISTENT 0; 
       PP.add_string pps "while "; 
       ppe pps e1; 
       PP.add_string pps " do "; 
       PP.add_break pps (0, 0); 
       PP.begin_block pps PP.CONSISTENT 3;		
       PP.add_string pps "("; 
       ppe pps e2; 
       PP.add_string pps ")"; 
       PP.end_block pps;
       PP.end_block pps) 
  | ppe pps (Var v) = PP.add_string pps v
  | ppe pps (Fn (v, t, e, NONE)) =
    (PP.add_string pps "(fn ";
    PP.add_string pps v;
    PP.add_string pps " : ";
    ppte pps t;
    PP.add_string pps " => ";
    ppe pps e; 
    PP.add_string pps ")")
  | ppe pps (Fn (v, t, e, SOME t')) =
    (PP.add_string pps "(fn ";
    PP.add_string pps v;
    PP.add_string pps " : ";
    ppte pps t;
    PP.add_string pps " => ";
    ppe pps e; 
    PP.add_string pps " : ";
    ppte pps t';
    PP.add_string pps ")")
  | ppe pps (App (e, es)) =
    (ppe pps e; 
    PP.add_string pps " (";
    ppes pps es;
    PP.add_string pps ")")
  | ppe pps (Let (v, t, e1, e2)) =
    (PP.add_string pps "let ";
    PP.add_string pps v;
    PP.add_string pps " : ";
    ppte pps t;
    PP.add_string pps " = ";
    ppe pps e1;
    PP.add_string pps " in ";
    ppe pps e2; 
    PP.add_string pps " end")    
  | ppe pps (Letrec (v, vts, t, body, e)) =
    (PP.begin_block pps PP.CONSISTENT 0;
    PP.add_string pps "fun ";
    PP.add_string pps v;
    PP.add_string pps " (";
    ppvts pps vts;
    PP.add_string pps " ) : ";
    ppte pps t; 
    PP.add_string pps " = ";
       PP.add_break pps (0, 3); 	
       ppe pps body;
    PP.add_break pps (0, 0); 	
    PP.add_string pps " in ";
       PP.add_break pps (0, 3); 	
       ppe pps e; 
    PP.add_break pps (0, 0); 	
    PP.add_string pps " end"; 
    PP.end_block pps
    )

and ppes pps [] = ()
  | ppes pps [e] = ppe pps e
  | ppes pps (e::es) =
    (ppe pps e;
    PP.add_string pps ", ";
    ppes pps es)

and ppvts pps [] = ()
  | ppvts pps [(v,t)] =
    (PP.add_string pps v;
    PP.add_string pps ":";
    ppte pps t)
  | ppvts pps ((v,t)::vts) =
    (PP.add_string pps v;
    PP.add_string pps ":";
    ppte pps t;
    PP.add_string pps ", ";
    ppvts pps vts)

val pp_type_exp  = Library.mk_pp ppte
val pp_expr      = Library.mk_pp ppe 



