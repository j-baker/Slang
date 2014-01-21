open AST_L1; 
open Lexer; 

(*

This concrete syntax is designed to make recursive descent 
parsing as easy as falling off a log.  We will get more 
sofisticated in later versions of Slang. 

lexical matters: an "identifier" starts with a letter 
(A-Z, a-z) followed by a sequence of zero or more 
letters or digits (0-9). 

An "integer" is a sequence of 1 or more digits. 

Comments start anywhere with a "%" and consume 
the remainder of the line. 

Slang.1 Grammar: 

program := expr EOF 

expr := 
  simple
| set identifier := expr                      
| while expr do expr 
| if expr then expr else expr 
| begin expr expr_list 

expr_list := ; expr expr_list 
           | end 
               
simple ::= term srest

term ::= factor trest

srest ::=  + term srest
        |  – term srest
        |  >= term srest 
        | 

trest ::=  *  factor trest
           |  

factor :=               
  identifier 
| integer 
| - expr 
| ~ expr 
| true                                   
| false                                   
| skip 
| ( expr )
| print expr 
*) 

(* structure Parser = struct *) 

exception ParseError of string 

fun parse_error s = raise (ParseError s) 

datatype trest  = Trest of oper * factor * trest | Trest_null 
     and srest  = Srest of oper * term * srest | Srest_null 
     and factor = Fident of string 
                | Fint of int 
                | Fbool of bool 
                | Fexpr of expr 
                | Fneg of expr 
                | Fnot of expr 
                | Fprint of expr 
                | Fskip
     and term   = Term of factor * trest 

type simple = term * srest 

fun factor_to_expr (Fident i) = Deref i 
  | factor_to_expr (Fint i) = Integer i 
  | factor_to_expr (Fbool b) = Boolean b
  | factor_to_expr (Fexpr e) = e 
  | factor_to_expr (Fneg e) = UnaryOp(Neg, e)  
  | factor_to_expr (Fnot e) = UnaryOp(Not, e)  
  | factor_to_expr (Fprint e) = Print e
  | factor_to_expr (Fskip) = Skip 

fun simple_to_expr(Term(f, Trest_null), Srest_null)  = 
       factor_to_expr f
  | simple_to_expr(Term(f1, Trest_null), Srest(opr, f2, sr)) = 
       Op(factor_to_expr f1, opr, simple_to_expr(f2, sr))
  | simple_to_expr(Term(f1, Trest(opr, f2, tr)), Srest_null) = 
       Op(factor_to_expr f1, opr, term_to_expr((f2, tr)))
  | simple_to_expr(Term(f1, Trest(opr1, f2, tr)), Srest(opr2, f3, sr))  = 
       Op(Op(factor_to_expr f1, opr1, term_to_expr(f2, tr)), opr2, simple_to_expr(f3, sr))

and term_to_expr (f, Trest_null) = 
       factor_to_expr f
  | term_to_expr (f1, Trest(opr, f2, tr)) = 
       Op(factor_to_expr f1, opr, term_to_expr(f2, tr))

fun parse_id lex_buf =
    let val (lex_buf1, next_token) = consume_next_token lex_buf 
    in 
        case next_token of 
          Tident i => (lex_buf1 , i) 
        | _ => parse_error "expecting identifier\n" 
    end 

fun parse_gets lex_buf =
    let val (lex_buf1, next_token) = consume_next_token lex_buf 
    in 
        case next_token of 
          Tgets => lex_buf1 
        | _ => parse_error "expecting := \n" 
    end 

fun parse_do lex_buf =
    let val (lex_buf1, next_token) = consume_next_token lex_buf 
    in 
        case next_token of 
          Tdo => lex_buf1 
        | _ => parse_error "expecting 'do' \n" 
    end 

(* fun parse_od lex_buf =
    let val (lex_buf1, next_token) = consume_next_token lex_buf 
    in 
        case next_token of 
          Tod => lex_buf1 
        | _ => parse_error "expecting 'od' \n" 
    end 
fun parse_fi lex_buf =
    let val (lex_buf1, next_token) = consume_next_token lex_buf 
    in 
        case next_token of 
          Tfi => lex_buf1 
        | _ => parse_error "expecting 'fi' \n" 
    end 
*) 


fun parse_then lex_buf = 
    let val (lex_buf1, next_token) = consume_next_token lex_buf 
    in 
        case next_token of 
          Tthen => lex_buf1 
        | _ => parse_error "expecting 'then' \n" 
    end 


fun parse_else lex_buf =
    let val (lex_buf1, next_token) = consume_next_token lex_buf 
    in 
        case next_token of 
          Telse => lex_buf1 
        | _ => parse_error "expecting 'else' \n" 
    end 

fun parse_end lex_buf =
    let val (lex_buf1, next_token) = consume_next_token lex_buf 
    in 
        case next_token of 
          Tend => lex_buf1 
        | _ => parse_error "expecting 'end' \n" 
    end 

fun parse_semi lex_buf =
    let val (lex_buf1, next_token) = consume_next_token lex_buf 
    in 
        case next_token of 
          Tsemi => lex_buf1 
        | _ => parse_error "expecting ';' \n" 
    end 

fun parse_right_paren lex_buf =
    let val (lex_buf1, next_token) = consume_next_token lex_buf 
    in 
        case next_token of 
          Tright_paren => lex_buf1 
        | _ => parse_error "expecting ')' \n" 
    end 

fun parse_expr lex_buf = 
    let val (lex_buf1, next_token) = consume_next_token lex_buf 
    in 
        case next_token of 
           Tset   => let val (lex_buf2, id) = parse_id lex_buf1
                         val lex_buf3 = parse_gets lex_buf2 
                         val (lex_buf4, e) = parse_expr lex_buf3
                     in 
		         (lex_buf4, Assign(id, e)) 
                     end 
         | Twhile => let val (lex_buf2, e1) = parse_expr lex_buf1
                         val lex_buf3 = parse_do lex_buf2 
                         val (lex_buf4, e2) = parse_expr lex_buf3
                     in 
		         (lex_buf4, While(e1, e2)) 
                     end 
         | Tif    => let val (lex_buf2, e1) = parse_expr lex_buf1
                         val lex_buf3 = parse_then lex_buf2 
                         val (lex_buf4, e2) = parse_expr lex_buf3
                         val lex_buf5 = parse_else lex_buf4 
                         val (lex_buf6, e3) = parse_expr lex_buf5
                     in 
		         (lex_buf6, If(e1, e2, e3)) 
                     end 
         | Tbegin => let val (lex_buf2, e1) = parse_expr lex_buf1
                         val (lex_buf3, e_opt) = parse_expr_list lex_buf2 
                     in 
		         case e_opt of 
                           SOME e2 => (lex_buf3, Seq(e1, e2)) 
                         | NONE    => (lex_buf3, e1) 
		         
                     end 
         | _      => parse_simple lex_buf 

    end 

and parse_expr_list lex_buf = 
    let val (lex_buf1, next_token) = consume_next_token lex_buf 
    in 
        case next_token of 
          Tsemi  => let val (lex_buf2, e1) = parse_expr lex_buf1
	                val (lex_buf3, e_opt) = parse_expr_list lex_buf2
                    in case e_opt of 
                         SOME e2 => (lex_buf3, SOME (Seq(e1, e2)))
                       | NONE    => (lex_buf3, SOME e1) 
                    end 
        | Tend   => (lex_buf1, NONE)
        | _      => parse_error "expecting expr list or 'end'"
    end 

and parse_simple lex_buf = 
    let val (lex_buf1, t) = parse_term lex_buf 
        val (lex_buf2, sr) = parse_srest lex_buf1
    in (lex_buf2, simple_to_expr(t, sr)) end  

and parse_term lex_buf = 
    let val (lex_buf1, f) = parse_factor lex_buf 
        val (lex_buf2, tr) = parse_trest lex_buf1
    in (lex_buf2, Term(f, tr)) end  

and parse_srest lex_buf = 
    case peek_next_token lex_buf of 
      Tplus => let val (lex_buf1, _) = consume_next_token lex_buf
                   val (lex_buf2, t) = parse_term lex_buf1
                   val (lex_buf3, sr) = parse_srest lex_buf2
               in 
                  (lex_buf3, Srest(Plus, t, sr))
               end 
    | Tminus => let val (lex_buf1, _) = consume_next_token lex_buf
                    val (lex_buf2, t) = parse_term lex_buf1
                    val (lex_buf3, sr) = parse_srest lex_buf2
                in 
                   (lex_buf3, Srest(Subt, t, sr))
                end 
    | Tgteq => let val (lex_buf1, _) = consume_next_token lex_buf
                   val (lex_buf2, t) = parse_term lex_buf1
                   val (lex_buf3, sr) = parse_srest lex_buf2
                in 
                   (lex_buf3, Srest(GTEQ, t, sr))
                end 
    | _ => (lex_buf, Srest_null) 

and parse_trest lex_buf = 
    case peek_next_token lex_buf of 
      Tstar => let val (lex_buf1, _) = consume_next_token lex_buf
                   val (lex_buf2, f) = parse_factor lex_buf1
                   val (lex_buf3, tr) = parse_trest lex_buf2
                in 
                   (lex_buf3, Trest(Mult, f, tr))
                end 
    | _ => (lex_buf, Trest_null) 

and parse_factor lex_buf = 
    let val (lex_buf1, next_token) = consume_next_token lex_buf 
    in 
       case next_token of 
          Tident i => (lex_buf1, Fident i)       
       |  Tint n   => (lex_buf1, Fint n)       
       |  Tminus   =>  
           let val (lex_buf2, e) = parse_expr lex_buf1   
           in 	   
               (lex_buf2, Fneg e)
           end 
       |  Tnot   =>  
           let val (lex_buf2, e) = parse_expr lex_buf1   
           in 	   
               (lex_buf2, Fnot e)
           end 
       |  Ttrue    => (lex_buf1, Fbool true)       
       |  Tfalse   => (lex_buf1, Fbool false)       
       |  Tskip    => (lex_buf1, Fskip)       
       |  Tleft_paren  => 
           let val (lex_buf2, e) = parse_expr lex_buf1   
               val lex_buf3 = parse_right_paren lex_buf2
           in 	   
               (lex_buf3, Fexpr e)
           end 
       |  Tprint  => 
           let val (lex_buf2, e) = parse_expr lex_buf1
           in (lex_buf2, Fprint e) end 	   
       | _ => parse_error "expecting a factor"
    end 

fun parse lex_buf = 
    let val (lex_buf1, e) = parse_expr lex_buf 
        val (_, next_token) = consume_next_token lex_buf1 
    in 
       case next_token of 
       Teof => e 
       | _ => parse_error "expecting end-of-file\n" 
    end

fun parse_file f = 
    let val result = parse (init_lex_buffer f) 
        val _ = if !Global.verbose
                   then (print "AST (in L1) = \n"; 
                         pp_expr result)
                   else () 
    in result end 

