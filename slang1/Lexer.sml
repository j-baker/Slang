structure Lexer :> Lexer = struct 
(* 
This is a simple lexer for Slang.1.  It is written by 
hand with an emphasis on clarity and not efficiency. 

We will get more sophisticated in later versions of Slang. 

*) 

exception LexerError of string 

fun lex_error s = raise (LexerError s) 

datatype lex_buffer = LexBuffer of {
  lexBuffer : string, (* the entire input file! *) 
  lexPosition : int, 
  lexSize : int
} 

fun at_eof (LexBuffer lex_buf) = ((#lexPosition lex_buf) = (#lexSize lex_buf))

fun current_char (LexBuffer lex_buf) = String.sub(#lexBuffer lex_buf, #lexPosition lex_buf)

fun advance_pos n (LexBuffer lex_buf) = 
  LexBuffer {lexBuffer = #lexBuffer lex_buf, 
         lexPosition = n + (#lexPosition lex_buf), 
         lexSize = #lexSize lex_buf} 
   
datatype token = 
     Teof               (* end-of-file *) 
   | Tint of int        (* integer     *) 
   | Tident of string   (* identifier  *) 
   | Ttrue              (* true        *) 
   | Tfalse             (* false       *) 
   | Tright_paren       (* )           *) 
   | Tleft_paren        (* (           *) 
   | Tsemi              (* ;           *) 
   | Tplus              (* +           *) 
   | Tstar              (* *           *) 
   | Tminus             (* -           *) 
   | Tnot               (* ~           *) 
   | Tgets              (* :=          *) 
   | Tgteq              (* >=          *) 
   | Tset               (* set         *) 
   | Tskip              (* skip        *) 
   | Tbegin             (* begin       *) 
   | Tend               (* end         *) 
   | Tif                (* if          *) 
   | Tthen              (* then        *) 
   | Telse              (* else        *) 
   | Twhile             (* while       *) 
   | Tdo                (* do          *) 
   | Tprint             (* print       *) 

fun str_to_token s = 
    case s of 
      "if"    => Tif
    | "then"  => Tthen
    | "else"  => Telse 
    | "while" => Twhile
    | "do"    => Tdo
    | "begin" => Tbegin
    | "end"   => Tend 
    | "set"   => Tset 
    | "skip"   => Tskip 
    | "print" => Tprint
    |  _      => Tident s

fun get_integer (lex_buf, dl) = 
      if Char.isDigit (current_char lex_buf) 
      then get_integer (advance_pos 1 lex_buf, (current_char lex_buf)::dl)
      else let val str = String.implode (List.rev dl)
           in case Int.fromString str of 
                SOME n => (lex_buf, Tint n) 
              | NONE => lex_error ("internal lex error : the string " ^ str ^ " should represent an integer")
           end 
 
fun get_alpha (lex_buf, str) = 
      if Char.isAlphaNum (current_char lex_buf) 
      then get_alpha (advance_pos 1 lex_buf, str ^ (String.implode[current_char lex_buf]))
      else (lex_buf, str_to_token str) 

fun get_longest_match lex_buf = 
       case current_char lex_buf of 
         #")" => (advance_pos 1 lex_buf, Tright_paren) 
       | #"(" => (advance_pos 1 lex_buf, Tleft_paren)
       | #";" => (advance_pos 1 lex_buf, Tsemi) 
       | #"+" => (advance_pos 1 lex_buf, Tplus) 
       | #"*" => (advance_pos 1 lex_buf, Tstar) 
       | #"-" => (advance_pos 1 lex_buf, Tminus) 
       | #"~" => (advance_pos 1 lex_buf, Tnot) 
       | #":" => (case current_char (advance_pos 1 lex_buf) of 
                  #"=" => (advance_pos 2 lex_buf, Tgets) 
                | _ => lex_error "expecting '=' after ':'")
       | #">" => (case current_char (advance_pos 1 lex_buf) of 
                  #"=" => (advance_pos 2 lex_buf, Tgteq) 
                | _ => lex_error "expecting '=' after '>'")
       | c => if Char.isDigit c 
              then get_integer (advance_pos 1 lex_buf, [c]) 
              else if Char.isAlpha c 
                   then get_alpha (advance_pos 1 lex_buf, String.implode [c]) 
                   else lex_error ("unexpected character : " ^ (String.implode [c])) 

fun ignore_comment lex_buf = 
   if at_eof lex_buf
   then lex_buf 
   else case current_char lex_buf of 
          #"\n" => ignore_whitespace (advance_pos 1 lex_buf)
        | _     => ignore_comment (advance_pos 1 lex_buf)

and ignore_whitespace lex_buf = 
   if at_eof lex_buf
   then lex_buf 
   else case current_char lex_buf of 
          #" "  => ignore_whitespace (advance_pos 1 lex_buf)
        | #"\n" => ignore_whitespace (advance_pos 1 lex_buf)
        | #"\t" => ignore_whitespace (advance_pos 1 lex_buf)
        | #"%"  => ignore_comment    (advance_pos 1 lex_buf)
        | _     => lex_buf 

fun consume_next_token lex_buf = 
    let val lex_buf1 = ignore_whitespace lex_buf 
    in 
        if at_eof lex_buf1 
    	then (lex_buf1, Teof) 
	else get_longest_match lex_buf1 
    end 

fun peek_next_token lex_buf = 
    let val lex_buf1 = ignore_whitespace lex_buf 
    in 
        if at_eof lex_buf1
    	then Teof
	else let val (_, tok) = get_longest_match lex_buf1 in tok end 
    end 

fun init_lex_buffer f = 
    let val instr = TextIO.openIn f 
        val buf   = TextIO.inputAll instr
        val  _    = TextIO.closeIn instr 
    in
      LexBuffer {lexBuffer = buf, lexPosition = 0, lexSize = String.size buf} 
    end

(* for testing ... *) 
fun file_to_token_list f = 
    let fun aux carry lex_buf= 
            let val (lex_buf1, tok) = consume_next_token lex_buf 
            in if tok = Teof 
               then List.rev (tok :: carry)
               else aux (tok :: carry) lex_buf1 
            end 
    in 
       aux [] (init_lex_buffer f) 
    end 

end (* end of structure *) 