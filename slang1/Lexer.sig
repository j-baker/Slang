exception  LexerError of string; 

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

type lex_buffer 

val init_lex_buffer    : string -> lex_buffer    (* string is a filename *) 
val peek_next_token    : lex_buffer -> token
val consume_next_token : lex_buffer -> (lex_buffer * token) 


