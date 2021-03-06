open AST_vsm_assembler; 
open AST_vsm_bytecode; 




fun mk_vsm_code_loc_map prog = 
    let fun aux lmap m rprog []                       = (m, lmap, List.rev rprog)
          | aux lmap m rprog ((VSM_Comment _)::rest)  = aux lmap m rprog rest
          | aux lmap m rprog ((VSM_Label l)::rest)    = aux ((l, m) :: lmap) m rprog rest
          | aux lmap m rprog (opr :: rest)            = aux lmap (m + 1) (opr :: rprog) rest
    in aux [] 0 [] prog end 

val convert_int = Word8.fromInt

fun find [] l = Library.internal_error ("unmatched label in vsm_assemble.sml: " ^ l)
  | find ((a, v) :: rest) l = if a = l then (Word8.fromInt v) else find rest l

fun vsm_assemble_op lmap (VSM_Comment _)    = 
        Library.internal_error "found VSM_Comment in cleaned code"
  | vsm_assemble_op lmap (VSM_Label _)      = 
        Library.internal_error "found VSM_Label in cleaned code"
  | vsm_assemble_op lmap VSM_Nop                  = VSM_B_Nop
  | vsm_assemble_op lmap (VSM_Push c)             = VSM_B_Push(convert_int c)
  | vsm_assemble_op lmap (VSM_PushFun cl)         = VSM_B_PushFun(find lmap cl)
  | vsm_assemble_op lmap (VSM_Load offset)        = VSM_B_Load(convert_int offset)
  | vsm_assemble_op lmap (VSM_Store offset)       = VSM_B_Store(convert_int offset)
  | vsm_assemble_op lmap (VSM_AllocateLocals n)   = VSM_B_AllocateLocals(convert_int n)
  | vsm_assemble_op lmap VSM_Pop                  = VSM_B_Pop
  | vsm_assemble_op lmap VSM_Add                  = VSM_B_Add
  | vsm_assemble_op lmap VSM_Sub                  = VSM_B_Sub
  | vsm_assemble_op lmap VSM_Mul                  = VSM_B_Mul
  | vsm_assemble_op lmap VSM_Hlt                  = VSM_B_Hlt 
  | vsm_assemble_op lmap (VSM_Jmp cl)             = VSM_B_Jmp(find lmap cl)
  | vsm_assemble_op lmap (VSM_Ifz cl)             = VSM_B_Ifz(find lmap cl)
  | vsm_assemble_op lmap (VSM_Ifp cl)             = VSM_B_Ifp(find lmap cl)
  | vsm_assemble_op lmap (VSM_Ifn cl)             = VSM_B_Ifn(find lmap cl)
  | vsm_assemble_op lmap VSM_Pri                  = VSM_B_Pri
  | vsm_assemble_op lmap VSM_Rdi                  = VSM_B_Rdi
  | vsm_assemble_op lmap VSM_Assign               = VSM_B_Assign
  | vsm_assemble_op lmap (VSM_Deref c)            = VSM_B_Deref (convert_int c)
  | vsm_assemble_op lmap VSM_Ref                  = VSM_B_Ref
  | vsm_assemble_op lmap (VSM_Call cl)            = VSM_B_Call(find lmap cl)
  | vsm_assemble_op lmap (VSM_Call_Closure n)     = VSM_B_Call_Closure (convert_int n)
  | vsm_assemble_op lmap (VSM_Return n)           = VSM_B_Return (convert_int n)
  | vsm_assemble_op lmap (VSM_Arg_Load fo)        = VSM_B_Arg_Load(convert_int fo)
  | vsm_assemble_op lmap (VSM_Closure (cl, c))    = VSM_B_Closure (find lmap cl, convert_int c)

fun vsm_assemble prog = 
    let val (inst_count, lmap, clean_prog) = mk_vsm_code_loc_map prog 
        val code = List.map (vsm_assemble_op lmap) clean_prog 
        val _ = if !Global.verbose
                then (print "Intruction count = "; 
                      print (Int.toString inst_count); 
                      print "\nVSM code: \n";  
                      print ((vsm_bytecode_program_to_string code) ^ "\n"))
                else () 
    in (inst_count, code) end 

fun vsm_peep_hole prog = 
    let fun aux carry [] = List.rev carry 
          | aux carry ((VSM_Push _) :: VSM_Pop :: rest) = aux carry rest 
          | aux carry (a :: rest) = aux (a :: carry) rest 
       val code = aux [] prog 
       val _ = if !Global.verbose
                then (print "\nVSM code after peep-hole : \n";  
                      print ((vsm_program_to_string code) ^ "\n"))
                else () 
    in code end 




