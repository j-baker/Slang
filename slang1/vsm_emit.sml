open AST_vsm_bytecode; 

val vsm_version = 0 
val vsm_instruction_limit = 256 

exception VSM_TooManyInstructions of string 

fun bout stm w = BinIO.output1(stm, w) 
fun lbout stm = List.app (bout stm) 
val fromInt = Word8.fromInt

fun emit_vsm_header stm (vsm_version, inst_count) = 
    lbout stm [fromInt vsm_version, fromInt inst_count] 

(* opcodes must be consistent with vsm/vsm.0/vsm.h *) 
fun emit_vsm_bcode stm VSM_B_Nop        = bout stm (fromInt 1) 
  | emit_vsm_bcode stm VSM_B_Pop        = bout stm (fromInt 5)
  | emit_vsm_bcode stm VSM_B_Add        = bout stm (fromInt 6)
  | emit_vsm_bcode stm VSM_B_Sub        = bout stm (fromInt 7)
  | emit_vsm_bcode stm VSM_B_Mul        = bout stm (fromInt 8)
  | emit_vsm_bcode stm VSM_B_Hlt        = bout stm (fromInt 9)  
  | emit_vsm_bcode stm VSM_B_Pri        = bout stm (fromInt 14)
  | emit_vsm_bcode stm VSM_B_Prb        = bout stm (fromInt 15)
  | emit_vsm_bcode stm (VSM_B_Push c)   = lbout stm [fromInt 2, c]
  | emit_vsm_bcode stm (VSM_B_Load dl)  = lbout stm [fromInt 3, dl] 
  | emit_vsm_bcode stm (VSM_B_Store dl) = lbout stm [fromInt 4, dl] 
  | emit_vsm_bcode stm (VSM_B_Jmp cl)   = lbout stm [fromInt 10, cl] 
  | emit_vsm_bcode stm (VSM_B_Ifz dl)   = lbout stm [fromInt 11, dl] 
  | emit_vsm_bcode stm (VSM_B_Ifp dl)   = lbout stm [fromInt 12, dl] 
  | emit_vsm_bcode stm (VSM_B_Ifn dl)   = lbout stm [fromInt 13, dl] 

fun emit_vsm_bytecode f (inst_count, code) = 
    if inst_count > vsm_instruction_limit 
    then raise (VSM_TooManyInstructions (Int.toString inst_count))
    else 
    let val stm = BinIO.openOut f
        val _ = emit_vsm_header stm (vsm_version, inst_count) 
        val _ = List.app (emit_vsm_bcode stm) code 
        val _ = BinIO.flushOut stm 
    in 
        BinIO.closeOut stm 
    end 

