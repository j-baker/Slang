open AST_vrm_bytecode; 

val vrm_version = 0 
val vrm_instruction_limit = 256 

exception VRM_TooManyInstructions of string 

fun bout stm w = BinIO.output1(stm, w) 
fun lbout stm = List.app (bout stm) 
val fromInt = Word8.fromInt

fun emit_vrm_header stm (vrm_version, inst_count) = 
    lbout stm [fromInt vrm_version, fromInt inst_count] 

(* opcodes must be consistent with vrm/vrm.0/vrm.h *) 
fun emit_vrm_bcode stm VRM_B_Hlt                   = bout stm (fromInt 7) 
  | emit_vrm_bcode stm VRM_B_Nop                   = bout stm (fromInt 1) 
  | emit_vrm_bcode stm (VRM_B_Jmp cl)              = lbout stm [fromInt 8, cl] 
  | emit_vrm_bcode stm (VRM_B_Set (dl, c))         = lbout stm [fromInt 2, dl, c] 
  | emit_vrm_bcode stm (VRM_B_Mov (dl1, dl2))      = lbout stm [fromInt 3, dl1, dl2] 
  | emit_vrm_bcode stm (VRM_B_Add (dl1, dl2, dl3)) = lbout stm [fromInt 4, dl1, dl2, dl3] 
  | emit_vrm_bcode stm (VRM_B_Sub (dl1, dl2, dl3)) = lbout stm [fromInt 5, dl1, dl2, dl3] 
  | emit_vrm_bcode stm (VRM_B_Mul (dl1, dl2, dl3)) = lbout stm [fromInt 6, dl1, dl2, dl3] 
  | emit_vrm_bcode stm (VRM_B_Ifz (dl, cl))        = lbout stm [fromInt 9, dl, cl] 
  | emit_vrm_bcode stm (VRM_B_Ifp (dl, cl))        = lbout stm [fromInt 10, dl, cl] 
  | emit_vrm_bcode stm (VRM_B_Ifn (dl, cl))        = lbout stm [fromInt 11, dl, cl] 
  | emit_vrm_bcode stm (VRM_B_Pri dl)              = lbout stm [fromInt 12, dl] 
  | emit_vrm_bcode stm (VRM_B_Prb dl)              = lbout stm [fromInt 13, dl] 
  | emit_vrm_bcode stm (VRM_B_Rdi dl)              = lbout stm [fromInt 14, dl] 
  | emit_vrm_bcode stm (VRM_B_Rdb dl)              = lbout stm [fromInt 15, dl] 

fun emit_vrm_bytecode f (inst_count, code) = 
    if inst_count > vrm_instruction_limit
    then raise (VRM_TooManyInstructions (Int.toString inst_count))
    else 
    let val stm = BinIO.openOut f
        val vrm_version = 0 
        val _ = emit_vrm_header stm (vrm_version, inst_count) 
        val _ = List.app (emit_vrm_bcode stm) code 
        val _ = BinIO.flushOut stm 
    in 
        BinIO.closeOut stm 
    end 

