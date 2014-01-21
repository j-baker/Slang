
type vsm_data_loc = Word8.word; 
type vsm_code_loc = Word8.word; 
type vsm_constant = Word8.word; 

datatype vsm_b_operation = 
       (* data operations *) 
         VSM_B_Nop 
       | VSM_B_Push of vsm_constant 
       | VSM_B_Load of vsm_data_loc
       | VSM_B_Store of vsm_data_loc
       | VSM_B_Pop
       | VSM_B_Add 
       | VSM_B_Sub 
       | VSM_B_Mul 
       (* control flow operations *) 
       | VSM_B_Hlt 
       | VSM_B_Jmp of vsm_code_loc
       | VSM_B_Ifz of vsm_code_loc
       | VSM_B_Ifp of vsm_code_loc
       | VSM_B_Ifn of vsm_code_loc
       (* input/output *) 
       | VSM_B_Pri 
       | VSM_B_Prb 

type vsm_b_vrm_program = vsm_b_operation list 

fun vsm_data_loc_to_string b = "s" ^ (Int.toString (Word8.toInt b))
fun vsm_code_loc_to_string b = "l" ^ (Int.toString (Word8.toInt b))
fun vsm_constant_to_string b = Int.toString (Word8.toIntX b)

fun vsm_b_operation_to_string VSM_B_Hlt                   = "hlt" 
  | vsm_b_operation_to_string VSM_B_Nop                   = "nop" 
  | vsm_b_operation_to_string VSM_B_Pop                   = "pop" 
  | vsm_b_operation_to_string VSM_B_Add                   = "add" 
  | vsm_b_operation_to_string VSM_B_Sub                   = "sub" 
  | vsm_b_operation_to_string VSM_B_Mul                   = "mul" 
  | vsm_b_operation_to_string VSM_B_Pri                   = "pri" 
  | vsm_b_operation_to_string VSM_B_Prb                   = "prb" 
  | vsm_b_operation_to_string (VSM_B_Jmp cl)              = "jmp " ^ (vsm_code_loc_to_string cl) 
  | vsm_b_operation_to_string (VSM_B_Ifz cl)              = "ifz " ^ (vsm_code_loc_to_string cl) 
  | vsm_b_operation_to_string (VSM_B_Ifp cl)              = "ifp " ^ (vsm_code_loc_to_string cl) 
  | vsm_b_operation_to_string (VSM_B_Ifn cl)              = "ifn " ^ (vsm_code_loc_to_string cl) 
  | vsm_b_operation_to_string (VSM_B_Push c)              = "push " ^ (vsm_constant_to_string c) 
  | vsm_b_operation_to_string (VSM_B_Load dl)             = "load " ^ (vsm_data_loc_to_string dl)
  | vsm_b_operation_to_string (VSM_B_Store dl)            = "store " ^ (vsm_data_loc_to_string dl)

fun vsm_bytecode_program_to_string prog = 
    let fun aux _ [] = "\n" 
          | aux m (c::rest) = "l" ^ (Int.toString m) ^ " : " ^ (vsm_b_operation_to_string c) ^ "\n" ^ (aux (m + 1) rest)
    in aux 0 prog end 





