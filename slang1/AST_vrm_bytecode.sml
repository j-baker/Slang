
type vrm_data_loc = Word8.word; 
type vrm_code_loc = Word8.word; 
type vrm_constant = Word8.word; 

datatype vrm_b_operation = 
       (* data operations *) 
         VRM_B_Nop 
       | VRM_B_Set of vrm_data_loc * vrm_constant 
       | VRM_B_Mov of vrm_data_loc * vrm_data_loc
       | VRM_B_Add of vrm_data_loc * vrm_data_loc * vrm_data_loc 
       | VRM_B_Sub of vrm_data_loc * vrm_data_loc * vrm_data_loc 
       | VRM_B_Mul of vrm_data_loc * vrm_data_loc * vrm_data_loc 
       (* control flow operations *) 
       | VRM_B_Hlt 
       | VRM_B_Jmp of vrm_code_loc 
       | VRM_B_Ifz of vrm_data_loc * vrm_code_loc 
       | VRM_B_Ifp of vrm_data_loc * vrm_code_loc 
       | VRM_B_Ifn of vrm_data_loc * vrm_code_loc 
       (* input/output *) 
       | VRM_B_Pri of vrm_data_loc
       | VRM_B_Prb of vrm_data_loc
       | VRM_B_Rdi of vrm_data_loc
       | VRM_B_Rdb of vrm_data_loc

type vrm_b_program = vrm_b_operation list 

fun vrm_data_loc_to_string b = "r" ^ (Int.toString (Word8.toInt b))
fun vrm_code_loc_to_string b = "l" ^ (Int.toString (Word8.toInt b))
fun vrm_constant_to_string b = Int.toString (Word8.toIntX b)  (* hmmm .... *) 

fun vrm_b_operation_to_string VRM_B_Hlt                   = "hlt" 
  | vrm_b_operation_to_string VRM_B_Nop                   = "nop" 
  | vrm_b_operation_to_string (VRM_B_Jmp cl)              = "jmp " ^ (vrm_code_loc_to_string cl) 
  | vrm_b_operation_to_string (VRM_B_Set (dl, c))         = "set " ^ (vrm_data_loc_to_string dl)  ^ " " ^ (vrm_constant_to_string c) 
  | vrm_b_operation_to_string (VRM_B_Mov (dl1, dl2))      = "mov " ^ (vrm_data_loc_to_string dl1) ^ " " ^ (vrm_data_loc_to_string dl2)
  | vrm_b_operation_to_string (VRM_B_Add (dl1, dl2, dl3)) = "add " ^ (vrm_data_loc_to_string dl1) ^ " " ^ (vrm_data_loc_to_string dl2) ^ " " ^ (vrm_data_loc_to_string dl3)
  | vrm_b_operation_to_string (VRM_B_Sub (dl1, dl2, dl3)) = "sub " ^ (vrm_data_loc_to_string dl1) ^ " " ^ (vrm_data_loc_to_string dl2) ^ " " ^ (vrm_data_loc_to_string dl3)
  | vrm_b_operation_to_string (VRM_B_Mul (dl1, dl2, dl3)) = "mul " ^ (vrm_data_loc_to_string dl1) ^ " " ^ (vrm_data_loc_to_string dl2) ^ " " ^ (vrm_data_loc_to_string dl3)
  | vrm_b_operation_to_string (VRM_B_Ifz (dl, cl))        = "ifz " ^ (vrm_data_loc_to_string dl)  ^ " " ^ (vrm_code_loc_to_string cl)
  | vrm_b_operation_to_string (VRM_B_Ifp (dl, cl))        = "ifp " ^ (vrm_data_loc_to_string dl)  ^ " " ^ (vrm_code_loc_to_string cl)
  | vrm_b_operation_to_string (VRM_B_Ifn (dl, cl))        = "ifn " ^ (vrm_data_loc_to_string dl)  ^ " " ^ (vrm_code_loc_to_string cl)
  | vrm_b_operation_to_string (VRM_B_Pri dl)              = "pri " ^ (vrm_data_loc_to_string dl)
  | vrm_b_operation_to_string (VRM_B_Prb dl)              = "prb " ^ (vrm_data_loc_to_string dl)
  | vrm_b_operation_to_string (VRM_B_Rdi dl)              = "rdi " ^ (vrm_data_loc_to_string dl)
  | vrm_b_operation_to_string (VRM_B_Rdb dl)              = "rdb " ^ (vrm_data_loc_to_string dl)

fun vrm_bytecode_program_to_string prog = 
    let fun aux _ [] = "\n" 
          | aux m (c::rest) = "l" ^ (Int.toString m) ^ " : " ^ (vrm_b_operation_to_string c) ^ "\n" ^ (aux (m + 1) rest)
    in aux 0 prog end 


