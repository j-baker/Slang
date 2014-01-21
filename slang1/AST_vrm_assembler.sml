
type vrm_data_loc = string 
type vrm_label = string
type vrm_constant = int

datatype vrm_operation = 
         VRM_Comment of string 
       | VRM_Label of vrm_label
       (* data operations *) 
       | VRM_Nop 
       | VRM_Set of vrm_data_loc * vrm_constant  
       | VRM_Mov of vrm_data_loc * vrm_data_loc 
       | VRM_Add of vrm_data_loc * vrm_data_loc * vrm_data_loc 
       | VRM_Sub of vrm_data_loc * vrm_data_loc * vrm_data_loc 
       | VRM_Mul of vrm_data_loc * vrm_data_loc * vrm_data_loc 
       (* control flow operations *) 
       | VRM_Hlt 
       | VRM_Jmp of vrm_label  
       | VRM_Ifz of vrm_data_loc * vrm_label  
       | VRM_Ifp of vrm_data_loc * vrm_label  
       | VRM_Ifn of vrm_data_loc * vrm_label  
       (* input/output *) 
       | VRM_Pri of vrm_data_loc 
       | VRM_Prb of vrm_data_loc 

type vrm_program = vrm_operation list 

fun vrm_operation_to_string (VRM_Comment s)           = "%" ^ s 
  | vrm_operation_to_string (VRM_Label l)             = l ^ " : "
  | vrm_operation_to_string VRM_Hlt                   = "hlt" 
  | vrm_operation_to_string VRM_Nop                   = "nop" 
  | vrm_operation_to_string (VRM_Jmp cl)              = "jmp " ^ cl 
  | vrm_operation_to_string (VRM_Set (dl, c))         = "set " ^ dl  ^ " " ^ (Int.toString c)
  | vrm_operation_to_string (VRM_Mov (dl1, dl2))      = "mov " ^ dl1 ^ " " ^ dl2 
  | vrm_operation_to_string (VRM_Add (dl1, dl2, dl3)) = "add " ^ dl1 ^ " " ^ dl2 ^ " " ^ dl3 
  | vrm_operation_to_string (VRM_Sub (dl1, dl2, dl3)) = "sub " ^ dl1 ^ " " ^ dl2 ^ " " ^ dl3 
  | vrm_operation_to_string (VRM_Mul (dl1, dl2, dl3)) = "mul " ^ dl1 ^ " " ^ dl2 ^ " " ^ dl3 
  | vrm_operation_to_string (VRM_Ifz (dl, cl))        = "ifz " ^ dl  ^ " " ^ cl 
  | vrm_operation_to_string (VRM_Ifp (dl, cl))        = "ifp " ^ dl  ^ " " ^ cl 
  | vrm_operation_to_string (VRM_Ifn (dl, cl))        = "ifn " ^ dl  ^ " " ^ cl 
  | vrm_operation_to_string (VRM_Pri dl)              = "pri " ^ dl 
  | vrm_operation_to_string (VRM_Prb dl)              = "prb " ^ dl 

fun vrm_program_to_string prog = 
    let fun aux comment [] = "\n" 
          | aux "" ((VRM_Comment s) :: rest) = 
              aux s rest
          | aux comment ((VRM_Comment s) :: rest) = 
              aux (comment ^ ". " ^ s) rest
          | aux comment ((VRM_Label l) :: rest) = 
              l ^ " : " ^ (aux comment rest)
          | aux "" (oper :: rest)  = 
              "\t" ^ (vrm_operation_to_string oper) ^ "\n" 
              ^ (aux "" rest)
          | aux comment (oper :: rest) = 
              "\t" ^ (vrm_operation_to_string oper) 
              ^ "\t%" ^ comment ^ "\n"
              ^ (aux "" rest)
     in aux "" prog end 