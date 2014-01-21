
type offset = int 
type vsm_label = string
type constant = int 

datatype vsm_operation = 
         VSM_Comment of string 
       | VSM_Label of vsm_label
       (* data operations *) 
       | VSM_Nop 
       | VSM_Push of constant 
       | VSM_Load of offset
       | VSM_Store of offset
       | VSM_Pop 
       | VSM_Add 
       | VSM_Sub 
       | VSM_Mul 
       (* control flow operations *) 
       | VSM_Hlt 
       | VSM_Jmp of vsm_label
       | VSM_Ifz of vsm_label
       | VSM_Ifp of vsm_label
       | VSM_Ifn of vsm_label
       (* input/output *) 
       | VSM_Pri 
       | VSM_Prb 

type vsm_program = vsm_operation list 

fun vsm_operation_to_string (VSM_Comment s) = "%" ^ s
  | vsm_operation_to_string (VSM_Label l)   = l ^ " : "
  | vsm_operation_to_string VSM_Hlt         = "hlt"
  | vsm_operation_to_string VSM_Nop         = "nop"
  | vsm_operation_to_string VSM_Add         = "add"
  | vsm_operation_to_string VSM_Sub         = "sub"
  | vsm_operation_to_string VSM_Mul         = "mul"
  | vsm_operation_to_string VSM_Pop         = "pop"
  | vsm_operation_to_string VSM_Pri         = "pri"
  | vsm_operation_to_string VSM_Prb         = "prb"
  | vsm_operation_to_string (VSM_Jmp cl)    = "jmp " ^ cl 
  | vsm_operation_to_string (VSM_Ifz cl)    = "ifz " ^ cl 
  | vsm_operation_to_string (VSM_Ifp cl)    = "ifp " ^ cl 
  | vsm_operation_to_string (VSM_Ifn cl)    = "ifn " ^ cl 
  | vsm_operation_to_string (VSM_Load dl)   = "load " ^ (Int.toString dl) 
  | vsm_operation_to_string (VSM_Store dl)  = "store "^ (Int.toString dl) 
  | vsm_operation_to_string (VSM_Push c)    = "push " ^ (Int.toString c) 

fun vsm_program_to_string prog = 
    let fun aux comment [] = "\n" 
          | aux "" ((VSM_Comment s) :: rest) = 
              aux s rest
          | aux comment ((VSM_Comment s) :: rest) = 
              aux (comment ^ ". " ^ s) rest
          | aux comment ((VSM_Label l) :: rest) = 
              l ^ " : " ^ (aux comment rest)
          | aux "" (oper :: rest)  = 
              "\t" ^ (vsm_operation_to_string oper) ^ "\n" 
              ^ (aux "" rest)
          | aux comment (oper :: rest) = 
              "\t" ^ (vsm_operation_to_string oper) 
              ^ "\t%" ^ comment ^ "\n"
              ^ (aux "" rest)
     in aux "" prog end 