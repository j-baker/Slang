
type offset = int 
type label = string 
type constant = int 

datatype vsm_operation = 
         VSM_Comment of string      (* Comment string                         *) 
       | VSM_Label of label         (* symbolic location (code address)       *) 
       | VSM_Nop                    (* do nothing                             *)
       | VSM_Push of constant       (* push a simple data value               *)
       | VSM_PushFun of label       (* push a function/procedure address      *)
       | VSM_Arg_Load of offset     (* push value at fp - offset              *) 
       | VSM_AllocateLocals of int  (* allocate stack space for local values  *) 
       | VSM_Load of offset         (* push local value at fp + offset        *)
       | VSM_Store of offset        (* pop top to fp + offset                 *) 
       | VSM_Pop                    (* pop top of stack                       *) 
       | VSM_Add                    (* replace top 2 values with sum          *) 
       | VSM_Sub                    (* replace top 2 values with difference   *) 
       | VSM_Mul                    (* replace top 2 values with product      *) 
       | VSM_Hlt                    (* stop the machine, I want to get off!   *) 
       | VSM_Jmp of label           (* jump to label                          *) 
       | VSM_Ifz of label           (* jump to label if top is zero, pop      *) 
       | VSM_Ifp of label           (* jump to label if top is positive, pop  *) 
       | VSM_Ifn of label           (* jump to label if top is negative, pop  *) 
       | VSM_Pri                    (* print stack-top, then pop it           *) 
       | VSM_Rdi                    (* read integer from stdin, push it       *) 
       | VSM_Assign                 (* assign via pointer to ref cell in heap *) 
       | VSM_Deref of constant      (* replace top with dereferenced value    *) 
       | VSM_Ref                    (* create ref cell on heap                *) 
       | VSM_Call of label          (* call known function                    *) 
       | VSM_Call_Closure of int    (* call closure                           *) 
       | VSM_Return of int          (* return and clear args off stack        *) 
       | VSM_Closure of label * int (* allocate closure on heap, push pointer *) 

type vsm_program = vsm_operation list 

fun vsm_operation_to_string (VSM_Comment s) = "%" ^ s
  | vsm_operation_to_string (VSM_Label l)   = l ^ ": "
  | vsm_operation_to_string VSM_Nop         = "nop"
  | vsm_operation_to_string (VSM_Push c)    = "push " ^ (Int.toString c) 
  | vsm_operation_to_string (VSM_PushFun c) = "pushfun " ^ c
  | vsm_operation_to_string (VSM_Load dl)   = "load " ^ (Int.toString dl) 
  | vsm_operation_to_string (VSM_AllocateLocals n) = "locals " ^ (Int.toString n) 
  | vsm_operation_to_string (VSM_Arg_Load fo) = "arg " ^ (Int.toString fo) 
  | vsm_operation_to_string (VSM_Store dl)  = "store "^ (Int.toString dl) 
  | vsm_operation_to_string VSM_Pop         = "pop"
  | vsm_operation_to_string VSM_Add         = "add"
  | vsm_operation_to_string VSM_Sub         = "sub"
  | vsm_operation_to_string VSM_Mul         = "mul"
  | vsm_operation_to_string VSM_Hlt         = "hlt"
  | vsm_operation_to_string (VSM_Jmp cl)    = "jmp " ^ cl 
  | vsm_operation_to_string (VSM_Ifz cl)    = "ifz " ^ cl 
  | vsm_operation_to_string (VSM_Ifp cl)    = "ifp " ^ cl 
  | vsm_operation_to_string (VSM_Ifn cl)    = "ifn " ^ cl 
  | vsm_operation_to_string VSM_Pri         = "pri"
  | vsm_operation_to_string VSM_Rdi         = "rdi"
  | vsm_operation_to_string VSM_Assign      = "assign"
  | vsm_operation_to_string (VSM_Deref c)   = "deref " ^ (Int.toString c) 
  | vsm_operation_to_string VSM_Ref         = "ref"
  | vsm_operation_to_string (VSM_Call cl)   = "call " ^ cl 
  | vsm_operation_to_string (VSM_Call_Closure n) = "callc " ^ (Int.toString n) 
  | vsm_operation_to_string (VSM_Return n)  = "return " ^ (Int.toString n) 
  | vsm_operation_to_string (VSM_Closure (f, k)) = 
       "closure " ^ f ^ " " ^ (Int.toString k) 

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
