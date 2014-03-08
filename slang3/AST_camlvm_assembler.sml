datatype caml_operation =
        ACC of int (* Peek the n+1th element on the stack and put into the accumulator. *)
      | PUSH (* Push the accumulator onto the stack. *)
      | POP of int (* Pop n elements from the stack. *)
      | ASSIGN of int (* Set the element of index n in the stack to the value
                         of the accumulator and set the accumulator to unit. *)
      | BRANCH of int (* Add ofs to the program counter *)
      | BRANCHIF of int (* Add ofs to the program counter if the accumulator is not zero. *)
      | BRANCHIFNOT of int (* Add ofs to the program counter if the accumulator is zero. *)
      | BOOLNOT (* Perform a boolean not on the accumulator *)
      | CONSTINT of int (* Set the accumulator to n *)
      | NEGINT (* acc := -acc *)
      | ADDINT (* acc := acc + value on top of stack (and pop that value) *)
      | SUBINT (* acc = acc - value on top of stack (and pop) *)
      | MULINT (* acc = acc * value... *)
      | DIVINT (* acc = acc / value ... *)
      | EQ (* set accumulator to non-zero or zero depending on whether the acc
              is equal to the value popped from the stack or not *)
      | NEQ (* opposite of eq *)
      | LTINT (* Sets accumulator to nz or z iff acc < popped top of stack or not *)
      | LEINT (* Sets acc to nz or z if acc <= *)
      | GTINT (* Set acc to nz or z if acc > *)
      | GEINT (* >= *)

type caml_program = caml_operation list

fun caml_operation_to_opcode (ACC _) = 8
  | caml_operation_to_opcode PUSH = 9
  | caml_operation_to_opcode (POP _) = 19
  | caml_operation_to_opcode (ASSIGN _) = 20
  | caml_operation_to_opcode (BRANCH _) = 84
  | caml_operation_to_opcode (BRANCHIF _) = 85
  | caml_operation_to_opcode (BRANCHIFNOT _) = 86
  | caml_operation_to_opcode BOOLNOT = 88
  | caml_operation_to_opcode (CONSTINT _) = 103
  | caml_operation_to_opcode NEGINT = 109
  | caml_operation_to_opcode ADDINT = 110
  | caml_operation_to_opcode SUBINT = 111
  | caml_operation_to_opcode MULINT = 112
  | caml_operation_to_opcode DIVINT = 113
  | caml_operation_to_opcode EQ = 121
  | caml_operation_to_opcode NEQ = 122 
  | caml_operation_to_opcode LTINT = 123
  | caml_operation_to_opcode LEINT = 124
  | caml_operation_to_opcode GTINT = 125
  | caml_operation_to_opcode GEINT = 126

fun caml_operation_to_string (ACC n) = "acc " ^ (Int.toString n)
  | caml_operation_to_string PUSH = "push"
  | caml_operation_to_string (POP n) = "pop " ^ (Int.toString n)
  | caml_operation_to_string (ASSIGN n) = "assign " ^ (Int.toString n)
  | caml_operation_to_string (BRANCH n) = "branch " ^ (Int.toString n)
  | caml_operation_to_string (BRANCHIF n) = "branchif " ^ (Int.toString n)
  | caml_operation_to_string (BRANCHIFNOT n) = "branchifnot " ^ (Int.toString n)
  | caml_operation_to_string BOOLNOT = "boolnot"
  | caml_operation_to_string (CONSTINT n) = "constint " ^ (Int.toString n)
  | caml_operation_to_string NEGINT = "negint"
  | caml_operation_to_string ADDINT = "addint"
  | caml_operation_to_string SUBINT = "subint"
  | caml_operation_to_string MULINT = "mulint"
  | caml_operation_to_string DIVINT = "divint"
  | caml_operation_to_string EQ = "eq"
  | caml_operation_to_string NEQ = "neq"
  | caml_operation_to_string LTINT = "ltint"
  | caml_operation_to_string LEINT = "leint"
  | caml_operation_to_string GTINT = "gtint"
  | caml_operation_to_string GEINT = "geint"

fun caml_program_to_string [] = "\n"
  | caml_program_to_string (oper :: rest) = (caml_operation_to_string oper) ^ "\n"
