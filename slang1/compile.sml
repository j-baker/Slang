open Global; 
open Lexer;
open parser;
open Typing; 
open L1_to_IR1; 
open IR1_to_IR2; 
open vrm_generate_code; 
open vsm_generate_code; 
open vrm_assemble; 
open vsm_assemble; 
open vrm_emit; 
open vsm_emit; 

fun back_end fout ast = 
    case !target of 
      VSM => emit_vsm_bytecode fout 
               (vsm_assemble 
                  (vsm_code_gen ast)) 
    | VRM => emit_vrm_bytecode fout 
               (vrm_assemble 
                  (vrm_code_gen ast)) 

fun compile fin fout = 
    back_end fout 
      (translate_ir1_to_ir2
        (translate_l1_to_ir1 
          (check_types 
            (parse (init_lex_buffer fin)))))





