open Global; 
open compile; 

fun print_error s1 s2 = print ("Error : " ^ s1 ^ " : " ^ s2 ^ "\n"); 

fun report_exception (Library.Missing s) = 
     print_error "missing" s 
  | report_exception (vrm_emit.VRM_TooManyInstructions s) = 
     print_error "too many instructions for the tiny VRM: " s 
  | report_exception (vsm_emit.VSM_TooManyInstructions s) = 
     print_error "too many instructions for the tiny VSM:" s 
  | report_exception (Lexer.LexerError s) = 
     print_error "lexer" s 
  | report_exception (parser.ParseError s) = 
     print_error "parser" s 
  | report_exception (Typing.TypeError s) = 
     print_error "type checking" s 
  | report_exception (Library.InternalError s) = 
     print_error "internal" s 
  | report_exception e = raise e 
  ; 

val fin = ref ""; 
val fout = ref ""; 

val usage = "slang1 [-v] [-vrm | -vsm ] infile.slang [outfile]\n" 
          ^ "        -v  = verbose \n" 
          ^ "        -vrm  = compile to VRM.0 (default) \n" 
          ^ "        -vsm  = compile to VSM.0  \n" 
          ^ "if outfile is not provided than infile.sang \n" 
          ^ "will be used to create outfile = infile.vrmo or infile.vsmo\n"
          ; 

fun scan_args [] = () 
  | scan_args ("-v"::rest) = (verbose := true; scan_args rest) 
  | scan_args ("-vrm"::rest) = scan_args rest
  | scan_args ("-vsm"::rest) = (target := VSM; scan_args rest) 
  | scan_args (f::rest) = 
     if !fin = "" 
     then (fin := f; scan_args rest) 
     else (fout := f; scan_args rest) 
  ;

fun infile_to_outfile f = 
    if String.substring(f, (size f)-5, 5) = "slang" 
    then if (!target) = VRM 
         then (fout := (String.substring(f, 0, (size f)-5)) ^ "vrmo"; true) 
         else (fout := (String.substring(f, 0, (size f)-5)) ^ "vsmo"; true) 
    else false 

fun process_args args = 
    let val _ = scan_args args 
    in 
        if !fin = "" 
        then (print usage; false) 
        else if !fout = ""
             then infile_to_outfile (!fin)
             else true 
    end ; 
  
fun main () = 
    if process_args (CommandLine.arguments())
    then compile (!fin) (!fout) 
    else print usage ; 

val _ = (main ()) handle e => (report_exception e);
