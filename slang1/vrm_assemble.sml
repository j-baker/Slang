open AST_vrm_assembler; 
open AST_vrm_bytecode; 

val new_data_loc = Library.new_counter()

fun update_data_map dmap dl = 
     (let val _ = Library.lookup(dmap, dl) in dmap end) 
       handle (Library.Missing _) => Library.update(dmap, dl, new_data_loc()) 

fun clean_prog_and_make_maps prog = 
    let fun aux lmap dmap m rprog []                        = (m, lmap, dmap, List.rev rprog)
          | aux lmap dmap m rprog ((VRM_Comment _) :: rest) = aux lmap dmap m rprog rest
          | aux lmap dmap m rprog ((VRM_Label l) :: rest)   = aux (Library.update(lmap, l, m)) dmap m rprog rest
          | aux lmap dmap m rprog (opr :: rest)              = 
            let val new_dmap = (case opr of 
                                  VRM_Set (l, _)    => update_data_map dmap l 
                                | VRM_Mov (l, _)    => update_data_map dmap l 
                                | VRM_Add (l, _, _) => update_data_map dmap l 
                                | VRM_Sub (l, _, _) => update_data_map dmap l 
                                | VRM_Mul (l, _, _) => update_data_map dmap l
                                | _                 => dmap) 
            in aux lmap new_dmap (m + 1) (opr :: rprog) rest end 
    in aux Library.empty_env Library.empty_env 0 [] prog end 

val convert_int = Word8.fromInt

fun find (map, key) = 
     (convert_int (Library.lookup (map, key)))
       handle (Library.Missing _) => Library.internal_error ("unmatched label or variable in vrm_assemble.sml : " ^ key)

(* OK, this is something of a hack for uninitialised variables *) 
fun find_or_init (map, key) = 
     (convert_int (Library.lookup (map, key)))
       handle (Library.Missing _) => find(map, vrm_generate_code.zero_loc)


fun vrm_assemble_op lmap dmap VRM_Hlt                   = VRM_B_Hlt 
  | vrm_assemble_op lmap dmap VRM_Nop                   = VRM_B_Nop
  | vrm_assemble_op lmap dmap (VRM_Jmp l)               = VRM_B_Jmp(find(lmap, l))
  | vrm_assemble_op lmap dmap (VRM_Set (d, c))          = VRM_B_Set(find(dmap, d), convert_int c)
  | vrm_assemble_op lmap dmap (VRM_Mov (dl1, dl2))      = VRM_B_Mov(find(dmap, dl1), find_or_init(dmap, dl2))
  | vrm_assemble_op lmap dmap (VRM_Add (dl1, dl2, dl3)) = VRM_B_Add(find(dmap, dl1), find_or_init(dmap, dl2), find_or_init(dmap, dl3))
  | vrm_assemble_op lmap dmap (VRM_Sub (dl1, dl2, dl3)) = VRM_B_Sub(find(dmap, dl1), find_or_init(dmap, dl2), find_or_init(dmap, dl3))
  | vrm_assemble_op lmap dmap (VRM_Mul (dl1, dl2, dl3)) = VRM_B_Mul(find(dmap, dl1), find_or_init(dmap, dl2), find_or_init(dmap, dl3))
  | vrm_assemble_op lmap dmap (VRM_Ifz (dl, cl))        = VRM_B_Ifz(find(dmap, dl), find(lmap, cl))
  | vrm_assemble_op lmap dmap (VRM_Ifp (dl, cl))        = VRM_B_Ifp(find(dmap, dl), find(lmap, cl))
  | vrm_assemble_op lmap dmap (VRM_Ifn (dl, cl))        = VRM_B_Ifn(find(dmap, dl), find(lmap, cl))
  | vrm_assemble_op lmap dmap (VRM_Pri dl)              = VRM_B_Pri(find_or_init(dmap, dl))
  | vrm_assemble_op lmap dmap (VRM_Prb dl)              = VRM_B_Prb(find_or_init(dmap, dl))
  | vrm_assemble_op lmap dmap (VRM_Comment _)           = Library.internal_error "VRM_Comment cannot exist in cleaned code"
  | vrm_assemble_op lmap dmap (VRM_Label _)             = Library.internal_error "VRM_Label cannot exist in cleaned code"
  
fun vrm_assemble prog = 
    let val (inst_count, lmap, dmap, clean_prog) = clean_prog_and_make_maps prog 
        val code = List.map (vrm_assemble_op lmap dmap) clean_prog 
        val _ = if !Global.verbose
                then (print "Intruction count = "; 
                      print (Int.toString inst_count); 
                      print "\ncode: \n";  
                      print ((vrm_bytecode_program_to_string code) ^ "\n"))
                else () 
    in (inst_count, code) end 
