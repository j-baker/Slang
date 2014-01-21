open AST_L1; 
open AST_IR1; 

fun read_locations_of e = 
    let fun add_loc l locs = 
            if List.exists (fn z => l = z) locs then locs else l :: locs 
        fun aux locs Skip = locs 
          | aux locs (Integer _) = locs 
          | aux locs (Boolean _) = locs 
          | aux locs (UnaryOp(_, e)) = aux locs e  
          | aux locs (Op(e1, _, e2)) = aux (aux locs e1) e2  
          | aux locs (Assign(_, e)) = aux locs e
          | aux locs (Deref l) = add_loc l locs 
          | aux locs (If(e1, e2, e3)) = aux (aux (aux locs e1) e2) e3
          | aux locs (Seq (e1, e2)) = aux (aux locs e1) e2  
          | aux locs (While (e1, e2)) = aux (aux locs e1) e2  
          | aux locs (Print e) = aux locs e 
    in aux [] e end 

fun write_locations_of e = 
    let fun add_loc l locs = 
            if List.exists (fn z => l = z) locs then locs else l :: locs 
        fun aux locs Skip = locs 
          | aux locs (Integer _) = locs 
          | aux locs (Boolean _) = locs 
          | aux locs (UnaryOp(_, e)) = aux locs e  
          | aux locs (Op(e1, _, e2)) = aux (aux locs e1) e2  
          | aux locs (Assign(l, e)) = add_loc l (aux locs e)  
          | aux locs (Deref _) = locs
          | aux locs (If(e1, e2, e3)) = aux (aux (aux locs e1) e2) e3
          | aux locs (Seq (e1, e2)) = aux (aux locs e1) e2  
          | aux locs (While (e1, e2)) = aux (aux locs e1) e2  
          | aux locs (Print e) = aux locs e 
    in aux [] e end 
                   

fun l1_to_ir2 Skip        = ([], IR1_ESkip)  
  | l1_to_ir2 (Integer n) = ([], IR1_Integer n)
  | l1_to_ir2 (Boolean b) = ([], IR1_Boolean b)
  | l1_to_ir2 (UnaryOp (uop, e)) = 
    let val (sl, e') = l1_to_ir2 e 
    in 
	(sl, IR1_UnaryOp(uop, e')) 
    end 
  | l1_to_ir2 (Op (e1, bop, e2)) = 
    let val (sl1, e1') = l1_to_ir2 e1 
        and (sl2, e2') = l1_to_ir2 e2 
    in 
       (* be careful! running e2 could change the value of e1 *) 
       if Library.intersects(read_locations_of e1, write_locations_of e2)  
       then let val l = Global.new_loc () 
            in 
               (sl1 @ [IR1_Assign(l, e1')] @ sl2, IR1_Op(IR1_Deref l, bop, e2')) 
            end 
       else (sl1 @ sl2, IR1_Op(e1', bop, e2')) 
    end 
  | l1_to_ir2 (Assign (l, e)) = 
    let val (sl, e') = l1_to_ir2 e 
    in 
       (sl @ [IR1_Assign(l, e')], IR1_ESkip) 
    end 
  | l1_to_ir2 (Deref l) = ([], IR1_Deref l)
  | l1_to_ir2 (Seq (e1, e2)) = 
    let val (sl1, _  ) = l1_to_ir2 e1 
        and (sl2, e2') = l1_to_ir2 e2 
    in 
	(sl1 @ sl2, e2') 
    end 
  | l1_to_ir2 (If(e1, e2, e3)) = 
    let val (sl1, e1') = l1_to_ir2 e1 
        and (sl2, e2') = l1_to_ir2 e2 
        and (sl3, e3') = l1_to_ir2 e3
    in 
        (* would be better to avoid duplication of e1'? *) 
	(sl1 @ [IR1_SIf(e1', IR1_Seq sl2, IR1_Seq sl3)], IR1_EIf(e1', e2', e3')) 	
    end 
  | l1_to_ir2 (While (e1, e2)) = 
    let val (sl1, e1') = l1_to_ir2 e1 
        and (sl2, _  ) = l1_to_ir2 e2 
    in 
        (sl1 @ [IR1_While(e1', IR1_Seq (sl2 @ sl1))], IR1_ESkip) 
    end 
  | l1_to_ir2 (Print e) = 
    let val (sl, e') = l1_to_ir2 e 
    in 
        (sl @ [IR1_Print e'], IR1_ESkip)
    end 

fun translate_l1_to_ir1 e = 
    let val (sl, e') = l1_to_ir2 e 
        val result = IR1_Seq (sl @ [IR1_Expr e']) 
        val _ = if !Global.verbose
                then (print "\n\n IR1 AST = \n" ; 
                      pp_ir1_stm result)
                else () 
    in 
        result 
    end 
    


