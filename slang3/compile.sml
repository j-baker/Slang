

fun reportParseError lexbuf =
    let val pos1 = Lexing.getLexemeStart lexbuf
        val pos2 = Lexing.getLexemeEnd lexbuf
    in
      (BasicIO.output (BasicIO.std_err,
        "Parsing error at characters " ^ Int.toString pos1 ^ "-" ^ Int.toString pos2 ^ "\n\n") ;
      BasicIO.exit 1
      )
    end

fun createLexerStream is =
	Lexing.createLexer (fn buff => fn n => Nonstdio.buff_input is buff 0 n)

fun back_end fout sl = 
    case !Global.target of 
      Global.VSM => vsm_emit.emit_vsm_bytecode fout 
                     (vsm_assemble.vsm_assemble 
                       (vsm_assemble.vsm_peep_hole 
                         (IR2_to_VSM.ir2_to_vsm sl)))
    | Global.VRM => (print "\n\n No VRM backend yet\n")

fun compile fin fout = 
    let val lexbuf = (createLexerStream (Nonstdio.open_in_bin fin))
    in 
       (back_end fout 
          (IR1_to_IR2.translate
                (L3_to_IR1.translate
                   (Alpha.convert 
                      (Typing.check_types 
                         (Parser.main Lexer.token lexbuf))))))
         handle Parsing.ParseError f => reportParseError lexbuf
    end 

