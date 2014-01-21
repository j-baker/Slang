Compiler Construction 2014
Computer Laboratory 
University of Cambridge 
Timothy G. Griffin

Slang.1 Compiler (Slang = Simple LANGuage).  

This (very minimal) compiler, written in Mosml, targets one of two virtual machines: 
   VRM.0 : Virtual Register Machine, version 0. 
   VSM.0 : Virtual Stack Machine, version 0. 
                                     
Files : 

Library.sml	      : A few auxiliary functions 
Global.sml	      : A few global declarations 

AST_L1.sml	      : AST of L1 language from Semantics of Programming Languages. 
Lexer.sml	      : hand-written lexer 	
parser.sml	      : a hand-written recursive-descent parser 
                      : (see comments in that file for a slang.1 grammar description)
Typing.sml            : type checking 

AST_IR1.sml           : Intermediate Representation 1 : split expressions (free of 
                        side effects) from statements (primarily side-effecting). 
L1_to_IR1.sml         : translated from L1 to IR1. 
AST_IR2.sml           : Intermediate Representation 2 : eliminate structured statements 
                        (while, if-then-else) with labels and jumps. 
IR1_to_IR2.sml        : translate from IR1 to IR2. 

AST_vrm_assembler.sml : AST for VRM.0 assembly code  
AST_vrm_bytecode.sml  : AST for VRM.0 byte code  	
vrm_generate_code.sml : translate IR2 to vrm_assembler
vrm_assemble.sml      : translate vrm_assembler to vrm_bytecode
                        (main tasks are mapping symbolic labels to code locations) 
vrm_emit.sml          : output binary file (for VRM.0)

AST_vsm_assembler.sml : AST for VSM.0 assembly code  	
AST_vsm_bytecode.sm   : AST for VSM.0 byte code  	
vsm_generate_code.sml : translate IR2 to vsm_assembler
vsm_assemble.sml      : translated vsm_assembler to vsm_bytecode
                        (main tasks are mapping symbolic labels to code locations) 
vsm_emit.sml          : output binary file (for VSM.0)

compile.sml           : functions to put it all together 		
main.sml	      : command-line processing 	

examples/             : directory containing a few slang examples 

Doing a "make" should produce slang1 (you need to alter 
the line of Makefile that defines the macro MOSMLHOME). 

Here are examples of using slang1:

slang1 -vrm examples/squares.slang    : compile squares.slang to VRM.0 to binary object file examples/squares.vrmo
slang1 examples/squares.slang         : same as above (VRM.0 is the default) 
slang1 -v examples/squares.slang      : same as above, but with verbose output at each stage of compilation

slang1 -vsm examples/squares.slang    : compile squares.slang to VSM.0 to binary object file examples/squares.vsmo
slang1 -v -vsm examples/squares.slang : same as above, but with verbose output at each stage of compilation

Please report bugs to tgg22@cam.ac.uk.  All bug reports must contain a 
proposed fix! 
