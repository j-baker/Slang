# Compiler Construction 2013 
# Computer Laboratory 
# University of Cambridge 
# Timothy G. Griffin (tgg22@cam.ac.uk) 

# Unix Makefile for Slang.1 implementation

# adjust this for your system
MOSMLHOME=/usr/local/
MOSMLTOOLS=$(MOSMLHOME)/bin/camlrunm $(MOSMLHOME)/tools
MOSMLC=$(MOSMLHOME)/bin/mosmlc 

FILES=Library.uo Global.uo AST_L1.uo AST_IR1.uo AST_IR2.uo L1_to_IR1.uo IR1_to_IR2.uo AST_vrm_assembler.uo AST_vrm_bytecode.uo AST_vsm_assembler.uo AST_vsm_bytecode.uo Lexer.uo parser.uo Typing.uo vrm_generate_code.uo vrm_assemble.uo vrm_emit.uo vsm_generate_code.uo vsm_assemble.uo vsm_emit.uo compile.uo

all: $(FILES)
	$(MOSMLC) -o slang1 $(FILES) main.sml 

.SUFFIXES :
.SUFFIXES : .sml .sig .ui .uo 

.sig.ui:
	$(MOSMLC) -c $<

.sml.uo:
	$(MOSMLC) -c $<

depend: 
	rm -f Makefile.bak
	mv Makefile Makefile.bak
	$(MOSMLTOOLS)/cutdeps < Makefile.bak > Makefile
	$(MOSMLTOOLS)/mosmldep >> Makefile

clean:
	rm -f *.ui *.uo
	rm -f Makefile.bak
	rm -f slang1

### DO NOT DELETE THIS LINE
vsm_generate_code.uo: Library.ui AST_vsm_assembler.uo AST_IR1.uo AST_IR2.uo \
    AST_L1.uo Global.uo 
vsm_emit.uo: AST_vsm_bytecode.uo 
vsm_assemble.uo: Library.ui AST_vsm_assembler.uo Global.uo \
    AST_vsm_bytecode.uo 
vrm_generate_code.uo: AST_IR1.uo AST_IR2.uo AST_vrm_assembler.uo AST_L1.uo \
    Global.uo 
vrm_emit.uo: AST_vrm_bytecode.uo 
vrm_assemble.uo: Library.ui AST_vrm_bytecode.uo AST_vrm_assembler.uo \
    Global.uo 
Typing.uo: AST_L1.uo Global.uo 
parser.uo: Lexer.ui AST_L1.uo Global.uo 
main.uo: Library.ui parser.uo vrm_emit.uo Lexer.ui Global.uo vsm_emit.uo \
    compile.uo 
Library.uo: Library.ui 
Lexer.uo: Lexer.ui 
L1_to_IR1.uo: Library.ui AST_IR1.uo AST_L1.uo Global.uo 
IR1_to_IR2.uo: AST_IR1.uo AST_IR2.uo Global.uo 
Global.uo: Library.ui 
compile.uo: IR1_to_IR2.uo parser.uo vrm_generate_code.uo vrm_assemble.uo \
    Lexer.ui vrm_emit.uo Global.uo L1_to_IR1.uo vsm_emit.uo vsm_assemble.uo \
    vsm_generate_code.uo Typing.uo 
AST_vsm_assembler.uo: Library.ui 
AST_L1.uo: Library.ui 
AST_IR2.uo: Library.ui AST_IR1.uo AST_L1.uo 
AST_IR1.uo: Library.ui AST_L1.uo 
