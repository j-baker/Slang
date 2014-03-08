# Compiler Construction 2014
# Computer Laboratory 
# University of Cambridge 
# Timothy G. Griffin (tgg22@cam.ac.uk) 

# Unix Makefile for Slang.3 implementation

# adjust next line for your system
MOSMLHOME=/Users/timothygriffin/installs/mosml
MOSMLTOOLS=$(MOSMLHOME)/bin/camlrunm $(MOSMLHOME)/tools
MOSMLC=$(MOSMLHOME)/bin/mosmlc 
MOSMLYAC=$(MOSMLHOME)/bin/mosmlyac
MOSMLLEX=$(MOSMLHOME)/bin/mosmllex

FILES= Library.uo \
	   AST_L3.uo Parser.uo Lexer.uo Global.uo \
	   Typing.uo \
	   Alpha.uo \
	   AST_IR1.uo L3_to_IR1.uo \
	   AST_IR2.uo IR1_to_IR2.uo \
	   AST_vsm_assembler.uo IR2_to_VSM.uo \
	   AST_vsm_bytecode.uo vsm_assemble.uo vsm_emit.uo \
	   compile.uo

all: slang3

slang3: $(FILES) main.sml
	$(MOSMLC) -o $@ $(FILES) main.sml 

.SUFFIXES :
.SUFFIXES : .sml .sig .ui .uo .grm .lex

.sig.ui:
	$(MOSMLC) -c $<

.sml.uo:
	$(MOSMLC) -c $<

%.sig %.sml: %.grm
	$(MOSMLYAC) -v $<

%.sml: %.lex
	$(MOSMLLEX) $<

.depend: 
	$(MOSMLTOOLS)/mosmldep > $@

clean:
	rm -f *.ui *.uo .depend 
	rm -f Parser.sml Parser.sig Lexer.sml Parser.output

purge: clean
	rm -f slang3

include .depend
Parser.uo: Parser.ui
