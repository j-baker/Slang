#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <inttypes.h>

/* 
 Compiler Construction 2013 
 Computer Laboratory 
 University of Cambridge 
 Timothy G. Griffin (tgg22@cam.ac.uk) 
*/ 

/** opcodes 
 ** The machine has a store and the contents at address m are store[m]. 
 ** The machine has a "stack" and most operations work on the top of the stack. 
 ** the notation "foo ==> bar" means that before the operation "foo" was on 
 ** top of the stack, after the operation "foo" is replaced by "bar". Both "foo" 
 ** and "bar" are read left-to-right.  This notation implicitly describes changes 
 ** in the stack pointer.  Here are a few example: 
 **   a, b => a +b    : replace the top-of-stack and the value under it with their sum. 
 **   a    =>         : pop the top of the stack
 **        => v       : push value v on the stack. 
 ** Below c represents an integer constant. 
*/ 
/* data opcodes */ 
#define OP_NOP     1 /* nop          : pc <- !pc +1                                   */
#define OP_PUSH    2 /* push c       : => c ; pc <- !pc +1                            */
#define OP_LOAD    3 /* load m       : => store[m] ; pc <- !pc +1                     */
#define OP_STORE   4 /* store m      : a => ; store[m] <- a ; pc <- !pc +1            */
#define OP_POP     5 /* pop          : a => ; pc <- !pc +1                            */
#define OP_ADD     6 /* add          : a, b => a + b; pc <- !pc +1                    */
#define OP_SUB     7 /* sub          : a, b => b - a; pc <- !pc +1                    */
#define OP_MUL     8 /* mul          : a, b => a * b; pc <- !pc +1                    */
/* control flow opcodes */ 
#define OP_HLT     9 /* hlt          : HALT the machine                               */
#define OP_JMP    10 /* jmp l        : pc <- l                                        */
#define OP_IFZ    11 /* ifz l        : a => ; if a == 0 then pc <- l else pc <- !pc+1 */
#define OP_IFP    12 /* ifp l        : a => ; if a => 0 then pc <- l else pc <- !pc+1 */
#define OP_IFN    13 /* ifn l        : a => ; if a <  0 then pc <- l else pc <- !pc+1 */
/* input/output opcodes */ 
#define OP_PRI    14 /* pri          : a => ; print out a as an integer; pc <- !pc+1  */ 
#define OP_PRB    15 /* prb          : a => ; print out a as an boolean; pc <- !pc+1  */ 

/* vsm.0 is a very tiny virtual stack machine */ 
#define INSTRUCTION_LIMIT   256
#define STACK_LIMIT         256

typedef uint8_t opcode;     
typedef uint8_t argument; 
typedef uint8_t code_range;  /* values program counter can take on     */ 
typedef uint8_t stack_range; /* values stack pointer can take on       */ 
typedef int32_t value;       /* what is held in stack or store         */ 
typedef uint8_t flag;        /* boolean flag                           */ 

typedef struct
{
  opcode     code; 
  argument   arg1; 
} bytecode; 

typedef struct
{
  flag        is_running; 
  code_range  instruction_count;   /* number of instructions*/ 
  code_range  pc;                  /* Program counter       */
  stack_range sp;                  /* stack pointer       */
  bytecode*   code;                /* Array of instructions */
  value*      stack;               /* the stack */ 
} vsm_state;


uint8_t vsm_version; 
vsm_state* vsm_create(void); 
void vsm_destruct(vsm_state *state); 
void vsm_abort(vsm_state *state); 
code_range vsm_read_binary_instructions(FILE* fd, bytecode** code, uint8_t* version);
void vsm_load(vsm_state *state, bytecode *instrs, code_range count); 
void vsm_execute_instruction(vsm_state *state, bytecode instr); 
void vsm_execute(vsm_state *state, flag verbose); 
void vsm_print_instruction(bytecode inst); 
void vsm_print_instructions(bytecode *instrs, code_range count); 



