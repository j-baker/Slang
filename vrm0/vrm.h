#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <inttypes.h>

/* 
 Compiler Construction 2014 
 Computer Laboratory 
 University of Cambridge 
 Timothy G. Griffin (tgg22@cam.ac.uk) 
*/ 

/** opcodes 
 ** The notation !r means the contents of register r, and 
 ** "r1 <- e" means evaluate e and put the results into register r. 
 ** Here c represents an integer constant. 
 ** The program counter (pc) can be thought of as a special register. 
 ** Execution of data instructions causes the pc to be incremented. 
 ** Execution of control flow instructions may change the pc in more 
 ** complicated ways. 
*/ 
/* data opcodes */ 
#define OP_NOP     1 /* nop          : Do Nothing but pc <- !pc +1              */
#define OP_SET     2 /* set r c      : r <- c          ; pc <- !pc +1           */
#define OP_MOV     3 /* mov r1 r2    : r1 <- !r2       ; pc <- !pc +1           */
#define OP_ADD     4 /* add r1 r2 r3 : r1 <- !r1 + !r2 ; pc <- !pc +1           */
#define OP_SUB     5 /* sub r1 r2 r3 : r1 <- !r1 - !r2 ; pc <- !pc +1           */
#define OP_MUL     6 /* mul r1 r2 r3 : r1 <- !r1 * !r2 ; pc <- !pc +1           */
/* control flow opcodes */ 
#define OP_HLT     7 /* hlt          : HALT the machine                         */
#define OP_JMP     8 /* jmp l        : pc <- l                                  */
#define OP_IFZ     9 /* ifz r l      : if !r == 0 then pc <- l else pc <- !pc+1 */
#define OP_IFP    10 /* ifp r l      : if !r >= 0 then pc <- l else pc <- !pc+1 */
#define OP_IFN    11 /* ifn r l      : if !r < 0 then pc <- l else pc <- !pc+1  */
/* input/output opcodes */ 
#define OP_PRI    12 /* pri r        : prints out !r as an integer; pc <- !pc+1 */ 
#define OP_PRB    13 /* prb r        : prints out !r as an boolean; pc <- !pc+1 */ 

/* vrm.0 is a very tiny virtual register machine */ 
#define NUMBER_OF_REGISTERS 256
#define INSTRUCTION_LIMIT   256

typedef uint8_t opcode;     
typedef uint8_t argument; 
typedef uint8_t code_range; /* values program counter can take on     */ 
typedef int32_t value;      /* what is held in a register (generous!) */ 
typedef uint8_t flag;       /* boolean flag                           */ 

typedef struct
{
  opcode     code; 
  argument   arg1; 
  argument   arg2; 
  argument   arg3; 
} bytecode; 


typedef struct
{
  flag       is_running; 
  code_range instruction_count;   /* number of instructions*/ 
  code_range pc;                  /* Program counter       */
  bytecode*  code;                /* Array of instructions */ 
  value*     registers;           /* Array of registers    */
  flag*      touched;             /* Array of flags for each register (for verbose mode) */
} vrm_state;


uint8_t vrm_version; 
vrm_state* vrm_create(void); 
void vrm_destruct(vrm_state *state); 
void vrm_abort(vrm_state *state); 
code_range vrm_read_binary_instructions(FILE* fd, bytecode** code, uint8_t* version);
void vrm_load(vrm_state *state, bytecode *instrs, code_range count); 
void vrm_execute_instruction(vrm_state *state, bytecode instr); 
void vrm_execute(vrm_state *state, flag verbose); 
void vrm_print_instruction(bytecode inst); 
void vrm_print_instructions(bytecode *instrs, code_range count); 



