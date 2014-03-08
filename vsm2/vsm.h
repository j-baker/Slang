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

/* vsm.2 is a very tiny virtual stack machine */ 
#define INSTRUCTION_LIMIT   256
#define STACK_LIMIT         256
#define HEAP_LIMIT          256

typedef uint8_t opcode;     
typedef uint8_t argument; 
typedef uint8_t code_range;  /* values program counter can take on     */ 
typedef uint8_t stack_range; /* values stack pointer can take on       */ 
typedef uint8_t heap_range;  /* values heap pointer can take on       */ 
typedef int32_t value;       /* what is held in stack or store         */ 
typedef uint8_t flag;        /* boolean flag                           */ 

typedef struct
{
  uint8_t    stack_type; 
  value      stack_data; 
} stack_value; 

/* stack types      */ 
/* We really need only a "one bit" tag on stack values to tell    */ 
/* us if a value is data or                                       */ 
/* if it is a pointer into the heap.  However, we tag values with */ 
/* additional information for the  verbose mode                   */

#define ST_CA   0 /* code address      */ 
#define ST_DT   1 /* data              */ 
#define ST_HP   2 /* pointer into heap */ 
#define ST_FP   3 /* frame pointer     */ 

/* heap type */ 
/* the heap will be implemented as an arraw of heap_values.          */ 
/* An object on the heap is prepresented by a index into this array. */ 
/* An object is made up of multiple contiguous entries in the arraw, */ 
/* with each entry marked with its heap type.                        */ 

typedef struct
{
  uint8_t    heap_type; 
  value      heap_data; 
} heap_value; 

/* heap types */ 
/* Again, we only need "one bit" to tell if a value is data or a pointer, but */ 
/* here we add types for the verbose mode                                     */
/* Currently the machine only supports three types of ojects on the heap : 
   ref cells, tuples (pairs actually), and function closures.   They have
   the following formats (H[p] representes the contents of the heap at location p): 

   ref cell at p: 
   ----------------- 
   H[p]   : HT_RF, 2
   H[p+1] : heap value 

   Tuple (pair) at p 
   ----------------- 
   H[p]   : HT_TU, 3
   H[p+1] : heap value (first) 
   H[p+2] : heap value (second) 

   Closure with environment of size k, at p 
   ----------------------------------------
   H[p]     : HT_CL, k+2 
   H[p+1]   : HT_CA, address of code 
   H[p+2]   : heap value 1
    ...        ... 
   H[p+k+1] : heap value k 

As an example, here is what a heap might look like that contains 
the value "(ref 17, f)", where f is a function needing 2 entries 
in its environment. 

H[0] : HT_RF, 2
H[1] : HT_DT, 17 
H[2] : HT_CL, 4 
H[3] : HT_CA, address of f 
H[4] : env entry 1 
H[5] : env entry 2
H[6] : HT_TU, 3
H[7] : HT_HP, 0 
H[8] : HT_HP, 2 

*/ 
#define HT_CA   0 /* code address                            */ 
#define HT_DT   1 /* data                                    */ 
#define HT_HP   2 /* heap pointer                            */ 
#define HT_RF   3 /* ref cell header, heap_data = size       */ 
#define HT_CL   4 /* closure header, heap_data = size        */ 
#define HT_TU   5 /* tuple header, heap_data = size          */ 

/* bytcodes have 0, 1, or 2 arguments */ 
typedef struct
{
  opcode     code; 
  argument   arg1; 
  argument   arg2; 
} bytecode; 

typedef struct
{
  flag         is_running; 
  code_range   instruction_count;   /* number of instructions                               */ 
  code_range   pc;                  /* Program counter                                      */
  stack_range  sp;                  /* stack pointer                                        */
  stack_range  fp;                  /* frame pointer                                        */
  heap_range   hp;                  /* heap pointer to next free                            */
  bytecode*    code;                /* Array of instructions                                */
  stack_value* stack;               /* the stack                                            */ 
  heap_value*  heap;                /* the heap                                             */ 
  int32_t      steps;               /* number of instructions executed (for verbose mode  ) */
} vsm_state;

/** opcodes 
 ** The machine has a stack and a heap. 
 ** Most operations work on the top of the stack. 
 ** The notation "foo ==> bar" means that before the operation "foo" was on 
 ** top of the stack, after the operation "foo" is replaced by "bar". 
 ** This notation implicitly describes changes 
 ** in the stack pointer.  Here are a few example: 
 **   a, b => a +b    : replace the top two stack values with their sum. 
 **   a    =>         : pop the top of the stack
 **        => v       : push value v on the stack. 
 ** The heap is represented as H.  Given a heap index p (think of it as a pointer), 
 ** the notation H[p] represents an object in the heap.  

 ** The function H2S translated heap types to stack types. 
 ** The function S2H translated stack types to heap types. 
*/ 
/* data opcodes */   /* ***********  informal semantics  **************************** */ 
#define OP_NOP     1 /* nop          : pc <- !pc +1                                   */
#define OP_PUSH    2 /* push c       : => c ; pc <- !pc +1                            */
#define OP_LOAD    3 /* load m       : => stack[fp + m] ; pc <- !pc +1                */
#define OP_STORE   4 /* store m      : a => ; store[fp + m] <- a ; pc <- !pc +1       */
#define OP_POP     5 /* pop          : a => ; pc <- !pc +1                            */
#define OP_ADD     6 /* add          : a, b => a + b; pc <- !pc +1                    */
#define OP_SUB     7 /* sub          : a, b => b - a; pc <- !pc +1                    */
#define OP_MUL     8 /* mul          : a, b => a * b; pc <- !pc +1                    */
/* control flow opcodes                                                               */ 
#define OP_HLT     9 /* hlt          : HALT the machine                               */
#define OP_JMP    10 /* jmp l        : pc <- l                                        */
#define OP_IFZ    11 /* ifz l        : a => ; if a == 0 then pc <- l else pc <- !pc+1 */
#define OP_IFP    12 /* ifp l        : a => ; if a => 0 then pc <- l else pc <- !pc+1 */
#define OP_IFN    13 /* ifn l        : a => ; if a <  0 then pc <- l else pc <- !pc+1 */
/* input/output opcodes                                                               */ 
#define OP_PRI    14 /* pri          : a => ; print out a as an integer; pc <- !pc+1  */ 
#define OP_RDI    15 /* rdi          : => a; read integer a; pc <- !pc+1              */ 
/* function-related opcodes */ 
#define OP_CALL   16 /* call f       : stack[sp] := fp; 
                                       fp := sp; 
                                       stack[sp + 1] := pc + 1;  
                                       sp := sp + 2; 
                                       pc := f                                        */
#define OP_RETURN 17 /* return n     : v := stack[sp -1];
                                       pc := stack[fp + 1]; 
                                       sp := fp; 
                                       fp := stack[sp]  
                                       stack[sp] := v 
                                       sp := sp + 1                                   */ 
#define OP_LARG     18 /* arg k       : push stack[fp - k] onto top of stack          */ 

#define OP_ASSIGN   21 /* assign      : H[p+1].heap_data = v.stack_data; 
                                        p, v => ; 
                                        pc <- !pc+1                                   */ 
#define OP_DEREF    22       /* deref k : p => H[p + k].heap_data; pc <- !pc+1        */ 
#define OP_REF      23       /* ref    : p = NewRef(); H[p].heap_data = v; v => p; pc <- !pc+1  */ 
#define OP_CALL_CLOSURE  24  /* callc n     : see lecture slides */ 
#define OP_MAKE_CLOSURE  25  /* closure g k : see lecture slides */ 
#define OP_MAKE_TUPLE    26  /* tuple k     : see lecture slides */ 
#define OP_PUSH_FUN      27  /* push_fun a  : see lecture slides*/ 
#define OP_LOCALS        28  /* locals n    : see lecture slides */ 


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



