#include "vsm.h"

/* 
 Compiler Construction 2014
 Computer Laboratory 
 University of Cambridge 
 Timothy G. Griffin (tgg22@cam.ac.uk) 
*/ 

uint8_t vsm_version = 0; 

vsm_state *vsm_create(void)
{
  vsm_state *state = (vsm_state *) malloc(sizeof(vsm_state));
  state->pc = 0;
  state->sp = 0;
  state->fp = 0;
  state->hp = 0;
  state->is_running = 0;
  state->instruction_count = 0;
  state->code = (bytecode *) calloc(INSTRUCTION_LIMIT, sizeof(bytecode));
  state->stack = (stack_value *) calloc(STACK_LIMIT, sizeof(stack_value));
  state->heap = (heap_value *) calloc(HEAP_LIMIT, sizeof(heap_value));
  state->steps = 0;
  return state;
}

void vsm_destruct(vsm_state *state)
{
  free(state->code);
  free(state->stack);
  free(state->heap);
  free(state);
}
void vsm_abort(vsm_state *state)
{
  vsm_destruct(state);
  fflush(stdout); 
  exit(1); 
}

void vsm_print_instruction(bytecode inst) {
  opcode   code = inst.code; 
  argument arg1 = inst.arg1;
  argument arg2 = inst.arg2;
  switch (code) {
        case OP_NOP:     { printf("nop\n");             break; }
        case OP_PUSH:    { printf("push %d \n", arg1);  break; }
        case OP_PUSH_FUN: { printf("pushfun %d \n", arg1);  break; }
        case OP_LOCALS:  { printf("locals %d \n", arg1);  break; }
        case OP_LOAD:    { printf("load %d\n", arg1);   break; }
        case OP_STORE:   { printf("store %d\n", arg1);  break; }
        case OP_POP:     { printf("pop\n");             break; }
        case OP_ADD:     { printf("add\n");             break; }
        case OP_SUB:     { printf("sub\n");             break; }
        case OP_MUL:     { printf("mul\n"); 	        break; } 
        case OP_HLT:     { printf("hlt\n"); 	        break; } 
        case OP_JMP:     { printf("jmp l%d\n", arg1);   break; } 
        case OP_IFZ:     { printf("ifz l%d\n", arg1);   break; }
        case OP_IFP:     { printf("ifp l%d\n", arg1);   break; }
        case OP_IFN:     { printf("ifn l%d\n", arg1);   break; }
        case OP_PRI:     { printf("pri\n");             break; }
        case OP_RDI:     { printf("rdi\n");             break; }
        case OP_CALL:    { printf("call %d\n", arg1);   break; }
        case OP_RETURN : { printf("return %d\n", arg1); break; }
        case OP_LARG:    { printf("larg %d\n", arg1);   break; }
        case OP_ASSIGN:  { printf("assign\n");          break; }
        case OP_REF:     { printf("ref\n");             break; }
        case OP_DEREF:   { printf("deref %d\n", arg1);  break; }
        case OP_MAKE_TUPLE: { printf("tuple %d\n", arg1);   break; }
        case OP_CALL_CLOSURE: { printf("callc %d\n", arg1); break; }
        case OP_MAKE_CLOSURE: { printf("closure %d %d\n", arg1, arg2);   break; }
        default:
        {
	  printf("vsm_print_instruction: undefined opcode: %d\n", code); 
	  exit(1);  
        }
  }
}

void vsm_print_instructions(bytecode *instrs, code_range count)
{
  code_range i;
  for (i = 0; i < count; i++) {
    printf("l%d : ", i);
    vsm_print_instruction(instrs[i]);
  }
}

void print_stack_type(vsm_state *state, uint8_t st) { 
      switch (st) {
      case ST_CA : {printf ("CA"); break;} 
      case ST_DT : {printf ("DT"); break;} 
      case ST_HP : {printf ("HP"); break;} 
      case ST_FP : {printf ("FP"); break;} 
      default : 
         {
  	    printf("vsm_print_state : undefined stack_type : %d\n", st);
	    vsm_abort(state);
         } 
    }
} 

void print_heap_type(vsm_state *state, uint8_t ht) { 
      switch (ht) {
      case HT_CA : {printf ("CA"); break;} 
      case HT_DT : {printf ("DT"); break;} 
      case HT_HP : {printf ("HP"); break;} 
      case HT_RF : {printf ("RF"); break;} 
      case HT_TU : {printf ("TU"); break;} 
      case HT_CL : {printf ("CL"); break;} 
      default : 
         {
  	    printf("print_heap_type : undefined heap_type : %d\n", ht);
	    vsm_abort(state);
         } 
      }
}


code_range vsm_read_binary_instructions(FILE* fd, bytecode** bcode, uint8_t* version)
{
  code_range instruction_count = 0;
  code_range j = 0 ; 
  opcode op_code; 
  argument arg1; 
  argument arg2; 

  fread(version, sizeof(uint8_t), 1, fd); 
  fread(&instruction_count, sizeof(code_range), 1, fd); 
  *bcode = (bytecode*)malloc(instruction_count * (sizeof(bytecode)));
  if (*bcode == NULL) {
    printf ("vsm_read_binary_instructions : malloc failed\n");
    exit (1);
  }
  while (0 != fread(&op_code, sizeof(opcode), 1, fd)) { 
    switch (op_code) {
        case OP_NOP:
        case OP_PRI:
        case OP_RDI:
        case OP_ADD:
        case OP_SUB:
        case OP_MUL:
        case OP_HLT:
        case OP_POP:
        case OP_ASSIGN:
        case OP_REF:
        {
	  break;
        }
        case OP_LOAD:
        case OP_STORE:
        case OP_JMP:
        case OP_IFZ:
        case OP_IFP:        
        case OP_IFN:
        case OP_PUSH:
        case OP_PUSH_FUN:
        case OP_LOCALS:
        case OP_RETURN:
        case OP_CALL:
        case OP_LARG:
        case OP_CALL_CLOSURE:
        case OP_MAKE_TUPLE:
        case OP_DEREF:
        {
	  fread(&arg1, sizeof(argument), 1, fd); 
	  break;
        }
        case OP_MAKE_CLOSURE:
        {
	  fread(&arg1, sizeof(argument), 1, fd); 
	  fread(&arg2, sizeof(argument), 1, fd); 
	  break;
        }
        default:
        {
  	  printf("vsm_read_binary_instructions: undefined opcode: %d\n", op_code); 
	  exit(1); 
        }
    }
    (*bcode)[j].code = op_code;
    (*bcode)[j].arg1 = arg1;
    (*bcode)[j].arg2 = arg2;
    j++; 
  }
  return instruction_count; 
}


void inc_sp(vsm_state *state)
{
    if ((STACK_LIMIT - 2) < state->sp) { 
      printf("stack limit %d exceeded!\n", STACK_LIMIT - 2);
      vsm_abort(state); 
    } 
    state->sp++;
}

void advance_hp(vsm_state *state, int i)
{
    /* GARBAGE COLLECTOR NEEDED ! */
    if ((HEAP_LIMIT - 2) < state->hp + i) { 
      printf("heap limit %d exceeded!\n", HEAP_LIMIT - 2);
      vsm_abort(state); 
    } 
    state->hp = state->hp + i;
}

heap_range allocate_ref(vsm_state* state)
{
  heap_range p = state->hp; 
  advance_hp(state, 2);  
  state->heap[p].heap_type = HT_RF; 
  state->heap[p].heap_data = 2; 
  return p; 
} 

heap_range allocate_tuple(vsm_state* state, int k)
{
  heap_range p = state->hp; 
  advance_hp(state, k + 1);  
  state->heap[p].heap_type = HT_TU; 
  state->heap[p].heap_data = k + 1; 
  return p; 
} 

heap_range allocate_closure(vsm_state* state, value g, int k)
{
  heap_range p = state->hp; 
  advance_hp(state, k + 2);  
  state->heap[p].heap_type = HT_CL; 
  state->heap[p].heap_data = k + 2; 
  state->heap[p + 1].heap_type = HT_CA; 
  state->heap[p + 1].heap_data = g; 
  return p; 
} 

void vsm_execute_instruction(vsm_state *state, bytecode instruction)
{
  opcode code   = instruction.code; 
  argument arg1 = instruction.arg1;
  argument arg2 = instruction.arg2;

  switch (code) {
       case OP_NOP:
        {
	  state->pc++; 
	  break;
        }
        case OP_PUSH:
        {
	  state->stack[state->sp].stack_type = ST_DT; 
	  state->stack[state->sp].stack_data = arg1; 
          inc_sp(state); 
	  state->pc++; 
	  break;
        }
        case OP_PUSH_FUN:
        {
	  state->stack[state->sp].stack_type = ST_CA; 
	  state->stack[state->sp].stack_data = arg1; 
          inc_sp(state); 
	  state->pc++; 
	  break;
        }
        case OP_LOCALS :
        {
          argument i; 
          for (i = 0; i != arg1 ; i++) { 
  	     state->stack[state->sp].stack_type = ST_DT; 
   	     state->stack[state->sp].stack_data = 0; 
             inc_sp(state); 
          } 
	  state->pc++; 
	  break;
        }

        case OP_LOAD:
        {
	  state->stack[state->sp].stack_data = state->stack[state->fp + arg1 + 1].stack_data; 
	  state->stack[state->sp].stack_type = state->stack[state->fp + arg1 + 1].stack_type; 
          inc_sp(state); 
	  state->pc++; 
	  break;
        }
        case OP_STORE:
        {
	  state->sp--; 
	  state->stack[state->fp + arg1 + 1].stack_data = state->stack[state->sp].stack_data;
	  state->stack[state->fp + arg1 + 1].stack_type = state->stack[state->sp].stack_type;
	  state->pc++; 
	  break;
        }
        case OP_POP:
        {
	  state->sp--; 
	  state->pc++; 
	  break;
        }
        case OP_ADD:
        {
          state->sp--; 
	  state->stack[state->sp - 1].stack_data = state->stack[state->sp].stack_data + state->stack[state->sp - 1].stack_data;
	  state->pc++; 
	  break;
        }
        case OP_SUB:
        {
          state->sp--; 
          /* this seems to be wrong in vsm0 ! */ 
	  state->stack[state->sp - 1].stack_data = state->stack[state->sp].stack_data - state->stack[state->sp - 1].stack_data;
	  state->pc++; 
	  break;
        }
        case OP_MUL:
        {
          state->sp--; 
	  state->stack[state->sp - 1].stack_data = state->stack[state->sp].stack_data * state->stack[state->sp - 1].stack_data;
	  state->pc++; 
	  break;
        }
        case OP_HLT:
        {
	  state->is_running = 0; 
	  break;
        }
        case OP_JMP:
        {
	  state->pc = arg1; 
	  break;
        }
        case OP_IFZ:
        {
          state->sp--; 
	  if (state->stack[state->sp].stack_data == 0) {
	    state->pc = arg1; 
	  } else {
	    state->pc++;
	  }
	  break;
        }
        case OP_IFP :
        {
          state->sp--; 
	  if (state->stack[state->sp].stack_data >= 0) {
	    state->pc = arg1; 
	  } else {
	    state->pc++;
	  }
	  break;
        }
        case OP_IFN :
        {
          state->sp--; 
	  if (state->stack[state->sp].stack_data < 0) {
	    state->pc = arg1; 
	  } else {
	    state->pc++;
	  }
	  break;
        }
        case OP_PRI :
        {
	  printf("slang output> %d\n", state->stack[--state->sp].stack_data);
	  state->pc++; 
	  break;
        }
        case OP_RDI :
        {
          int input_int;
          int result; 
          printf ("slang input> "); 
          result = scanf("%d", &input_int);
          /* keep executing this operation until input is legal! */ 
          if (result != 0) { 
            state->stack[state->sp].stack_type = ST_DT; 
            state->stack[state->sp].stack_data = input_int; 
	    inc_sp(state); 
	    state->pc++;
          } 
	  break;
        }
        case OP_CALL :
	{ 
	  state->stack[state->sp].stack_type = ST_FP; 
	  state->stack[state->sp].stack_data = state->fp; 
	  state->fp = state->sp; 
          inc_sp(state); 
	  state->stack[state->sp].stack_type = ST_CA; 
	  state->stack[state->sp].stack_data = state->pc + 1; 
          inc_sp(state); 
          state->pc = arg1; 
	  break;
        }
        case OP_RETURN :
	{ 
          int i; 
          value return_value = state->stack[state->sp -1].stack_data; 
          uint8_t return_type = state->stack[state->sp -1].stack_type; 
	  state->pc = state->stack[state->fp + 1].stack_data; 
          state->sp = state->fp; 
	  state->fp = state->stack[state->sp].stack_data; 
          for (i = 0; i < arg1; i++)
     	  {
	    state->sp--; 
     	  }          
          state->stack[state->sp].stack_type = return_type;  
          state->stack[state->sp].stack_data = return_value; 
	  state->sp++; 
	  break;
        }
        case OP_LARG :
        {
          state->stack[state->sp].stack_type = state->stack[state->fp - arg1].stack_type; 
          state->stack[state->sp].stack_data = state->stack[state->fp - arg1].stack_data; 
          inc_sp(state);  
	  state->pc++; 
	  break;
        }
        case OP_REF :
        {
          value p = allocate_ref(state); 
          state->heap[p + 1].heap_data = state->stack[state->sp -1].stack_data; 
          state->heap[p + 1].heap_type = state->stack[state->sp -1].stack_type; 
          state->stack[state->sp - 1].stack_data = p; 
          state->stack[state->sp - 1].stack_type = ST_HP;  
	  state->pc++; 
	  break;
        }
        case OP_ASSIGN :
        {
          value p = state->stack[state->sp - 2].stack_data; 
	  state->sp--; 
          state->heap[p + 1].heap_data = state->stack[state->sp].stack_data; 
	  state->sp--; 
	  state->pc++; 
	  break;
        }
        case OP_DEREF :
        {
          value p = arg1 + state->stack[state->sp - 1].stack_data; 
          state->stack[state->sp -1].stack_data = state->heap[p].heap_data; 
          state->stack[state->sp -1].stack_type = state->heap[p].heap_type; 
	  state->pc++; 
	  break;
        }
        case OP_MAKE_TUPLE :
        {
          int i; 
          value p = allocate_tuple(state, arg1); 
	  state->sp--; 
          for (i = arg1; 0 < i; i--) { 
             state->heap[p + i].heap_data = state->stack[state->sp].stack_data; 
             state->heap[p + i].heap_type = state->stack[state->sp].stack_type; 
    	     state->sp--; 
          } 
    	  state->sp++; 
          state->stack[state->sp].stack_data = p; 
          state->stack[state->sp].stack_type = ST_HP;  
    	  state->sp++; 
	  state->pc++; 
	  break;
        }
        case OP_MAKE_CLOSURE :
        {
          int i; 
          value p = allocate_closure(state, arg1, arg2); 
          state->heap[p + 1].heap_data = arg1; 
          state->heap[p + 1].heap_type = HT_CA; 
	  state->sp--; 
          for (i = arg2; 0 < i; i--) { 
             state->heap[p + 1 + i].heap_data = state->stack[state->sp].stack_data; 
             state->heap[p + 1 + i].heap_type = state->stack[state->sp].stack_type; 
    	     state->sp--; 
          } 
    	  state->sp++; 
          state->stack[state->sp].stack_data = p; 
          state->stack[state->sp].stack_type = ST_HP;  
    	  state->sp++; 
	  state->pc++; 
	  break;
        }
        case OP_CALL_CLOSURE :
	{ 
          uint8_t t = state->stack[state->sp - arg1].stack_type; 
          value d = state->stack[state->sp - arg1].stack_data; 
	  state->stack[state->sp].stack_type = ST_FP; 
	  state->stack[state->sp].stack_data = state->fp; 
	  state->fp = state->sp; 
          inc_sp(state); 
	  state->stack[state->sp].stack_type = ST_CA; 
	  state->stack[state->sp].stack_data = state->pc + 1; 
          inc_sp(state); 
          if (t == ST_CA) {
	    state->pc = d; 
          } else if (t == ST_HP) {
	    state->pc = state->heap[d + 1].heap_data; 
          } else {
	    printf("unexpected stack type : %d\n", t);
	    vsm_abort(state);
          }
	  break;
        }

        default:
        {
	  printf("vsm_execute_instruction: undefined opcode: %d\n", code);
	  vsm_abort(state);
        }
  }
  state->steps++; 
}

void vsm_load(vsm_state *state, bytecode *instrs, code_range count)
{
  code_range i;
  for (i = 0; i < count; i++) {
    state->instruction_count++;
    state->code[i] = instrs[i];
  }
}

void vsm_print_state(vsm_state *state) { 
  stack_range i; 
  printf("stack = \n"); 
  if (state->sp != 0) {
    for (i = state->sp - 1; 0 < i ; i--) {
      printf("%d : ", i); 
      print_stack_type(state, state->stack[i].stack_type); 
      printf(" %d\n", state->stack[i].stack_data); 
    } 
    printf("%d : ", 0); 
    print_stack_type(state, state->stack[0].stack_type); 
    printf(" %d\n", state->stack[0].stack_data); 
  } else { 
      printf("empty stack\n"); 
  } 
  if (state->hp != 0) { 
      printf("heap = \n"); 
      for (i = 0; i < state->hp; i++) {
	printf("%d : ", i); 
	print_heap_type(state, state->heap[i].heap_type); 
	printf(" %d \n", state->heap[i].heap_data); 
      } 
  }
} 

void vsm_execute(vsm_state *state, flag verbose)
{
  state->is_running = 1; 
  while (state->is_running) {
    if (verbose) {
      printf("step %d) doing %d : ", state->steps, state->pc); 
      vsm_print_instruction(state->code[state->pc]);
    }
    vsm_execute_instruction(state, state->code[state->pc]);
    if (verbose) { vsm_print_state(state); } 
  }
}
