#include "vsm.h"

/* 
 Compiler Construction 2013 
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
  state->is_running = 0;
  state->instruction_count = 0;
  state->code = (bytecode *) calloc(INSTRUCTION_LIMIT, sizeof(bytecode));
  state->stack = (value *) calloc(STACK_LIMIT, sizeof(value));
  return state;
}

void vsm_destruct(vsm_state *state)
{
  free(state->code);
  free(state->stack);
  free(state);
}
void vsm_abort(vsm_state *state)
{
  vsm_destruct(state);
  exit(1); 
}

void vsm_print_instruction(bytecode inst) {
  opcode   code = inst.code; 
  argument arg1 = inst.arg1;

  switch (code) {
        case OP_NOP:   { printf("nop\n");             break; }
        case OP_PUSH:  { printf("push %d \n", arg1);  break; }
        case OP_LOAD:  { printf("load %d\n", arg1);  break; }
        case OP_STORE: { printf("store %d\n", arg1); break; }
        case OP_POP:   { printf("pop\n");             break; }
        case OP_ADD:   { printf("add\n");             break; }
        case OP_SUB:   { printf("sub\n");             break; }
        case OP_MUL:   { printf("mul\n"); 	      break; } 
        case OP_HLT:   { printf("hlt\n"); 	      break; } 
        case OP_JMP:   { printf("jmp l%d\n", arg1);   break; } 
        case OP_IFZ:   { printf("ifz l%d\n", arg1);   break; }
        case OP_IFP:   { printf("ifp l%d\n", arg1);   break; }
        case OP_IFN:   { printf("ifn l%d\n", arg1);   break; }
        case OP_PRI:   { printf("pri\n");             break; }
        case OP_PRB:   { printf("prb\n");             break; }
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


code_range vsm_read_binary_instructions(FILE* fd, bytecode** bcode, uint8_t* version)
{
  code_range instruction_count = 0;
  code_range j = 0 ; 
  opcode op_code; 
  argument arg1; 

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
        case OP_PRB:
        case OP_ADD:
        case OP_SUB:
        case OP_MUL:
        case OP_HLT:
        case OP_POP:
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
        {
	  fread(&arg1, sizeof(argument), 1, fd); 
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
    j++; 
  }
  return instruction_count; 
}


void vsm_execute_instruction(vsm_state *state, bytecode instruction)
{
  opcode code   = instruction.code; 
  argument arg1 = instruction.arg1;
  switch (code) {
       case OP_NOP:
        {
	  state->pc++; 
	  break;
        }
        case OP_PUSH:
        {
	  state->stack[state->sp++] = arg1; 
	  state->pc++; 
	  break;
        }
        case OP_LOAD:
        {
	  state->stack[state->sp++] = state->stack[arg1]; 
	  state->pc++; 
	  break;
        }
        case OP_STORE:
        {
	  state->stack[arg1] = state->stack[--state->sp];
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
          state->sp = state->sp--; 
	  state->stack[state->sp - 1] = state->stack[state->sp] + state->stack[state->sp - 1];
	  state->pc++; 
	  break;
        }
        case OP_SUB:
        {
          state->sp = state->sp--; 
	  state->stack[state->sp - 1] = state->stack[state->sp - 1] - state->stack[state->sp];

	  state->pc++; 
	  break;
        }
        case OP_MUL:
        {
          state->sp = state->sp--; 
	  state->stack[state->sp - 1] = state->stack[state->sp] * state->stack[state->sp - 1];
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
	  if (state->stack[--state->sp] == 0) {
	    state->pc = arg1; 
	  } else {
	    state->pc++;
	  }
	  break;
        }
        case OP_IFP:
        {
	  if (state->stack[--state->sp] >= 0) {
	    state->pc = arg1; 
	  } else {
	    state->pc++;
	  }
	  break;
        }
        case OP_IFN:
        {
	  if (state->stack[--state->sp] < 0) {
	    state->pc = arg1; 
	  } else {
	    state->pc++;
	  }
	  break;
        }
        case OP_PRI:
        {
	  printf("%d\n", state->stack[--state->sp]);
	  state->pc++; 
	  break;
        }
        case OP_PRB:
        {
	  if (state->stack[--state->sp] == 0) {
	    printf("false\n");
	  } else {
	    printf("true\n");
	  }
	  state->pc++; 
	  break;
        }
        default:
        {
	  printf("vsm_execute_instruction: undefined opcode: %d\n", code);
	  vsm_abort(state);
        }
    }
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
  if (state->sp != 0) {
    printf("resulting in stack:\n"); 
    i = state->sp - 1;
    while (i != 0) {
      printf("%d : %d\n", i, state->stack[i]); 
      i = i - 1; 
    } 
    printf("%d : %d\n", i, state->stack[0]); 
  } else {
    printf("resulting in empty stack\n"); 
  } 
} 

void vsm_execute(vsm_state *state, flag verbose)
{
  state->is_running = 1; 
  while (state->is_running) {
    if (verbose) {
      printf("Doing %d : ", state->pc); 
      vsm_print_instruction(state->code[state->pc]);
    }
    vsm_execute_instruction(state, state->code[state->pc]);
    if (verbose) { vsm_print_state(state); } 
  }
}
