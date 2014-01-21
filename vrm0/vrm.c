#include "vrm.h"

/* 
 Compiler Construction 2013 
 Computer Laboratory 
 University of Cambridge 
 Timothy G. Griffin (tgg22@cam.ac.uk) 
*/ 

uint8_t vrm_version = 0; 

vrm_state *vrm_create(void)
{
  vrm_state *state = (vrm_state *) malloc(sizeof(vrm_state));
  argument i = 0;
  flag f = 1;
  state->pc = 0;
  state->is_running = 0;
  state->instruction_count = 0;
  state->code = (bytecode *) calloc(INSTRUCTION_LIMIT, sizeof(bytecode));
  state->registers = (value *) calloc(NUMBER_OF_REGISTERS, sizeof(value));
  state->touched = (flag *) calloc(NUMBER_OF_REGISTERS, sizeof(flag));
  /* small hack : if i = 255 then i++ is 0 */ 
  while (f) { state->touched[i++] = 0; if (i == 0) {f = 0;} }

  return state;
}

void vrm_destruct(vrm_state *state)
{
  free(state->code);
  free(state->registers);
  free(state->touched);
  free(state);
}
void vrm_abort(vrm_state *state)
{
  vrm_destruct(state);
  exit(1); 
}

void vrm_print_instruction(bytecode inst) {
  opcode   code = inst.code; 
  argument arg1 = inst.arg1;
  argument arg2 = inst.arg2;
  argument arg3 = inst.arg3;
  switch (code) {
        case OP_NOP: { printf("nop\n");                               break; }
        case OP_SET: { printf("set r%d %d\n", arg1, arg2);            break; }
        case OP_MOV: { printf("mov r%d r%d\n", arg1, arg2);           break; }
        case OP_ADD: { printf("add r%d r%d r%d\n", arg1, arg2, arg3); break; }
        case OP_SUB: { printf("sub r%d r%d r%d\n", arg1, arg2, arg3); break; }
        case OP_MUL: { printf("mul r%d r%d r%d\n", arg1, arg2, arg3); break; }
        case OP_HLT: { printf("hlt\n");                               break; }
        case OP_JMP: { printf("jmp l%d\n", arg1);                     break; }
        case OP_IFZ: { printf("ifz r%d l%d\n", arg1, arg2);           break; } 
        case OP_IFP: { printf("ifp r%d l%d\n", arg1, arg2);           break; } 
        case OP_IFN: { printf("ifn r%d l%d\n", arg1, arg2);           break; } 
        case OP_PRI: { printf("pri r%d\n", arg1);                     break; } 
        case OP_PRB: { printf("prb r%d\n", arg1);                     break; } 
        default:
        {
	  printf("vrm_print_instruction: undefined opcode: %d\n", code);
	  exit(1);  
        }
  }
}

void vrm_print_instructions(bytecode *instrs, code_range count)
{
  code_range i;
  for (i = 0; i < count; i++) {
    printf("l%d : ", i);
    vrm_print_instruction(instrs[i]);
  }
}


code_range vrm_read_binary_instructions(FILE* fd, bytecode** bcode, uint8_t* version)
{
  code_range instruction_count = 0;
  code_range j = 0 ; 
  opcode op_code; 
  argument arg1; 
  argument arg2; 
  argument arg3; 

  fread(version, sizeof(uint8_t), 1, fd); 
  fread(&instruction_count, sizeof(code_range), 1, fd); 
  *bcode = (bytecode*)malloc(instruction_count * (sizeof(bytecode)));
  if (*bcode == NULL) {
    printf ("vrm_read_binary_instructions : malloc failed\n");
    exit (1);
  }
  while (0 != fread(&op_code, sizeof(opcode), 1, fd)) { 
    switch (op_code) {
        case OP_NOP:
        case OP_HLT:
        {
	  break;
        }
        case OP_JMP:
        case OP_PRI:
        case OP_PRB:
        {
	  fread(&arg1, sizeof(argument), 1, fd); 
	  break;
        }
        case OP_SET:
        case OP_MOV:
        case OP_IFZ:
        case OP_IFP:
        case OP_IFN:
        {
	  fread(&arg1, sizeof(argument), 1, fd); 
	  fread(&arg2, sizeof(argument), 1, fd); 
	  break;
        }
        case OP_ADD:
        case OP_SUB:
        case OP_MUL:
        {
	  fread(&arg1, sizeof(argument), 1, fd); 
	  fread(&arg2, sizeof(argument), 1, fd); 
	  fread(&arg3, sizeof(argument), 1, fd); 
	  break;
        }
        default:
        {
	  printf("vrm_read_binary_instructions: undefined opcode: %d\n", op_code);
	  exit(1);
        }
    }
    (*bcode)[j].code = op_code;
    (*bcode)[j].arg1 = arg1;
    (*bcode)[j].arg2 = arg2;
    (*bcode)[j].arg3 = arg3;
    j++; 
  }
  return instruction_count; 
}


void vrm_execute_instruction(vrm_state *state, bytecode instruction)
{
  opcode code   = instruction.code; 
  argument arg1 = instruction.arg1;
  argument arg2 = instruction.arg2;
  argument arg3 = instruction.arg3;

  switch (code) {
       case OP_NOP:
        {
	  state->pc++; 
	  break;
        }
        case OP_SET:
        {
	  state->registers[arg1] = arg2; 
	  state->touched[arg1] = 1; 
	  state->pc++; 
	  break;
        }
        case OP_MOV:
        {
	  state->registers[arg1] = state->registers[arg2];
	  state->touched[arg1] = 1; 
	  state->touched[arg2] = 1; 
	  state->pc++; 
	  break;
        }
        case OP_ADD:
        {
	  state->registers[arg1] = state->registers[arg2] + state->registers[arg3];
	  state->touched[arg1] = 1; 
	  state->touched[arg2] = 1; 
	  state->touched[arg3] = 1; 
	  state->pc++; 
	  break;
        }
        case OP_SUB:
        {
	  state->registers[arg1] = state->registers[arg2] - state->registers[arg3];
	  state->touched[arg1] = 1; 
	  state->touched[arg2] = 1; 
	  state->touched[arg3] = 1; 
	  state->pc++; 
	  break;
        }
        case OP_MUL:
        {
	  state->registers[arg1] = state->registers[arg2] * state->registers[arg3];
	  state->touched[arg1] = 1; 
	  state->touched[arg2] = 1; 
	  state->touched[arg3] = 1; 
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
	  if (state->registers[arg1] == 0) {
	    state->pc = arg2; 
	  } else {
	    state->pc++;
	  }
	  state->touched[arg1] = 1; 
	  break;
        }
        case OP_IFP:
        {
	  if (state->registers[arg1] >= 0) {
	    state->pc = arg2; 
	  } else {
	    state->pc++;
	  }
	  state->touched[arg1] = 1; 
	  break;
        }
        case OP_IFN:
        {
	  if (state->registers[arg1] < 0) {
	    state->pc = arg2; 
	  } else {
	    state->pc++;
	  }
	  state->touched[arg1] = 1; 
	  break;
        }
        case OP_PRI:
        {
	  printf("%d\n", state->registers[arg1]);
	  state->touched[arg1] = 1; 
	  state->pc++; 
	  break;
        }
        case OP_PRB:
        {
	  if (state->registers[arg1] == 0) {
	    printf("false\n");
	  } else {
	    printf("true\n");
	  }
	  state->touched[arg1] = 1; 
	  state->pc++; 
	  break;
        }
        default:
        {
	  printf("vrm_execute_instruction: undefined opcode: %d\n", code);
	  vrm_abort(state);
        }
    }
}

void vrm_load(vrm_state *state, bytecode *instrs, code_range count)
{
  code_range i;
  for (i = 0; i < count; i++) {
    state->instruction_count++;
    state->code[i] = instrs[i];
  }
}

void vrm_print_state(vrm_state *state) { 
  argument i = 0; 
  flag f = 1; 
  printf("resulting in registers:\n"); 
  while(f) { 
    if (state->touched[i] == 1) {
      printf("r%d : %d\n", i, state->registers[i]); 
    } 
    if (++i == 0) {f = 0;} 
  }
} 

void vrm_execute(vrm_state *state, flag verbose)
{
  state->is_running = 1; 
  while (state->is_running) {
    if (verbose) {
      printf("Doing %d : ", state->pc); 
      vrm_print_instruction(state->code[state->pc]);
    }
    vrm_execute_instruction(state, state->code[state->pc]);
    if (verbose) { vrm_print_state(state); } 
  }
}
