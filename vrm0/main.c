#include "vrm.h"
/* 
 Compiler Construction 2013 
 Computer Laboratory 
 University of Cambridge 
 Timothy G. Griffin (tgg22@cam.ac.uk) 
*/ 

char* usage = "usage: vrm0 [-v | -s] <binary program file>\n      -v = verbose mode, -s = just print code and exit\n"; 

/* run compiled code */ 
int main(int argc, char **argv) 
{
  FILE* fd; 
  char* fname; 
  bytecode* code_in  = NULL;
  vrm_state *state   = vrm_create();
  uint8_t version    = 0;
  code_range inst_count = 0; 
  flag verbose       = 0; 
  flag show          = 0; 

  /* process command line arguments */ 
  if(argc == 2) { 
    fname = argv[1];
  } else {
    if (argc == 3) {
      fname = argv[2];
      if (0 == strcmp("-v", argv[1])) {
        verbose = 1; 
      } else { 
	if (0 == strcmp("-s", argv[1])) { 
	  show = 1; 
	} else {
	  printf("%s", usage); vrm_destruct(state); exit(1);
	} 
      } 
    } else {
      printf("%s", usage); vrm_destruct(state); exit(1);
    } 
  }
  /* read in binary code and run it */ 
  fd = fopen(fname, "r+b"); 
  if(fd) {
    inst_count = vrm_read_binary_instructions(fd, &code_in, &version);
    fclose(fd);
    if (verbose || show) { 
      printf("VRM version = %d\n", version);            
      printf("instruction count = %d\n", inst_count);            
      printf("instructions:  \n");            
      vrm_print_instructions(code_in, inst_count); 
    }
    if (0 == show) { 
      vrm_load(state, code_in, inst_count); 
      if (verbose) printf("\nrunning ...  \n");  
      vrm_execute(state, verbose); 
    } 
    vrm_destruct(state); 
    free(code_in); 
  } else {
    printf("Cannot open file %s\n", fname);
    vrm_destruct(state);  exit(1);
  } 
  exit(0);
} 
