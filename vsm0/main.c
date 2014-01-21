#include "vsm.h"
/* 
 Compiler Construction 2013 
 Computer Laboratory 
 University of Cambridge 
 Timothy G. Griffin (tgg22@cam.ac.uk) 
*/ 

char* usage = "usage: vsm0 [-v | -s] <binary program file>\n      -v = verbose mode, -s = just print code and exit\n"; 

/* run compiled code */ 
int main(int argc, char **argv) 
{
  FILE* fd; 
  char* fname; 
  bytecode* code_in  = NULL;
  vsm_state *state   = vsm_create();
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
	  printf("%s", usage); vsm_destruct(state); exit(1);
	} 
      } 
    } else {
      printf("%s", usage); vsm_destruct(state); exit(1);
    } 
  }
  /* read in binary code and run it */ 
  fd = fopen(fname, "r+b"); 
  if(fd) {
    inst_count = vsm_read_binary_instructions(fd, &code_in, &version);
    fclose(fd);
    if (verbose || show) { 
      printf("VSM version = %d\n", version);            
      printf("instruction count = %d\n", inst_count);            
      printf("instructions:  \n");            
      vsm_print_instructions(code_in, inst_count); 
    }
    if (0 == show) { 
      vsm_load(state, code_in, inst_count); 
      if (verbose) printf("\nrunning ...  \n");  
      vsm_execute(state, verbose); 
    } 
    vsm_destruct(state); 
    free(code_in); 
  } else {
    printf("Cannot open file %s\n", fname);
    vsm_destruct(state);  exit(1);
  } 
  exit(0);
} 
