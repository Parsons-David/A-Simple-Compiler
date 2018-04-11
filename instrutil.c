/**********************************************
        CS415  Project 2
        Spring  2018
        Author: Ulrich Kremer
**********************************************/

#include <stdio.h>
#include <stdlib.h>
#include "instrutil.h"

static next_register = 1; /* register 0 is reserved */
static next_label = 0;
static next_offset = 0;

int NextRegister() 
{
  if (next_register < MAX_VIRTUAL_REGISTERS)
    return next_register++;
  else {
    printf("*** ERROR *** Reached limit of virtual registers: %d\n", next_register);
    exit(-1);
  }
}

int NextLabel() 
{
  return next_label++;
}

int NextOffset(int units) 
{ 
  int current_offset = next_offset;
  next_offset = next_offset + 4*units;
  return current_offset;
}

void
emitComment(char *comment)
{
  fprintf(outfile, "\t// %s\n", comment);  
}

void
emit(int label_index,
     Opcode_Name opcode, 
     int field1, 
     int field2, 
     int field3) 
{
  char *label = " ";
  
  if (label_index < NOLABEL) {
    printf("ERROR: \"%d\" is an illegal label index.\n", label_index);
    return;
  }
    
  if (label_index > NOLABEL) {
    label = (char *) malloc(100);
    sprintf(label, "L%d:", label_index);
  };
  
    switch (opcode) { /* ---------------------- NON OPTIMIZED ------------------------------- */
    case NOP: 
      fprintf(outfile, "%s\t nop \n", label);
      break;
    case ADDI:
      /* Example: addI r1, 1024 => r1 */
      fprintf(outfile, "%s\t addI r%d, %d \t=> r%d \n", label, field1, field2, field3);
      break;
    case ADD:
      fprintf(outfile, "%s\t add r%d, r%d \t=> r%d \n", label, field1, field2, field3);
      break;
    case SUBI:
      /* Example: subI r1, 1024 => r1 */
      fprintf(outfile, "%s\t subI r%d, %d \t=> r%d \n", label, field1, field2, field3);
      break;
    case SUB: 
      fprintf(outfile, "%s\t sub r%d, r%d \t=> r%d \n", label, field1, field2, field3);
      break;
    case MULT: 
      fprintf(outfile, "%s\t mult r%d, r%d \t=> r%d \n", label, field1, field2, field3);
      break;
    case AND_INSTR:
      fprintf(outfile, "%s\t and r%d, r%d \t=> r%d \n", label, field1, field2, field3);
      break;
    case OR_INSTR:
      fprintf(outfile, "%s\t or r%d, r%d \t=> r%d \n", label, field1, field2, field3);
      break;
    case LOAD: 
      /* Example: load r1 => r1 */
      fprintf(outfile, "%s\t load r%d \t=> r%d \n", label, field1, field2);
      break;
    case LOADI: 
      /* Example: loadI 1024 => r1 */
      fprintf(outfile, "%s\t loadI %d \t=> r%d \n", label, field1, field2);
      break;
    case LOADAI: 
      /* Example: loadAI r1, 16 => r3 */
      fprintf(outfile, "%s\t loadAI r%d, %d \t=> r%d \n", label, field1, field2, field3);
      break;
    case LOADAO: 
      /* Example: loadAO r1, r2 => r3 */
      fprintf(outfile, "%s\t loadAO r%d, r%d \t=> r%d \n", label, field1, field2, field3);
      break;
    case STORE: 
      /* Example: store r1 => r2 */
      fprintf(outfile, "%s\t store r%d \t=> r%d \n", label, field1, field2);
      break;
    case STOREAI: 
      /* Example: storeAI r1 => r2, 16 */
      fprintf(outfile, "%s\t storeAI r%d \t=> r%d, %d \n", label, field1, field2, field3);
      break;
    case STOREAO: 
      /* Example: storeAO r1 => r2, r3 */
      fprintf(outfile, "%s\t storeAO r%d \t=> r%d, r%d \n", label, field1, field2, field3);
      break;
    case BR: 
      /* Example: br L1 */
      fprintf(outfile, "%s\t br L%d\n", label, field1);
      break;
    case CBR: 
      /* Example: cbr r1 => L1, L2 */
      fprintf(outfile, "%s\t cbr r%d \t=> L%d, L%d\n", label, field1, field2, field3);
      break;
    case CMPLT: 
      /* Example: cmp_LT r1, r2 => r3 */
      fprintf(outfile, "%s\t cmp_LT r%d, r%d \t=> r%d\n", label, field1, field2, field3);
      break;
    case CMPLE: 
      /* Example: cmp_LE r1, r2 => r3 */
      fprintf(outfile, "%s\t cmp_LE r%d, r%d \t=> r%d\n", label, field1, field2, field3);
      break;
    case CMPGT: 
      /* Example: cmp_GT r1, r2 => r3 */
      fprintf(outfile, "%s\t cmp_GT r%d, r%d \t=> r%d\n", label, field1, field2, field3);
      break;
    case CMPGE: 
      /* Example: cmp_GE r1, r2 => r3 */
      fprintf(outfile, "%s\t cmp_GE r%d, r%d \t=> r%d\n", label, field1, field2, field3);
      break;
    case CMPEQ: 
      /* Example: cmp_EQ r1, r2 => r3 */
      fprintf(outfile, "%s\t cmp_EQ r%d, r%d \t=> r%d\n", label, field1, field2, field3);
      break;
    case CMPNE: 
      /* Example: cmp_NE r1, r2 => r3 */
      fprintf(outfile, "%s\t cmp_NE r%d, r%d \t=> r%d\n", label, field1, field2, field3);
      break;
    case OUTPUTAI: 
      /* Example: outputAI r0, 16  */
      fprintf(outfile, "%s\t outputAI r%d, %d\n", label, field1, field2);
      break;
    default:
      fprintf(stderr, "Illegal instruction in \"emit\" \n");
    }
}




