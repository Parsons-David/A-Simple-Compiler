/**********************************************
        CS415  Project 3
        Spring  2018
        Author: Ulrich Kremer
**********************************************/

#include <stdio.h>
#include <stdlib.h>
#include "instrutil.h"
#include "valnum.h"

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


/*
 * emit implements CSE
 */

int
emit(int label_index,
     Opcode_Name opcode,
     int field1,
     int field2,
     int field3)
{
  char *label = " ";

  if (label_index < NOLABEL) {
    printf("ERROR: \"%d\" is an illegal label index.\n", label_index);
    return -1;
  }

  if (label_index > NOLABEL) {
    label = (char *) malloc(100);
    sprintf(label, "L%d:", label_index);
  };


  if (!cse_optimization_flag) {

    switch (opcode) { /* ---------------------- NON OPTIMIZED ------------------------------- */
    case NOP:
      fprintf(outfile, "%s\t nop \n", label);
      return -1;
      break;
    case ADD:
      fprintf(outfile, "%s\t add r%d, r%d \t=> r%d \n", label, field1, field2, field3);
      return field3;
      break;
    case SUB:
      fprintf(outfile, "%s\t sub r%d, r%d \t=> r%d \n", label, field1, field2, field3);
      return field3;
      break;
    case MULT:
      fprintf(outfile, "%s\t mult r%d, r%d \t=> r%d \n", label, field1, field2, field3);
      return field3;
      break;
    case LOADI:
      /* Example: loadI 1024 => r1 */
      fprintf(outfile, "%s\t loadI %d \t=> r%d \n", label, field1, field2);
      return field2;
      break;
    case LOADAI:
      /* Example: loadAI r1, 16 => r3 */
      fprintf(outfile, "%s\t loadAI r%d, %d \t=> r%d \n", label, field1, field2, field3);
      return field3;
      break;
    case STOREAI:
      /* Example: storeAI r1 => r2, 16 */
      fprintf(outfile, "%s\t storeAI r%d \t=> r%d, %d \n", label, field1, field2, field3);
      break;
    case OUTPUTAI:
      /* Example: outputAI r0, 16  */
      fprintf(outfile, "%s\t outputAI r%d, %d\n", label, field1, field2);
      break;
    default:
      fprintf(stderr, "Illegal instruction in \"emit\" \n");
    }
    return -1;
  }

  else {
    InitExpressionTable();
    char buffer[1000];
    char * current_key = NULL;
    int reg = -1;
    switch (opcode) { /* ---------------------- CSE OPTIMIZED ------------------------------- */

    case NOP:
      fprintf(outfile, "%s\t nop \n", label);
      return -1;
      break;

    case ADD:
      sprintf(buffer, "add r%d, r%d", field1, field2);
      current_key = strdup(buffer);
      reg = lookup_expression(current_key);
      // printf("\n%s -> %d\n", current_key, reg);
      // printf("LOOKING FOR %s\n", current_key);
      // print_Expression_Hashtable();
      if (reg == -1) {
        insert_expression(current_key, field3);

        sprintf(buffer, "add r%d, r%d", field2, field1);
        current_key = strdup(buffer);
        insert_expression(current_key, field3);

        fprintf(outfile, "%s\t add r%d, r%d \t=> r%d \n", label, field1, field2, field3);
        return field3;
      } else {
        return reg;
      }
      break;

    case SUB:
      sprintf(buffer, "sub r%d, r%d", field1, field2);
      current_key = strdup(buffer);
      reg = lookup_expression(current_key);
      // printf("\n%s -> %d\n", current_key, reg);
      // printf("LOOKING FOR %s\n", current_key);
      // print_Expression_Hashtable();
      if (reg == -1) {
        insert_expression(current_key, field3);
        fprintf(outfile, "%s\t sub r%d, r%d \t=> r%d \n", label, field1, field2, field3);
        return field3;
      } else {
        return reg;
      }
      break;

    case MULT:
      sprintf(buffer, "mult r%d, r%d", field1, field2);
      current_key = strdup(buffer);
      reg = lookup_expression(current_key);
      // printf("\n%s -> %d\n", current_key, reg);
      // printf("LOOKING FOR %s\n", current_key);
      // print_Expression_Hashtable();
      if (reg == -1) {
        insert_expression(current_key, field3);

        sprintf(buffer, "mult r%d, r%d", field2, field1);
        current_key = strdup(buffer);
        insert_expression(current_key, field3);

        fprintf(outfile, "%s\t mult r%d, r%d \t=> r%d \n", label, field1, field2, field3);
        return field3;
      } else {
        return reg;
      }
      break;

    case LOADI:
      sprintf(buffer, "loadI %d", field1);
      current_key = strdup(buffer);
      reg = lookup_expression(current_key);
      // printf("\n%s -> %d\n", current_key, reg);
      // printf("LOOKING FOR %s\n", current_key);
      // print_Expression_Hashtable();
      if (reg == -1) {
        insert_expression(current_key, field2);
        /* Example: loadI 1024 => r1 */
        fprintf(outfile, "%s\t loadI %d \t=> r%d \n", label, field1, field2);
        return field2;
      } else {
        return reg;
      }
      break;

    case LOADAI:
      sprintf(buffer, "loadAI r%d, %d", field1, field2);
      current_key = strdup(buffer);
      reg = lookup_expression(current_key);
      // printf("LOOKING FOR %s\n", current_key);
      // printf("\n%s -> %d\n", current_key, reg);
      // print_Expression_Hashtable();
      if (reg == -1) {
        insert_expression(current_key, field3);
        /* Example: loadAI r1, 16 => r3 */
        fprintf(outfile, "%s\t loadAI r%d, %d \t=> r%d \n", label, field1, field2, field3);
        return field3;
      } else {
        return reg;
      }
      break;

    case STOREAI:
      sprintf(buffer, "loadAI r%d, %d", field2, field3);
      current_key = strdup(buffer);
      reg = lookup_expression(current_key);
      // printf("LOOKING FOR %s\n", current_key);
      // printf("\n%s -> %d\n", current_key, reg);
      if(reg != -1){
        remove_key(current_key);
      }
      insert_expression(current_key, field1);
      // print_Expression_Hashtable();
      /* Example: storeAI r1 => r2, 16 */
      fprintf(outfile, "%s\t storeAI r%d \t=> r%d, %d \n", label, field1, field2, field3);
      break;
    case OUTPUTAI:
      /* Example: outputAI r0, 16  */
      fprintf(outfile, "%s\t outputAI r%d, %d\n", label, field1, field2);
      break;
    default:
      fprintf(stderr, "Illegal instruction in \"emit\" \n");
    }
  return -1;
  }
}
