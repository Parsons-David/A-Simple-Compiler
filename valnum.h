/**********************************************
        CS415  Project 3
        Spring  2018
        Student Version
**********************************************/

#ifndef VALNUM_H
#define VALNUM_H

#include <string.h>

/* INSERT WHATEVER YOU NEED FOR THE VALUE NUMBER HASH FUNCTION */
typedef struct {
  char *key;
  int virtualRegister;
} SubExpression;

extern
void InitExpressionTable();

extern
SubExpression * lookup_expression(char *key);

extern
void insert_expression(char *key, int virtualRegister);

extern
void PrintExpressionTable();

#endif
