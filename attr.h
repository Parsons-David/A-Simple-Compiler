/**********************************************
        CS415  Project 2
        Spring  2015
        Student Version
**********************************************/

#ifndef ATTR_H
#define ATTR_H

#include <stdlib.h>
#include <errno.h>
#include <stdio.h>

// START LL TYPE DEFINITION/FUNCTIONS
typedef struct LLNode LLNode;
struct LLNode{
  void* data;
  LLNode* next;
};

typedef struct LL{
  LLNode * head;
  int length;
} LL;

LL * create_LL();

void free_LL();

int push(LL * list, void* data);

void* pop(LL * list);

int remove_target(LL * list, void* target_data);

// END LL TYPE DEFINITION/FUNCTIONS

typedef union {int num; char *str;} tokentype;

typedef enum type_expression {TYPE_INT=0, TYPE_BOOL, TYPE_ERROR} Type_Expression;

typedef struct {
  Type_Expression type;
  int targetRegister;
} regInfo;

typedef struct ID_node ID_node;
struct ID_node {
  LL * ID_list;
};

// Bundled Type and Size Struct
typedef struct {
  // Type of the new vars
  Type_Expression type;
  // Size of the new vars, > 1 for arrays..
  int size;
} typeSize;


#endif
