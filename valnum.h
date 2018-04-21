/**********************************************
        CS415  Project 3
        Spring  2018
        Student Version
**********************************************/

#ifndef VALNUM_H
#define VALNUM_H

#include <string.h>

/* INSERT WHATEVER YOU NEED FOR THE VALUE NUMBER HASH FUNCTION */

#include <stdlib.h>
#include <errno.h>
#include <stdio.h>

typedef struct {
  char *key;
  int virtualRegister;
} SubExpression;

extern
void InitExpressionTable();

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

typedef struct HashtableItem{
  char * key;
  int data;
} HashtableItem;

HashtableItem * create_HashtableItem(char * key, int data);

typedef struct Hashtable{
  LL ** table;
  int size;
} Hashtable;

Hashtable * create_Hashtable(int size);

void free_Hashtable(Hashtable * target);

int hash(char * key);

int insert_expression(char * key, int data);

int remove_key(char * target_key);

int lookup_expression(char * target_key);

void print_Expression_Hashtable();

#endif
