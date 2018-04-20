/**********************************************
        CS415  Project 3
        Spring  2018
        Student Version
**********************************************/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "instrutil.h"
#include "valnum.h"

#define HASH_TABLE_SIZE 467

/*  --- Static VARIABLES AND FUNCTIONS --- */
static
SubExpression **Expression_HashTable;

void flushExpressionTable(){
  Expression_HashTable = NULL;
}

static
int hash(char *key) {
  int i;
  int hashValue = 1;

  for (i=0; i < strlen(key); i++) {
    hashValue = (hashValue * key[i]) % HASH_TABLE_SIZE;
  }

  return hashValue;
}


void
InitExpressionTable() {
  if(!(Expression_HashTable == NULL)){
    return;
  }
  // printf("INIT Expression Table\n");
  int i;
  int dummy;

  Expression_HashTable = (SubExpression **) malloc (sizeof(SubExpression *) * HASH_TABLE_SIZE);
  for (i=0; i < HASH_TABLE_SIZE; i++)
    Expression_HashTable[i] = NULL;
}


/* Returns pointer to symbol table entry, if entry is found */
/* Otherwise, NULL is returned */
SubExpression *
lookup_expression(char *key) {
  int currentIndex;
  int visitedSlots = 0;

  currentIndex = hash(key);
  while (Expression_HashTable[currentIndex] != NULL && visitedSlots < HASH_TABLE_SIZE) {
    if (!strcmp(Expression_HashTable[currentIndex]->key, key) )
      return Expression_HashTable[currentIndex];
    currentIndex = (currentIndex + 1) % HASH_TABLE_SIZE;
    visitedSlots++;
  }
  return NULL;
}


void
insert_expression(char *key, int virtualRegister) {
  int currentIndex;
  int visitedSlots = 0;

  currentIndex = hash(key);
  while (Expression_HashTable[currentIndex] != NULL && visitedSlots < HASH_TABLE_SIZE) {
    if (!strcmp(Expression_HashTable[currentIndex]->key, key) )
      printf("*** WARNING *** in function \"insert\": %s has already an entry\n", key);
    currentIndex = (currentIndex + 1) % HASH_TABLE_SIZE;
    visitedSlots++;
  }
  if (visitedSlots == HASH_TABLE_SIZE) {
    printf("*** ERROR *** in function \"insert\": No more space for entry %s\n", key);
    return;
  }

  Expression_HashTable[currentIndex] = (SubExpression *) malloc (sizeof(SubExpression));
  Expression_HashTable[currentIndex]->key = (char *) malloc (strlen(key)+1);
  strcpy(Expression_HashTable[currentIndex]->key, key);
  Expression_HashTable[currentIndex]->virtualRegister = virtualRegister; /* in bytes */
}

void
PrintExpressionTable() {
  int i;

  printf("\n --- Expression Table ---------------\n\n");
  for (i=0; i < HASH_TABLE_SIZE; i++) {
    if (Expression_HashTable[i] != NULL) {
      printf("\t \"%s\" of type %s with virtualRegister %d\n",
		Expression_HashTable[i]->key, "integer", Expression_HashTable[i]->virtualRegister);
    }
  }
  printf("\n --------------------------------\n\n");
}
