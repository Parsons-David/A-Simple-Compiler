/**********************************************
        CS415  Project 2
        Spring  2018
        Student Version
**********************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "symtab.h"

#define HASH_TABLE_SIZE 467

/*  --- Static VARIABLES AND FUNCTIONS --- */
static 
SymTabEntry **HashTable;

static
int hash(char *name) {
  int i;
  int hashValue = 1;
  
  for (i=0; i < strlen(name); i++) {
    hashValue = (hashValue * name[i]) % HASH_TABLE_SIZE;
  }

  return hashValue;
}


void
InitSymbolTable() {
  int i;
  int dummy;

  HashTable = (SymTabEntry **) malloc (sizeof(SymTabEntry *) * HASH_TABLE_SIZE);
  for (i=0; i < HASH_TABLE_SIZE; i++)
    HashTable[i] = NULL;
}


/* Returns pointer to symbol table entry, if entry is found */
/* Otherwise, NULL is returned */
SymTabEntry * 
lookup(char *name) {
  int currentIndex;
  int visitedSlots = 0;
  
  currentIndex = hash(name);
  while (HashTable[currentIndex] != NULL && visitedSlots < HASH_TABLE_SIZE) {
    if (!strcmp(HashTable[currentIndex]->name, name) )
      return HashTable[currentIndex];
    currentIndex = (currentIndex + 1) % HASH_TABLE_SIZE; 
    visitedSlots++;
  }
  return NULL;
}


void 
insert(char *name, Type_Expression type, int offset) {
  int currentIndex;
  int visitedSlots = 0;

  currentIndex = hash(name);
  while (HashTable[currentIndex] != NULL && visitedSlots < HASH_TABLE_SIZE) {
    if (!strcmp(HashTable[currentIndex]->name, name) ) 
      printf("*** WARNING *** in function \"insert\": %s has already an entry\n", name);
    currentIndex = (currentIndex + 1) % HASH_TABLE_SIZE; 
    visitedSlots++;
  }
  if (visitedSlots == HASH_TABLE_SIZE) {
    printf("*** ERROR *** in function \"insert\": No more space for entry %s\n", name);
    return;
  }
  
  HashTable[currentIndex] = (SymTabEntry *) malloc (sizeof(SymTabEntry));
  HashTable[currentIndex]->name = (char *) malloc (strlen(name)+1);
  strcpy(HashTable[currentIndex]->name, name);
  HashTable[currentIndex]->type = type; /* type expression */
  HashTable[currentIndex]->offset = offset; /* in bytes */
}

static
char *
TypeToString(Type_Expression type)
{
  switch (type) { 
    case TYPE_INT: return("integer");	break;
    case TYPE_BOOL: return("boolean"); break;
    case TYPE_ERROR: return ("undefined type"); break;
    default: printf(" *** ERROR in routine TypeToString \n");
      return ("ERROR in TypeToString: undefined type");
  }
}


void 
PrintSymbolTable() {
  int i;
  
  printf("\n --- Symbol Table ---------------\n\n");
  for (i=0; i < HASH_TABLE_SIZE; i++) {
    if (HashTable[i] != NULL) {
      printf("\t \"%s\" of type %s with offset %d\n", 
		HashTable[i]->name, TypeToString(HashTable[i]->type), HashTable[i]->offset); 
    }
  }
  printf("\n --------------------------------\n\n");
}


