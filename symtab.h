/**********************************************
        CS415  Project 3
        Spring  2018
        Student Version
**********************************************/


#ifndef SYMTAB_H
#define SYMTAB_H

#include <string.h>
#include "attr.h"

/* The symbol table implementation uses a single hash     */
/* function. Starting from the hashed position, entries   */
/* are searched in increasing index order until a         */
/* matching entry is found, or an empty entry is reached. */


typedef struct { /* need to augment this */
  char *name;
  int offset;
} SymTabEntry;

extern
void InitSymbolTable();

extern
SymTabEntry * lookup(char *name);

extern
void insert(char *name, int offset);

extern
void PrintSymbolTable();


#endif
