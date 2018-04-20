%{
#include <stdio.h>
#include "attr.h"
#include "instrutil.h"
int yylex();
void yyerror(char * s);
#include "symtab.h"

FILE *outfile;
char *CommentBuffer;

%}

%union {tokentype token;
        regInfo targetReg;
       }

%token PROG PERIOD VAR
%token INT PRINT
%token BEG END ASG
%token <token> ID ICONST

%type <targetReg> exp

%start program

%left '+' '-'
%left '*'

%%
program : {
  int emit_return;
  if (cse_optimization_flag == 1)
  emitComment(" THIS IS CSE OPTIMIZED CODE");
  emitComment("Assign STATIC_AREA_ADDRESS to register \"r0\"");
  emit_return = emit(NOLABEL, LOADI, STATIC_AREA_ADDRESS, 0, EMPTY);
  } PROG ID ';' block PERIOD {

  }
	;

block	: variables cmpdstmt { }
	;

variables: /* empty */
	| VAR vardcls { }
	;

vardcls	: vardcls vardcl ';' { }
	| vardcl ';' { }
	| error ';' { yyerror("***Error: illegal variable declaration\n");}
	;

vardcl	: ID ':' INT { insert($1.str, NextOffset(1)); }
	;

stmtlist : stmtlist ';' stmt { }
	| stmt { }
  | error { yyerror("***Error: ';' expected or illegal statement \n");}
	;

stmt    : astmt { }
	| writestmt { }
	;

cmpdstmt: BEG stmtlist END { }
	;

writestmt: PRINT '(' exp ')' {
    int printOffset = -4; /* default location for printing */
    int emit_return;

    sprintf(CommentBuffer, "Code for \"PRINT\" from offset %d", printOffset);
    emitComment(CommentBuffer);
    emit_return = emit(NOLABEL, STOREAI, $3.targetRegister, 0, printOffset);
    emit_return = emit(NOLABEL, OUTPUTAI, 0, printOffset, EMPTY);
  }
;


astmt : ID ASG exp {
    SymTabEntry *entry = lookup($1.str);
    int emit_return;
    if (entry == NULL) {
      printf("ERROR: Variable \"%s\" not declared.\n");
      return -1;
    }

    emit_return = emit(NOLABEL, STOREAI, $3.targetRegister, 0, entry->offset);
  }
  ;


exp	: exp '+' exp {
    int newReg = NextRegister();
    int emit_return;
    emit_return = emit(NOLABEL, ADD, $1.targetRegister, $3.targetRegister, newReg);
    $$.targetRegister = emit_return;
  }

  | exp '-' exp {
    int newReg = NextRegister();
    int emit_return;
    emit_return = emit(NOLABEL, SUB, $1.targetRegister, $3.targetRegister, newReg);
    $$.targetRegister = emit_return;
  }


  | exp '*' exp {
    int newReg = NextRegister();
    int emit_return;
    emit_return = emit(NOLABEL, MULT, $1.targetRegister, $3.targetRegister, newReg);
    $$.targetRegister = emit_return;
  }

  | ID {
    int newReg = NextRegister();
    SymTabEntry *entry = lookup($1.str);
    int emit_return;

    if (entry == NULL) {
      printf("ERROR: Variable \"%s\" not declared.\n");
      return -1;
    }
    emit_return = emit(NOLABEL, LOADAI, 0, entry->offset, newReg);
    $$.targetRegister = emit_return;
  }

  | ICONST {
    int newReg = NextRegister();
    int emit_return;
    emit_return = emit(NOLABEL, LOADI, $1.num, newReg, EMPTY);
    $$.targetRegister = emit_return;
  }

	| error { yyerror("***Error: illegal expression\n");}
	;

%%

void yyerror(char* s) {
        fprintf(stderr,"%s\n",s);
        }

int cse_optimization_flag = 0;

int main(int argc, char* argv[]) {

  printf("\n     CS415 Spring 2018 Compiler\n");
  if (argc == 2 && strcmp(argv[1],"-O") == 0) {
    cse_optimization_flag = 1;
    printf("            CSE OPTIMIZER \n\n");
  }
  else
    printf("\n");

  outfile = fopen("iloc.out", "w");
  if (outfile == NULL) {
    printf("ERROR: cannot open output file \"iloc.out\".\n");
    return -1;
  }

  CommentBuffer = (char *) malloc(650);
  InitSymbolTable();

  printf("1\t");
  yyparse();
  printf("\n");

  PrintSymbolTable();

  fclose(outfile);

  return 1;
}
