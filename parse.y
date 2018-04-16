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

%union {
  tokentype token;
  regInfo targetReg;
  ID_node idNode;
  typeSize type_size;
  Type_Expression basic_type;
}

%token PROG PERIOD VAR
%token INT BOOL PRINT THEN IF DO
%token ARRAY OF
%token BEG END ASG
%token EQ NEQ LT LEQ GT GEQ AND OR TRUE FALSE
%token ELSE
%token WHILE
%token <token> ID ICONST

%type <targetReg> exp
%type <targetReg> lhs
%type <idNode> idlist
%type <type_size> type
%type <targetReg> vardcl
%type <basic_type> stype

%start program

%nonassoc EQ NEQ LT LEQ GT GEQ
%left '+' '-' AND
%left '*' OR

%nonassoc THEN
%nonassoc ELSE

%%
program : {
    emitComment("Assign STATIC_AREA_ADDRESS to register \"r0\"");
    emit(NOLABEL, LOADI, STATIC_AREA_ADDRESS, 0, EMPTY);
  }

PROG ID ';' block PERIOD {

}
;

block	: variables cmpdstmt {

  }
	;

variables: /* empty */
	| VAR vardcls {

  }
	;

vardcls	: vardcls vardcl ';' {

  }
	| vardcl ';' {
  }
	| error ';' {
    yyerror("***Error: illegal variable declaration\n");
  }
	;

vardcl	: idlist ':' type {
    // Get Type of list
    Type_Expression list_type = $3.type;
    LL * ID_Name_List = $1.ID_list;
    // Get Pointer to List of ID's
    LLNode * ptr = ID_Name_List->head;
    // Add each child to symbol table with their new type
    while(ptr != NULL){
      char * child_name = (char *) (ptr->data);
      /* printf("%s\n", child_name); */
      // Add ID to symbol table
      int new_offset = NextOffset($3.size);
      insert(child_name, $3.type, new_offset);
      ptr = ptr->next;

      sprintf(CommentBuffer, "Make %s a %d at Offset %d", child_name, $3.type, new_offset);
      emitComment(CommentBuffer);
    }
    // Free List of ID names
    free_LL(ID_Name_List);
  }
	;

idlist	: idlist ',' ID {
    // Get Pointer to current list of ID_list
    $$.ID_list = $1.ID_list;
    // Add another id to list of ID_list
    push($$.ID_list, $3.str);

    sprintf(CommentBuffer, "Add %s to a ID List", $3.str);
    emitComment(CommentBuffer);
  }
  | ID		{
    // Start a list of ID_list for the root ID list
    LL * ID_list = create_LL();
    // Add Token name to list
    push(ID_list, $1.str);

    sprintf(CommentBuffer, "Add %s to a ID List", $1.str);
    emitComment(CommentBuffer);
    // Add List pointer to parent
    $$.ID_list = ID_list;
  }
	;


/* type is a typeSize */
/* stype is a basic_type */
type	: ARRAY '[' ICONST ']' OF stype {
    // Pass up stype and len(Array) (i.e. ICONST value)
    $$.type = $6;
    $$.size = $3.num;
  }
  /* stype is a basic_type */
  | stype {
    // Pass up stype
    $$.type = $1;
    // size is 1 since its not an array
    $$.size = 1;
  }
	;

/* stype is a basic_type */
stype	: INT {
    // Pass up TYPE_INT
    $$ = TYPE_INT;
  }
  | BOOL {
    // Pass up TYPE_BOOL
    $$ = TYPE_BOOL;
  }
	;

stmtlist : stmtlist ';' stmt {

  }
	| stmt {

  }
  | error {
    yyerror("***Error: ';' expected or illegal statement \n");
  }
	;

stmt    : ifstmt {

  }
	| wstmt {

  }
	| astmt {

  }
	| writestmt {

  }
	| cmpdstmt {

  }
	;

cmpdstmt: BEG stmtlist END {

  }
	;

ifstmt :  ifhead THEN stmt ELSE stmt;

ifhead : IF condexp {

  }
  ;

writestmt: PRINT '(' exp ')' {
    int printOffset = -4; /* default location for printing */
    sprintf(CommentBuffer, "Code for \"PRINT\" from offset %d", printOffset);
    emitComment(CommentBuffer);
    emit(NOLABEL, STOREAI, $3.targetRegister, 0, printOffset);
    emit(NOLABEL, OUTPUTAI, 0, printOffset, EMPTY);
  }
	;

wstmt	: WHILE  {

  }
  condexp {

  }
  DO stmt  {

  }
	;


astmt : lhs ASG exp {
    if (! ((($1.type == TYPE_INT) && ($3.type == TYPE_INT)) ||
        (($1.type == TYPE_BOOL) && ($3.type == TYPE_BOOL)))) {
        printf("*** ERROR ***: Assignment types do not match.\n");
    }
    emit(NOLABEL, STORE, $3.targetRegister, $1.targetRegister, EMPTY);
  }
	;

/* lhs is a targetReg */
lhs	: ID			{ /* BOGUS  - needs to be fixed */
    // Lookup offset for given id
    // Send up reg where offset is stored to assign value at the given offset
    // Send up type for give id
    char * id_name = $1.str;
    SymTabEntry * id_entry = lookup(id_name);
    if(id_entry == NULL){
      // TODO : !!!!!!!!!!!!!!!!!!!!!
      // REPORT ERROR!
    }

    sprintf(CommentBuffer, "Assigning %s | offset %d", $1.str, id_entry->offset);
    emitComment(CommentBuffer);
    // Store offset
    int offset_reg = NextRegister();
    emit(NOLABEL, LOADI, id_entry->offset, offset_reg, EMPTY);

    int final_addr_reg = NextRegister();
    emit(NOLABEL, ADD, 0, offset_reg, final_addr_reg);

    $$.targetRegister = final_addr_reg;
    $$.type = id_entry->type;
  }

  |  ID '[' exp ']' {
    // Lookup offset for given id
    // Send up reg where offset is stored to assign value at the given offset,
    // based of exp
    // Send up type for give id
    char * id_name = $1.str;
    SymTabEntry * id_entry = lookup(id_name);
    if(id_entry == NULL){
      // TODO : !!!!!!!!!!!!!!!!!!!!!
      // REPORT ERROR!
    }
    // Evaluate Expersion
    // TODO: ???????????????????
    // Make sure expression is a valid offset
    // make sure expression is int
    if($3.type != TYPE_BOOL){
      // TODO : !!!!!!!!!!!!!!!!!!!!!
      // REPORT ERROR!
    }

    sprintf(CommentBuffer, "Assigning %s[] | offset %d", $1.str, id_entry->offset);
    emitComment(CommentBuffer);

    // Store 4 * exp offset in offset_reg
    // register for i4
    int four_reg = NextRegister();
    // loadI 4 => vReg
    emit(NOLABEL, LOADI, 4, four_reg, EMPTY);
    // exp.reg
    int exp_value_reg = $3.targetRegister;
    int offset_reg = NextRegister();
    emit(NOLABEL, MULT, four_reg, exp_value_reg, offset_reg);
    // Store offset_reg + id_offset_value in total_offset_reg
    // Load ID offset lookup into a vReg
    int id_offset_value_reg = NextRegister();
    emit(NOLABEL, LOADI, id_entry->offset, id_offset_value_reg, EMPTY);
    // Perform offset + id_offset addition
    int total_offset_reg = NextRegister();
    emit(NOLABEL, ADD, offset_reg, id_offset_value_reg, total_offset_reg);
    int final_addr_reg = NextRegister();
    emit(NOLABEL, ADD, 0, total_offset_reg, final_addr_reg);

    $$.targetRegister = final_addr_reg;
    $$.type = id_entry->type;
  }
  ;


exp	: exp '+' exp		{
    int newReg = NextRegister();
    if (! (($1.type == TYPE_INT) && ($3.type == TYPE_INT))) {
      printf("*** ERROR ***: Operator types must be integer.\n");
    }
    $$.type = $1.type;
    $$.targetRegister = newReg;
    emit(NOLABEL, ADD, $1.targetRegister, $3.targetRegister, newReg);
  }

  | exp '-' exp		{

  }

  | exp '*' exp		{

  }

  | exp AND exp		{

  }

  | exp OR exp    {

  }


  | ID {
    // Look up ID in symbol table
    // Lookup offset for given id
    char * id_name = $1.str;
    SymTabEntry * id_entry = lookup(id_name);
    if(id_entry == NULL){
      // TODO : !!!!!!!!!!!!!!!!!!!!!
      // REPORT ERROR!
    }
    // vReg to store value of ID
    int val_reg = NextRegister();
    sprintf(CommentBuffer, "Load %s into vReg %d", id_name, val_reg);
    emitComment(CommentBuffer);
    // loadAI 0, offset => vReg
    emit(NOLABEL, LOADAI, 0, id_entry->offset, val_reg);
    $$.targetRegister = val_reg;
    $$.type = id_entry->type;
  }

  | ID '[' exp ']'	{
    // Look Up ID
  }

  | ICONST  {
    int newReg = NextRegister();
    $$.targetRegister = newReg;
	  $$.type = TYPE_INT;
    sprintf(CommentBuffer, "vReg %d | %d", newReg, $1.num);
    emitComment(CommentBuffer);
	  emit(NOLABEL, LOADI, $1.num, newReg, EMPTY);
  }

  | TRUE {
    int newReg = NextRegister(); /* TRUE is encoded as value '1' */
    $$.targetRegister = newReg;
    $$.type = TYPE_BOOL;
    sprintf(CommentBuffer, "vReg %d | %d", newReg, 1);
    emitComment(CommentBuffer);
    emit(NOLABEL, LOADI, 1, newReg, EMPTY);
  }

  | FALSE  {
    int newReg = NextRegister(); /* FALSE is encoded as value '0' */
    $$.targetRegister = newReg;
    $$.type = TYPE_BOOL;
    sprintf(CommentBuffer, "vReg %d | %d", newReg, 0);
    emitComment(CommentBuffer);
    emit(NOLABEL, LOADI, 0, newReg, EMPTY);
  }

  | error {
    yyerror("***Error: illegal expression\n");
  }
	;


condexp	: exp NEQ exp		{

  }

  | exp EQ exp		{

  }

  | exp LT exp		{

  }

  | exp LEQ exp		{

  }

  | exp GT exp		{

  }

  | exp GEQ exp		{

  }

  | error {
    yyerror("***Error: illegal conditional expression\n");
  }
  ;

%%

void yyerror(char* s) {
  fprintf(stderr,"%s\n",s);
}


int main(int argc, char* argv[]) {

  printf("\n     CS415 Spring 2018 Compiler\n\n");

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
