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
  whileJump while_jump;
  ifJump if_jump;
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
%type <targetReg> condexp
%type <while_jump> WHILE
%type <if_jump> ifhead


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
      // FREE STRING
      ptr = ptr->next;

      sprintf(CommentBuffer, "Declare %s | Type %d | Offset %d", child_name, $3.type, new_offset);
      emitComment(CommentBuffer);
    }
    // Free List of ID names
    free_LL(ID_Name_List);
  }
	;

idlist	: idlist ',' ID {
    // Get Pointer to current list of ID_list
    $$.ID_list = $1.ID_list;
    // TODO : Copy String
    // Add another id to list of ID_list
    push($$.ID_list, $3.str);

    sprintf(CommentBuffer, "ID List.add(%s)", $3.str);
    emitComment(CommentBuffer);
  }
  | ID		{
    // Start a list of ID_list for the root ID list
    LL * ID_list = create_LL();
    // TODO : Copy String
    // Add Token name to list
    push(ID_list, $1.str);

    sprintf(CommentBuffer, "ID List.add(%s)", $1.str);
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

ifstmt :  ifhead THEN {
    // True Label
    emit($1.true_label, NOP, EMPTY, EMPTY, EMPTY);
    sprintf(CommentBuffer, "THEN BODY");
    emitComment(CommentBuffer);
  } stmt {
    // Jump to end Label
    emit(NOLABEL, BR, $1.end_label,EMPTY, EMPTY);
  } ELSE {
    // False Label
    emit($1.false_label, NOP, EMPTY, EMPTY, EMPTY);
    sprintf(CommentBuffer, "ELSE BODY");
    emitComment(CommentBuffer);
  } stmt {
    // End Label
    emit($1.end_label, NOP, EMPTY, EMPTY, EMPTY);
    sprintf(CommentBuffer, "END OF IF");
    emitComment(CommentBuffer);
  };

ifhead : IF
  {
  sprintf(CommentBuffer, "Control for IF STATEMENT");
  emitComment(CommentBuffer);
  } condexp {
    // Store Labels
    $$.true_label = NextLabel();
    $$.false_label = NextLabel();
    $$.end_label = NextLabel();
    // conditional Break on condexp reg
    emit(NOLABEL, CBR, $3.targetRegister, $$.true_label, $$.false_label);
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
    sprintf(CommentBuffer, "Control for While Statement");
    emitComment(CommentBuffer);
    int stmtLabel = NextLabel();
    int bodyLabel = NextLabel();
    int endLabel = NextLabel();
    emit(stmtLabel, NOP, EMPTY, EMPTY, EMPTY);
    $1.stmt_label = stmtLabel;
    $1.true_label = bodyLabel;
    $1.false_label = endLabel;
  } condexp {
    // Jump on value of condexp
    emit(NOLABEL, CBR, $3.targetRegister, $1.true_label, $1.false_label);
    // Start of Body of While stmt
    sprintf(CommentBuffer, "Body of While Statement");
    emitComment(CommentBuffer);
    emit($1.true_label, NOP, EMPTY, EMPTY, EMPTY);
  } DO stmt  {
    sprintf(CommentBuffer, "Loop Break");
    emitComment(CommentBuffer);
    // Add Loop Jump
    emit(NOLABEL, BR, $1.stmt_label, EMPTY, EMPTY);

    sprintf(CommentBuffer, "***End of While Statement***");
    emitComment(CommentBuffer);
    // End of While stmt
    emit($1.false_label, NOP, EMPTY, EMPTY, EMPTY);
  }
	;


astmt : lhs ASG exp {
    if (! ((($1.type == TYPE_INT) && ($3.type == TYPE_INT)) ||
        (($1.type == TYPE_BOOL) && ($3.type == TYPE_BOOL)))) {
        /* printf("*** ERROR ***: Assignment types do not match.\n"); */
    }
    // TODO : IMPLEMENT?
    /* printf("\n*** ERROR ***: Assignment types do not match.\n"); */
    sprintf(CommentBuffer, "Store Value in v%d | At Offset in v%d", $3.targetRegister, $1.targetRegister);
    emitComment(CommentBuffer);
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

      /* printf("\n*** ERROR ***: Variable %s not declared.\n", ...); */
      // TODO : !!!!!!!!!!!!!!!!!!!!!
      // REPORT ERROR!
    }
    /* // TODO : IMPLEMENT */
    /* printf("\n*** ERROR ***: Variable %s is not a scalar variable.\n", ...); */

    int final_addr_reg = NextRegister();
    sprintf(CommentBuffer, "Load %s Offset | %d + 1024 | Into v%d", $1.str, id_entry->offset, final_addr_reg);
    emitComment(CommentBuffer);
    // Store offset
    int offset_reg = NextRegister();
    emit(NOLABEL, LOADI, id_entry->offset, offset_reg, EMPTY);

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
      /* printf("\n*** ERROR ***: Variable %s not declared.\n", ...); */
    }
    // TODO : IMPLEMENT
    /* printf("\n*** ERROR ***: Variable %s is not an array variable.\n", ...); */
    // Evaluate Expersion
    // TODO: ???????????????????
    // Make sure expression is a valid offset
    // make sure expression is int
    if($3.type == TYPE_BOOL){
      // TODO : !!!!!!!!!!!!!!!!!!!!!
      // REPORT ERROR!
      /* printf("\n*** ERROR ***: Array variable %s index type must be integer.\n", ...); */
    }

    int final_addr_reg = NextRegister();
    sprintf(CommentBuffer, "Load %s[exp] Offset | %d + 1024 + (4 * exp) | Into v%d", $1.str, id_entry->offset, final_addr_reg);
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
    int newReg = NextRegister();
    if (! (($1.type == TYPE_INT) && ($3.type == TYPE_INT))) {
      printf("*** ERROR ***: Operator types must be integer.\n");
    }
    $$.type = $1.type;
    $$.targetRegister = newReg;
    emit(NOLABEL, SUB, $1.targetRegister, $3.targetRegister, newReg);
  }

  | exp '*' exp		{
    int newReg = NextRegister();
    if (! (($1.type == TYPE_INT) && ($3.type == TYPE_INT))) {
      printf("*** ERROR ***: Operator types must be integer.\n");
    }
    $$.type = $1.type;
    $$.targetRegister = newReg;
    emit(NOLABEL, MULT, $1.targetRegister, $3.targetRegister, newReg);
  }

  | exp AND exp		{
      int newReg = NextRegister();
      if (! (($1.type == TYPE_BOOL) && ($3.type == TYPE_BOOL))) {
        printf("*** ERROR ***: Operator types must be boolean.\n");
      }
      $$.type = $1.type;
      $$.targetRegister = newReg;
      emit(NOLABEL, AND_INSTR, $1.targetRegister, $3.targetRegister, newReg);
  }

  | exp OR exp    {
      int newReg = NextRegister();
      if (! (($1.type == TYPE_BOOL) && ($3.type == TYPE_BOOL))) {
        printf("\n*** ERROR ***: Operand type must be boolean.\n");
      }
      $$.type = $1.type;
      $$.targetRegister = newReg;
      emit(NOLABEL, OR_INSTR, $1.targetRegister, $3.targetRegister, newReg);
  }


  | ID {
    // Look up ID in symbol table
    // Lookup offset for given id
    char * id_name = $1.str;
    SymTabEntry * id_entry = lookup(id_name);
    if(id_entry == NULL){
      // TODO : !!!!!!!!!!!!!!!!!!!!!
      // REPORT ERROR!
      /* printf("\n*** ERROR ***: Variable %s not declared.\n", ...); */
    }
    // TODO : IMPLEMENT
    /* printf("\n*** ERROR ***: Variable %s is not a scalar variable.\n", ...); */
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
    // TODO : IMPLEMENT
    // Lookup offset for given id
    // Send up reg where value of id at offset is stored
    // Send up type for give id
    char * id_name = $1.str;
    SymTabEntry * id_entry = lookup(id_name);
    if(id_entry == NULL){
      // TODO : !!!!!!!!!!!!!!!!!!!!!
      // REPORT ERROR!
      /* printf("\n*** ERROR ***: Variable %s not declared.\n", ...); */
      /* printf("\n*** ERROR ***: Array variable %s index type must be integer.\n", ...); */
    }
    // TODO : IMPLEMENT
    /* printf("\n*** ERROR ***: Variable %s is not an array variable.\n", ...); */
    // Evaluate Expersion
    // TODO: ???????????????????
    // Make sure expression is a valid offset
    // make sure expression is int
    if($3.type == TYPE_BOOL){
      // TODO : !!!!!!!!!!!!!!!!!!!!!
      // REPORT ERROR!
      /* printf("\n*** ERROR ***: Array variable %s index type must be integer.\n", ...); */
    }

    int final_value_reg = NextRegister();
    sprintf(CommentBuffer, "Load %s[exp] Value | %d + 1024 + (4 * exp) | Into v%d", $1.str, id_entry->offset, final_value_reg);
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
    emit(NOLABEL, LOAD, final_addr_reg, final_value_reg, EMPTY);
    $$.targetRegister = final_value_reg;
    $$.type = id_entry->type;
  }

  | ICONST  {
    int newReg = NextRegister();
    $$.targetRegister = newReg;
	  $$.type = TYPE_INT;
    sprintf(CommentBuffer, "Load %d | Into v%d", $1.num, newReg);
    emitComment(CommentBuffer);
	  emit(NOLABEL, LOADI, $1.num, newReg, EMPTY);
  }

  | TRUE {
    int newReg = NextRegister(); /* TRUE is encoded as value '1' */
    $$.targetRegister = newReg;
    $$.type = TYPE_BOOL;
    sprintf(CommentBuffer, "Load %d \"TRUE\" | Into v%d", 1, newReg);
    emitComment(CommentBuffer);
    emit(NOLABEL, LOADI, 1, newReg, EMPTY);
  }

  | FALSE  {
    int newReg = NextRegister(); /* FALSE is encoded as value '0' */
    $$.targetRegister = newReg;
    $$.type = TYPE_BOOL;
    sprintf(CommentBuffer, "Load %d \"FALSE\" | Into v%d", 0, newReg);
    emitComment(CommentBuffer);
    emit(NOLABEL, LOADI, 0, newReg, EMPTY);
  }

  | error {
    yyerror("***Error: illegal expression\n");
  }
	;


condexp	: exp NEQ exp		{
    if (! ((($1.type == TYPE_INT) && ($3.type == TYPE_INT)) ||
        (($1.type == TYPE_BOOL) && ($3.type == TYPE_BOOL)))) {
        printf("*** ERROR ***: Assignment types do not match.\n");
        /* printf("\n*** ERROR ***: == or != operator with different types.\n"); */
    }
    // Register to store comparison result in
    int resultReg = NextRegister();
    sprintf(CommentBuffer, "Store v%d != v%d | Into v%d", $1.targetRegister, $3.targetRegister, resultReg);
    emitComment(CommentBuffer);
    emit(NOLABEL, CMPNE, $1.targetRegister, $3.targetRegister, resultReg);
    $$.targetRegister = resultReg;
  }

  | exp EQ exp		{
    if (! ((($1.type == TYPE_INT) && ($3.type == TYPE_INT)) ||
        (($1.type == TYPE_BOOL) && ($3.type == TYPE_BOOL)))) {
        printf("*** ERROR ***: Assignment types do not match.\n");
        /* printf("\n*** ERROR ***: == or != operator with different types.\n"); */
    }
    // Register to store comparison result in
    int resultReg = NextRegister();
    sprintf(CommentBuffer, "Store v%d == v%d | Into v%d", $1.targetRegister, $3.targetRegister, resultReg);
    emitComment(CommentBuffer);
    emit(NOLABEL, CMPEQ, $1.targetRegister, $3.targetRegister, resultReg);
    $$.targetRegister = resultReg;
  }

  | exp LT exp		{

    /* printf("\n*** ERROR ***: Relational operator with illegal type.\n"); */
    if (! ((($1.type == TYPE_INT) && ($3.type == TYPE_INT)) ||
        (($1.type == TYPE_BOOL) && ($3.type == TYPE_BOOL)))) {
        printf("*** ERROR ***: Assignment types do not match.\n");
    }
    // Register to store comparison result in
    int resultReg = NextRegister();
    sprintf(CommentBuffer, "Store v%d < v%d | Into v%d", $1.targetRegister, $3.targetRegister, resultReg);
    emitComment(CommentBuffer);
    emit(NOLABEL, CMPLT, $1.targetRegister, $3.targetRegister, resultReg);
    $$.targetRegister = resultReg;
  }

  | exp LEQ exp		{
    /* printf("\n*** ERROR ***: Relational operator with illegal type.\n"); */
    if (! ((($1.type == TYPE_INT) && ($3.type == TYPE_INT)) ||
        (($1.type == TYPE_BOOL) && ($3.type == TYPE_BOOL)))) {
        printf("*** ERROR ***: Assignment types do not match.\n");
    }
    // Register to store comparison result in
    int resultReg = NextRegister();
    sprintf(CommentBuffer, "Store v%d <= v%d | Into v%d", $1.targetRegister, $3.targetRegister, resultReg);
    emitComment(CommentBuffer);
    emit(NOLABEL, CMPLE, $1.targetRegister, $3.targetRegister, resultReg);
    $$.targetRegister = resultReg;
  }

  | exp GT exp		{
      /* printf("\n*** ERROR ***: Relational operator with illegal type.\n"); */
      if (! ((($1.type == TYPE_INT) && ($3.type == TYPE_INT)) ||
          (($1.type == TYPE_BOOL) && ($3.type == TYPE_BOOL)))) {
          printf("*** ERROR ***: Assignment types do not match.\n");
      }
      // Register to store comparison result in
      int resultReg = NextRegister();
      sprintf(CommentBuffer, "Store v%d > v%d | Into v%d", $1.targetRegister, $3.targetRegister, resultReg);
      emitComment(CommentBuffer);
      emit(NOLABEL, CMPGT, $1.targetRegister, $3.targetRegister, resultReg);
      $$.targetRegister = resultReg;
  }

  | exp GEQ exp		{
      /* printf("\n*** ERROR ***: Relational operator with illegal type.\n"); */
      if (! ((($1.type == TYPE_INT) && ($3.type == TYPE_INT)) ||
          (($1.type == TYPE_BOOL) && ($3.type == TYPE_BOOL)))) {
          printf("*** ERROR ***: Assignment types do not match.\n");
      }
      // Register to store comparison result in
      int resultReg = NextRegister();
      sprintf(CommentBuffer, "Store v%d >= v%d | Into v%d", $1.targetRegister, $3.targetRegister, resultReg);
      emitComment(CommentBuffer);
      emit(NOLABEL, CMPGE, $1.targetRegister, $3.targetRegister, resultReg);
      $$.targetRegister = resultReg;

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
