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

/* vardcl is just a token */
/* idlist is a idNode */
/* type is a type_size */
vardcl	: idlist ':' type {
    // Get Type of list
    // We are going to assign this to each ID in the list
    Type_Expression list_type = $3.type;

    // Get Pointer to List of ID's
    LL * ID_Name_List = $1.ID_list;

    // Get head of LL
    LLNode * ptr = ID_Name_List->head;

    // Add each child to symbol table with their new type
    while(ptr != NULL){
      char * child_name = (char *) (ptr->data);
      /* printf("%s\n", child_name); */
      // Add ID to symbol table
      int new_offset = NextOffset($3.size);
      insert(child_name, $3.type, new_offset, $3.var_type);

      sprintf(CommentBuffer, "Declare %s | Type %d | Offset %d", child_name, $3.type, new_offset);
      emitComment(CommentBuffer);

      // FREE STRING
      free(child_name);

      // Get next item in list
      ptr = ptr->next;

    }

    // Free List of ID names
    free_LL(ID_Name_List);
  }
	;

/* idlist is a idNode */
/* ID is just a token */
idlist	: idlist ',' ID {
    // Get Pointer to current list of ID_list
    $$.ID_list = $1.ID_list;

    // Add another id to list of ID_list
    // strdup duplicates the ID string
    // This is freed when the LL is freed
    push($$.ID_list, strdup($3.str));

    sprintf(CommentBuffer, "ID List.add(%s)", $3.str);
    emitComment(CommentBuffer);
  }
  /* ID is just a token */
  | ID {
    // Start a list of ID_list for the root ID list
    LL * ID_list = create_LL();

    // strdup duplicates the ID string
    // This is freed when the LL is freed
    // Add Token name to list
    push(ID_list, strdup($1.str));

    sprintf(CommentBuffer, "ID List.add(%s)", $1.str);
    emitComment(CommentBuffer);

    // Add List pointer to parent
    $$.ID_list = ID_list;
  }
	;


/* type is a typeSize */
/* stype is a basic_type */
type : ARRAY '[' ICONST ']' OF stype {
    // PASS UP ARRAY TYPE
    $$.var_type = TYPE_ARRAY;
    // Pass up stype type
    $$.type = $6;
    // and len(Array) (i.e. ICONST value)
    $$.size = $3.num;
  }
  /* stype is a basic_type */
  | stype {
    // PASS UP SCALAR TYPE
    $$.var_type = TYPE_SCALAR;
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

stmt : ifstmt {

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

/* ifstmt is just a token */
/* ifhead is a ifJump */
/* stmt is just a token */
/* ELSE is just a token */
/* stmt is just a token */
ifstmt :  ifhead THEN {
    // Emit True Label set by ifhead
    emit($1.true_label, NOP, EMPTY, EMPTY, EMPTY);

    sprintf(CommentBuffer, "THEN BODY");
    emitComment(CommentBuffer);
  } stmt {
    // emit Jump to end Label set by ifhead
    emit(NOLABEL, BR, $1.end_label,EMPTY, EMPTY);
  } ELSE {
    // Emit False Label set by ifhead
    emit($1.false_label, NOP, EMPTY, EMPTY, EMPTY);

    sprintf(CommentBuffer, "ELSE BODY");
    emitComment(CommentBuffer);
  } stmt {
    // Emit End Label set by ifhead
    emit($1.end_label, NOP, EMPTY, EMPTY, EMPTY);

    sprintf(CommentBuffer, "END OF IF");
    emitComment(CommentBuffer);
  };

/* ifhead is a ifJump */
/* IF is just a token */
/* condexp is a regInfo */
ifhead : IF {
    sprintf(CommentBuffer, "Control for IF STATEMENT");
    emitComment(CommentBuffer);
  } condexp {
    // Store Labels
    // Label for if conditional is true
    $$.true_label = NextLabel();
    // Label for if conditional is false
    $$.false_label = NextLabel();
    // Label for end of if body
    // where true/then jumps after it runs it's body
    $$.end_label = NextLabel();
    // conditional Break on condexp reg
    // Jump to true or false based on condexp value
    emit(NOLABEL, CBR, $3.targetRegister, $$.true_label, $$.false_label);
  }
  ;

/* UNTOCHED FROM BASE CODE */
writestmt: PRINT '(' exp ')' {
    int printOffset = -4; /* default location for printing */
    sprintf(CommentBuffer, "Code for \"PRINT\" from offset %d", printOffset);
    emitComment(CommentBuffer);
    emit(NOLABEL, STOREAI, $3.targetRegister, 0, printOffset);
    emit(NOLABEL, OUTPUTAI, 0, printOffset, EMPTY);
  }
	;
/* wstmt is just a token */
/* WHILE is a whileJump  */
/* condexp is a regInfo */
/* DO is just a token */
/* stmt is just a token */
wstmt	: WHILE {
    sprintf(CommentBuffer, "Control for WHILE STATEMENT");
    emitComment(CommentBuffer);
    // Label for control logic
    // We need to loop to this label on every iteration
    int stmtLabel = NextLabel();
    // Label for when condexp is true
    int trueLabel = NextLabel();
    // Label for when condexp is false
    int falseLabel = NextLabel();
    // Emit statement label now
    // in needs to come before any logic
    // so we can run that logic after a jump
    emit(stmtLabel, NOP, EMPTY, EMPTY, EMPTY);
    // Store labels for later tokens
    $1.stmt_label = stmtLabel;
    $1.true_label = trueLabel;
    $1.false_label = falseLabel;
  } condexp {
    // Jump on value of condexp
    // Emit label that jumps on condexp
    emit(NOLABEL, CBR, $3.targetRegister, $1.true_label, $1.false_label);
    // Start of Body of While stmt
    sprintf(CommentBuffer, "BODY of WHILE STATEMENT");
    emitComment(CommentBuffer);
    // Emit label that represents logic if condexp is true
    emit($1.true_label, NOP, EMPTY, EMPTY, EMPTY);
  } DO stmt  {
    sprintf(CommentBuffer, "LOOP BREAK");
    emitComment(CommentBuffer);
    // Add Loop Jump
    // Emit jump back to while condition logic
    emit(NOLABEL, BR, $1.stmt_label, EMPTY, EMPTY);

    sprintf(CommentBuffer, "END of WHILE STATEMENT");
    emitComment(CommentBuffer);
    // End of While stmt
    // Emit label at the end of the while statement
    // We need to jump here when the condexp is false
    // when we need to exit the loop
    emit($1.false_label, NOP, EMPTY, EMPTY, EMPTY);
  }
	;

/* lhs is a regInfo */
/* ASG is just a token */
/* exp is a regInfo */
astmt : lhs ASG exp {
    // Make sure the lhs and rhs types match, emit error if they aren't
    if (! ((($1.type == TYPE_INT) && ($3.type == TYPE_INT)) ||
        (($1.type == TYPE_BOOL) && ($3.type == TYPE_BOOL)))) {
        printf("\n*** ERROR ***: Assignment types do not match.\n");
    }
    // TODO : BLACK MAGIC???
    sprintf(CommentBuffer, "Store Value in v%d | At Offset in v%d", $3.targetRegister, $1.targetRegister);
    emitComment(CommentBuffer);
    // Store lhs value into rhs offset
    emit(NOLABEL, STORE, $3.targetRegister, $1.targetRegister, EMPTY);
  }
  ;

/* lhs is a targetReg */
lhs	: ID {
    // Lookup offset for given id
    // Send up reg where offset is stored to assign value at the given offset
    // Send up type for give id
    char * id_name = $1.str;
    SymTabEntry * id_entry = lookup(id_name);
    if(id_entry == NULL){
      // TODO : !!!!!!!!!!!!!!!!!!!!!
      // REPORT ERROR!
      printf("\n*** ERROR ***: Variable %s not declared.\n", id_name);
      return 1;
    }
    /* // TODO : IMPLEMENT */
    if(id_entry->var_type != TYPE_SCALAR){
      printf("\n*** ERROR ***: Variable %s is not a scalar variable.\n", id_name);
    }

    // Register where the final computed address will live
    int final_addr_reg = NextRegister();
    sprintf(CommentBuffer, "Load %s Offset | %d + 1024 | Into v%d", $1.str, id_entry->offset, final_addr_reg);
    emitComment(CommentBuffer);

    // Offset stored for the variable in the Symbol Table
    int offset_reg = NextRegister();
    emit(NOLABEL, LOADI, id_entry->offset, offset_reg, EMPTY);

    // Calculate address from variable offset from base register
    emit(NOLABEL, ADD, 0, offset_reg, final_addr_reg);

    // Pass up register where variable address lives
    $$.targetRegister = final_addr_reg;
    // Pass up type of variable
    $$.type = id_entry->type;
  }

  |  ID '[' exp ']' {
    // Find ID in symbol table
    char * id_name = $1.str;
    SymTabEntry * id_entry = lookup(id_name);
    if(id_entry == NULL){
      // TODO : !!!!!!!!!!!!!!!!!!!!!
      // REPORT ERROR!
      printf("\n*** ERROR ***: Variable %s not declared.\n", id_name);
      return 1;
    }
    // TODO : IMPLEMENT
    if(id_entry->var_type != TYPE_ARRAY){
      printf("\n*** ERROR ***: Variable %s is not an array variable.\n", id_name);
    }
    // Evaluate Expersion?
    // TODO: ???????????????????
    // Make sure expression is a valid offset??????
    // make sure expression is int
    if($3.type != TYPE_INT){
      // TODO : !!!!!!!!!!!!!!!!!!!!!
      // REPORT ERROR!
      printf("\n*** ERROR ***: Array variable %s index type must be integer.\n", id_name);
    }

    // Register that holds expression value
    int exp_value_reg = $3.targetRegister;

    // Final address where value will be stored
    int final_addr_reg = NextRegister();
    sprintf(CommentBuffer, "Load %s[%d] Offset | %d + 1024 + (4 * exp) | Into v%d", $1.str, exp_value_reg, id_entry->offset, final_addr_reg);
    emitComment(CommentBuffer);

    // Store 4 * exp offset in offset_reg
    // vReg that holds 4
    int four_reg = NextRegister();
    // loadI 4 => vReg
    emit(NOLABEL, LOADI, 4, four_reg, EMPTY);

    // Register that holds offset based on index i.e. (4 * exp)
    int index_offset_reg = NextRegister();
    emit(NOLABEL, MULT, four_reg, exp_value_reg, index_offset_reg);
    // Load ID offset lookup into a vReg
    int id_offset_value_reg = NextRegister();
    emit(NOLABEL, LOADI, id_entry->offset, id_offset_value_reg, EMPTY);
    // Store index_offset_reg + id_offset_value in total_offset_reg
    // Perform index_offset + id_offset addition i.e. id[(4 * exp)]
    int total_offset_reg = NextRegister();
    emit(NOLABEL, ADD, index_offset_reg, id_offset_value_reg, total_offset_reg);
    // Add Total offset to base register,
    // to compute final address where the value will be stored
    emit(NOLABEL, ADD, 0, total_offset_reg, final_addr_reg);

    // Pass Up final address where value will be stored
    $$.targetRegister = final_addr_reg;
    // Pass up type of value
    $$.type = id_entry->type;
  }
  ;


exp	: exp '+' exp {
    // Register where evaluated value will live
    int newReg = NextRegister();
    // Must add ints
    if (! (($1.type == TYPE_INT) && ($3.type == TYPE_INT))) {
      printf("\n*** ERROR ***: Operand type must be integer.\n");
    }
    // add r1, r2 -> r3
    emit(NOLABEL, ADD, $1.targetRegister, $3.targetRegister, newReg);
    // Pass up type
    $$.type = $1.type;
    // Pass up register where value lives
    $$.targetRegister = newReg;
  }

  | exp '-' exp {
    // Register where evaluated value will live
    int newReg = NextRegister();
    // Must sub ints
    if (! (($1.type == TYPE_INT) && ($3.type == TYPE_INT))) {
      printf("\n*** ERROR ***: Operand type must be integer.\n");
    }
    // sub r1, r2 -> r3
    emit(NOLABEL, SUB, $1.targetRegister, $3.targetRegister, newReg);
    // Pass up type
    $$.type = $1.type;
    // Pass up register where value lives
    $$.targetRegister = newReg;
  }

  | exp '*' exp {
    // Register where evaluated value will live
    int newReg = NextRegister();
    // Must mult ints
    if (! (($1.type == TYPE_INT) && ($3.type == TYPE_INT))) {
      printf("\n*** ERROR ***: Operand type must be integer.\n");
    }
    // mult r1, r2 -> r3
    emit(NOLABEL, MULT, $1.targetRegister, $3.targetRegister, newReg);
    // Pass up type
    $$.type = $1.type;
    // Pass up register where value lives
    $$.targetRegister = newReg;
  }

  | exp AND exp {
    // Register where evaluated value will live
    int newReg = NextRegister();
    // Logical Operations must be done on booleans
    if (! (($1.type == TYPE_BOOL) && ($3.type == TYPE_BOOL))) {
      printf("\n*** ERROR ***: Operand type must be boolean.\n");
    }
    // and r1, r2 -> r3
    emit(NOLABEL, AND_INSTR, $1.targetRegister, $3.targetRegister, newReg);
    // Pass up type
    $$.type = $1.type;
    // Pass up register where value lives
    $$.targetRegister = newReg;
  }

  | exp OR exp {
    // Register where evaluated value will live
    int newReg = NextRegister();
    // Logical Operations must be done on booleans
    if (! (($1.type == TYPE_BOOL) && ($3.type == TYPE_BOOL))) {
      printf("\n*** ERROR ***: Operand type must be boolean.\n");
    }
    // or r1, r2 -> r3
    emit(NOLABEL, OR_INSTR, $1.targetRegister, $3.targetRegister, newReg);
    // Pass up type
    $$.type = $1.type;
    // Pass up register where value lives
    $$.targetRegister = newReg;
  }

  | ID {
    // Find ID in symbol table
    char * id_name = $1.str;
    SymTabEntry * id_entry = lookup(id_name);
    if(id_entry == NULL){
      // TODO : !!!!!!!!!!!!!!!!!!!!!
      // REPORT ERROR!
      printf("\n*** ERROR ***: Variable %s not declared.\n", id_entry);
      return 1;
    }
    /* // TODO : IMPLEMENT */
    if(id_entry->var_type != TYPE_SCALAR){
      printf("\n*** ERROR ***: Variable %s is not a scalar variable.\n", id_name);
    }
    // vReg to store value of ID
    int val_reg = NextRegister();

    sprintf(CommentBuffer, "Load %s into vReg %d", id_name, val_reg);
    emitComment(CommentBuffer);

    // Load value at offset into vReg
    // loadAI 0, offset => vReg
    emit(NOLABEL, LOADAI, 0, id_entry->offset, val_reg);

    // Pass up register where value lives
    $$.targetRegister = val_reg;
    // Pass up type
    $$.type = id_entry->type;
  }

  | ID '[' exp ']'	{
    // Find ID in symbol table
    char * id_name = $1.str;
    SymTabEntry * id_entry = lookup(id_name);
    if(id_entry == NULL){
      // TODO : !!!!!!!!!!!!!!!!!!!!!
      // REPORT ERROR!
      printf("\n*** ERROR ***: Variable %s not declared.\n", id_entry);
      return 1;
    }
    // TODO : IMPLEMENT
    if(id_entry->var_type != TYPE_ARRAY){
      printf("\n*** ERROR ***: Variable %s is not an array variable.\n", id_name);
    }
    // Evaluate Expersion?
    // TODO: ???????????????????
    // Make sure expression is a valid offset??????
    // make sure expression is int
    if($3.type != TYPE_INT){
      // TODO : !!!!!!!!!!!!!!!!!!!!!
      // REPORT ERROR!
      printf("\n*** ERROR ***: Array variable %s index type must be integer.\n", id_name);
    }

    // Register that holds expression value
    int exp_value_reg = $3.targetRegister;

    int final_value_reg = NextRegister();
    sprintf(CommentBuffer, "Load %s[%d] Value | %d + 1024 + (4 * exp) | Into v%d", $1.str, exp_value_reg, id_entry->offset, final_value_reg);
    emitComment(CommentBuffer);

    // Store 4 * exp offset in offset_reg
    // vReg that holds 4
    int four_reg = NextRegister();
    // loadI 4 => vReg
    emit(NOLABEL, LOADI, 4, four_reg, EMPTY);

    // Register that holds offset based on index i.e. (4 * exp)
    int index_offset_reg = NextRegister();
    emit(NOLABEL, MULT, four_reg, exp_value_reg, index_offset_reg);
    // Load ID offset lookup into a vReg
    int id_offset_value_reg = NextRegister();
    emit(NOLABEL, LOADI, id_entry->offset, id_offset_value_reg, EMPTY);
    // Store index_offset_reg + id_offset_value in total_offset_reg
    // Perform index_offset + id_offset addition i.e. id[(4 * exp)]
    int total_offset_reg = NextRegister();
    emit(NOLABEL, ADD, index_offset_reg, id_offset_value_reg, total_offset_reg);
    // Add Total offset to base register,
    // to compute final address where the value will be stored
    int final_addr_reg = NextRegister();
    emit(NOLABEL, ADD, 0, total_offset_reg, final_addr_reg);

    // Load Value at computed address into a register
    emit(NOLABEL, LOAD, final_addr_reg, final_value_reg, EMPTY);
    // Pass Up final register where value
    // at the computed addresswill be stored
    $$.targetRegister = final_value_reg;
    // Pass up type of value
    $$.type = id_entry->type;

  }

  | ICONST  {
    int newReg = NextRegister();
    // Pass up register where value lives
    $$.targetRegister = newReg;
    // Pass up type
	  $$.type = TYPE_INT;
    sprintf(CommentBuffer, "Load %d | Into v%d", $1.num, newReg);
    emitComment(CommentBuffer);
	  emit(NOLABEL, LOADI, $1.num, newReg, EMPTY);
  }

  | TRUE {
    int newReg = NextRegister(); /* TRUE is encoded as value '1' */
    // Pass up register where value lives
    $$.targetRegister = newReg;
    // Pass up type
    $$.type = TYPE_BOOL;
    sprintf(CommentBuffer, "Load %d \"TRUE\" | Into v%d", 1, newReg);
    emitComment(CommentBuffer);
    emit(NOLABEL, LOADI, 1, newReg, EMPTY);
  }

  | FALSE  {
    int newReg = NextRegister(); /* FALSE is encoded as value '0' */
    // Pass up register where value lives
    $$.targetRegister = newReg;
    // Pass up type
    $$.type = TYPE_BOOL;
    sprintf(CommentBuffer, "Load %d \"FALSE\" | Into v%d", 0, newReg);
    emitComment(CommentBuffer);
    emit(NOLABEL, LOADI, 0, newReg, EMPTY);
  }

  | error {
    yyerror("***Error: illegal expression\n");
  }
	;

condexp	: exp NEQ exp {
    if (! ((($1.type == TYPE_INT) && ($3.type == TYPE_INT)) ||
        (($1.type == TYPE_BOOL) && ($3.type == TYPE_BOOL)))) {
        printf("\n*** ERROR ***: == or != operator with different types.\n");
    }
    // Register to store comparison result in
    int resultReg = NextRegister();

    sprintf(CommentBuffer, "Store v%d != v%d | Into v%d", $1.targetRegister, $3.targetRegister, resultReg);
    emitComment(CommentBuffer);

    // cmpne r1, r2 -> r3
    emit(NOLABEL, CMPNE, $1.targetRegister, $3.targetRegister, resultReg);
    // Pass up register where value lives
    $$.targetRegister = resultReg;
  }

  | exp EQ exp {
    if (! ((($1.type == TYPE_INT) && ($3.type == TYPE_INT)) ||
        (($1.type == TYPE_BOOL) && ($3.type == TYPE_BOOL)))) {
        printf("\n*** ERROR ***: == or != operator with different types.\n");
    }
    // Register to store comparison result in
    int resultReg = NextRegister();

    sprintf(CommentBuffer, "Store v%d == v%d | Into v%d", $1.targetRegister, $3.targetRegister, resultReg);
    emitComment(CommentBuffer);

    // cmpeq r1, r2 -> r3
    emit(NOLABEL, CMPEQ, $1.targetRegister, $3.targetRegister, resultReg);
    // Pass up register where value lives
    $$.targetRegister = resultReg;
  }

  | exp LT exp {
    // Must perform these ops on ints
    if (! ((($1.type == TYPE_INT) && ($3.type == TYPE_INT))) ){
        // TODO : IMPLEMENT !!!!!!!
        printf("\n*** ERROR ***: Relational operator with illegal type.\n");
    }
    // Register to store comparison result in
    int resultReg = NextRegister();

    sprintf(CommentBuffer, "Store v%d < v%d | Into v%d", $1.targetRegister, $3.targetRegister, resultReg);
    emitComment(CommentBuffer);

    // cmplt r1, r2 -> r3
    emit(NOLABEL, CMPLT, $1.targetRegister, $3.targetRegister, resultReg);
    // Pass up register where value lives
    $$.targetRegister = resultReg;
  }

  | exp LEQ exp	{
    // Must perform these ops on ints
    if (! ((($1.type == TYPE_INT) && ($3.type == TYPE_INT))) ){
        // TODO : IMPLEMENT !!!!!!!
        printf("\n*** ERROR ***: Relational operator with illegal type.\n");
    }
    // Register to store comparison result in
    int resultReg = NextRegister();

    sprintf(CommentBuffer, "Store v%d <= v%d | Into v%d", $1.targetRegister, $3.targetRegister, resultReg);
    emitComment(CommentBuffer);

    // cmple r1, r2 -> r3
    emit(NOLABEL, CMPLE, $1.targetRegister, $3.targetRegister, resultReg);
    // Pass up register where value lives
    $$.targetRegister = resultReg;
  }

  | exp GT exp {
    // Must perform these ops on ints
    if (! ((($1.type == TYPE_INT) && ($3.type == TYPE_INT))) ){
        // TODO : IMPLEMENT !!!!!!!
        printf("\n*** ERROR ***: Relational operator with illegal type.\n");
    }
    // Register to store comparison result in
    int resultReg = NextRegister();

    sprintf(CommentBuffer, "Store v%d > v%d | Into v%d", $1.targetRegister, $3.targetRegister, resultReg);
    emitComment(CommentBuffer);

    // cmpgt r1, r2 -> r3
    emit(NOLABEL, CMPGT, $1.targetRegister, $3.targetRegister, resultReg);
    // Pass up register where value lives
    $$.targetRegister = resultReg;
  }

  | exp GEQ exp	{
    // Must perform these ops on ints
    if (! ((($1.type == TYPE_INT) && ($3.type == TYPE_INT))) ){
        // TODO : IMPLEMENT !!!!!!!
        printf("\n*** ERROR ***: Relational operator with illegal type.\n");
    }
    // Register to store comparison result in
    int resultReg = NextRegister();
    sprintf(CommentBuffer, "Store v%d >= v%d | Into v%d", $1.targetRegister, $3.targetRegister, resultReg);
    emitComment(CommentBuffer);

    // cmpge r1, r2 -> r3
    emit(NOLABEL, CMPGE, $1.targetRegister, $3.targetRegister, resultReg);
    // Pass up register where value lives
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
