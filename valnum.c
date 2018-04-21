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
Hashtable * Expression_Hashtable;

void
InitExpressionTable() {
  if(!(Expression_Hashtable == NULL)){
    return;
  }
  // printf("INIT Expression Table\n");
  Expression_Hashtable = create_Hashtable(5);
}

// START LL TYPE DEFINITION/FUNCTIONS

// Create a new Linked List
LL * create_LL(){
  // Allocate space for LL
  // - return NULL if error
  LL * newLL = (LL*) malloc(sizeof(LL));
  // - Check for unsuccessful LL malloc
  if(newLL == NULL) {
    perror("Error allocating memory for new LL in create_LL().");
    return NULL;
  }
  // Build Struct
  *newLL = (LL) {
    .head = NULL,
    .length = 0
  };

  // Return ptr
  return newLL;
}

// TODO : ADD NULL POINTER CHECK
// Free a Linked List
void free_LL(LL * list){
  // Determine if any nodes need to be freed
  if(list->length < 1){
    // Free all nodes
    while(list->head != NULL){
      LLNode * ptr = list->head->next;
      free(list->head);
      list->head = ptr;
    }
  }
  // Free Struct
  free(list);
}

// TODO : ADD NULL POINTER CHECK
// Push a new node containing data into List
// New Nodes are prepended to the list
// Returns Length of list after addition
int push(LL * list, void* data){
  // Allocate Space for node
  // - Error if malloc fails
  LLNode * newNode = (LLNode*) malloc(sizeof(LLNode));
  // - Check for unsuccessful LLNode malloc
  if(newNode == NULL) {
    perror("Error allocating memory for new LLNode in push().");
    return -1;
  }

  // Build Node
  *newNode = (LLNode) {
    .data = data,
    .next = list->head
  };

  // prepend node & update head
  list->head = newNode;

  // Update size
  list->length += 1;

  return list->length;
}
// END LL TYPE DEFINITION/FUNCTIONS


HashtableItem * create_HashtableItem(char * key, int data){
  // Allocate space for a Item
  // - return NULL if error
  HashtableItem * newItem = (HashtableItem*) malloc(sizeof(HashtableItem));
  // - Check for unsuccessful table malloc
  if(newItem == NULL) {
    perror("Error allocating memory for new buckets in create_Hashtable().");
    return NULL;
  }
  // Build Struct
  *newItem = (HashtableItem) {
    .key = key,
    .data = data
  };
}

// Create a new Hashtable
Hashtable * create_Hashtable(int size){

  // Allocate space for table ("buckets") Hashtable
  // - return NULL if error
  LL ** newBuckets = (LL**) malloc(sizeof(LL *) * size);
  // - Check for unsuccessful table malloc
  if(newBuckets == NULL) {
    perror("Error allocating memory for new buckets in create_Hashtable().");
    return NULL;
  }

  // Allocate space for a Hashtable
  // - return NULL if error
  Hashtable * newHashtable = (Hashtable*) malloc(sizeof(Hashtable));
  // - Check for unsuccessful Hashtable malloc
  if(newHashtable == NULL) {
    free(newBuckets);
    perror("Error allocating memory for new LL in create_LL().");
    return NULL;
  }

  // Nullify buckets
  int i = 0;
  for (i = 0; i < size; i++) {
    newBuckets[i] = NULL;
  }

  // Build Struct
  *newHashtable = (Hashtable) {
    .table = newBuckets,
    .size = size
  };
}

// TODO : ADD NULL POINTER CHECK
// Free a Hashtable
void free_Hashtable(Hashtable * target){
  // Determine if any buckets need to be freed
  int i = 0;
  for (i = 0; i < target->size; i++) {
    if (target->table[i] != NULL){
      // Free each item in table bucket
      LLNode * ptr = target->table[i]->head;
      while(ptr != NULL){
        free(ptr->data);
        ptr = ptr->next;
      }
      // Free Table
      free_LL(target->table[i]);
    }
  }
  // Free Buckets
  free(target->table);
  // Free Struct
  free(target);
}

// TODO : Implement Something for Complex like djb2 or Adler32
// Very primitive hash function
int hash(char * key){
  if (key == NULL) {
    perror("Error hasing a NULL key in hash().");
    return -1;
  }
  int value = 1;
  int i = 0;
  for (i = 0; i < strlen(key); i++) {
    value = (value * key[i]) % Expression_Hashtable->size;
  }
  return value;
}

// Insert data with a given key into a Hashtable
int insert_expression(char * key, int data){
  if (key == NULL) {
    perror("Error inserting NULL key in insert_expression().");
    return -1;
  }

  // Get hash_value
  int hash_value = hash(key);
  if (hash_value < 0) {
    return -1;
  }

  // Get bucket to place data in
  LL * bucket = Expression_Hashtable->table[hash_value];
  // Create List if bucket doesn't have one yet
  if (bucket == NULL) {
    bucket = create_LL();
    Expression_Hashtable->table[hash_value] = bucket;
  }

  HashtableItem * newItem = create_HashtableItem(key, data);

  // Append data to bucket
  push(bucket, newItem);
  return 1;
}

// Remove data with a given key into a Hashtable
int remove_key(char * target_key){
  if (target_key == NULL) {
    perror("Error inserting NULL key in remove_key().");
    return -1;
  }

  // Get hash_value
  int hash_value = hash(target_key);
  if (hash_value < 0) {
    return -1;
  }

  // Get bucket to place data in
  LL * bucket = Expression_Hashtable->table[hash_value];
  // Create List if bucket doesn't have one yet
  if (bucket == NULL) {
    perror("Key mapped to empty bucket in remove_key().");
    return -1;
  }

  // Look at each item in table bucket
  LL * list = Expression_Hashtable->table[hash_value];


  if(list->head == NULL){
    return -1;
  }
  // If head is target, update it, free, and return
  if((strcmp(((HashtableItem *) list->head->data)->key, target_key) == 0)){
    LLNode * ptr = list->head->next;
    free(((HashtableItem *) list->head->data)->key);
    free((HashtableItem *) list->head->data);
    free(list->head);
    list->head = ptr;
    // Update Length
    list->length -= 1;
    // NOTE : Remove if REMOVING ALL OCCURENCES
    return -1;
  }
  // current and prev ptrs .... The Flashbacks to 112....oh god
  LLNode * ptr = list->head->next;
  LLNode * prev = list->head;
  while(ptr != NULL){
    // printf("%s or %s\n", ((HashtableItem *) ptr->data)->key, target_key);
    if((strcmp(((HashtableItem *) ptr->data)->key, target_key) == 0)){
      // By Pass ptr, which is currently prev->next
      prev->next = prev->next->next;
      free(((HashtableItem *) ptr->data)->key);
      free((HashtableItem *) ptr->data);
      free(ptr);
      // Update Length
      list->length -= 1;
      // NOTE : Remove if REMOVING ALL OCCURENCES
      return 1;
    }
    // Update ptrs
    prev = ptr;
    ptr = ptr->next;
  }
  // Couldn't Find Target
  return -1;
}

int lookup_expression(char * target_key){
  if (target_key == NULL) {
    perror("Error looking up NULL key in lookup_expression().");
    return -1;
  }

  // Get hash_value
  int hash_value = hash(target_key);
  if (hash_value < 0) {
    return -1;
  }

  if(Expression_Hashtable->table[hash_value] == NULL){
    return -1;
  }

  // Look at each item in table bucket
  LLNode * ptr = Expression_Hashtable->table[hash_value]->head;
  while(ptr != NULL){
    if (strcmp(((HashtableItem *) ptr->data)->key, target_key) == 0) {
      return ((HashtableItem *) ptr->data)->data;
    }
    ptr = ptr->next;
  }
  return -1;
}

void print_Expression_Hashtable(){
  if(Expression_Hashtable == NULL){
    return ;
  }

  printf("---------Hashtable---------\n");

  int b = 0;
  for (b = 0; b < Expression_Hashtable->size; b++) {
    printf("[%d] -> ", b);

    if (Expression_Hashtable->table[b] == NULL) {
      printf("/\n");
      continue;
    }

    LLNode * ptr = Expression_Hashtable->table[b]->head;
    while(ptr != NULL){
      HashtableItem * item = (HashtableItem *) ptr->data;
      printf("|Key: %s, Data: %d| -> ", item->key, item->data);
      ptr = ptr->next;
    }

    printf("/\n");

  }

  printf("---------End Hashtable---------\n");
}
