/**********************************************
        CS415  Project 2
        Spring  2018
        Student Version
**********************************************/

#include "attr.h"

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

void * pop(LL * list){
  if (list->length == 0) {
    return NULL;
  }

  void * retval = list->head->data;
  list->head = list->head->next;
  list->length -= 1;

  return retval;
}

// TODO : ADD NULL POINTER CHECK
// TODO : REMOVE ALL?
// Remove a new node containing the target data from the List
// Returns Length of list after removal
int remove_target(LL * list, void* target_data){
  if(list->head == NULL){
    return list->length;
  }
  // If head is target, update it, free, and return
  if(list->head->data == target_data){
    LLNode * ptr = list->head->next;
    free(list->head);
    list->head = ptr;
    // Update Length
    list->length -= 1;
    // NOTE : Remove if REMOVING ALL OCCURENCES
    return list->length;
  }
  // current and prev ptrs .... The Flashbacks to 112....oh god
  LLNode * ptr = list->head->next;
  LLNode * prev = list->head;
  while(ptr != NULL){
    if(ptr->data == target_data){
      // By Pass ptr, which is currently prev->next
      prev->next = prev->next->next;
      free(ptr);
      // Update Length
      list->length -= 1;
      // NOTE : Remove if REMOVING ALL OCCURENCES
      return list->length;
    }
    // Update ptrs
    prev = ptr;
    ptr = ptr->next;
  }
  // Couldn't Find Target
  return list->length;
}
// END LL TYPE DEFINITION/FUNCTIONS
