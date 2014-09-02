#ifndef __BINDING_H
#define __BINDING_H 1

typedef struct _binding_t {
  char* name;
  int value;
} binding_t;

binding_t* new_binding( char* name, int value );
void update_binding( binding_t* binding, int value );
void free_binding( binding_t* binding );

#endif
