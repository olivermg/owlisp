#ifndef __BINDING_H
#define __BINDING_H 1

#include "value.h"

typedef struct _binding_t {
  char* name;
  value_t* value;
} binding_t;

binding_t* new_binding( char* name, value_t* value );
void free_binding( binding_t* binding );

#endif
