#ifndef __VALUE_H
#define __VALUE_H 1

#include "type.h"

typedef struct _value_t {
  type_t type;
  void* value;
} value_t;

value_t* new_value_integer( int intvalue );
void free_value( value_t* value );

#endif
