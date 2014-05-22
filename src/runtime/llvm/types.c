#include <stdlib.h>

#include "types.h"

struct value_t* new_value( int val )
{
  struct value_t* new_val = calloc( 1, sizeof( struct value_t ) );
  new_val->type = TYPE_INT;
  new_val->value = val;

  return new_val;
}
