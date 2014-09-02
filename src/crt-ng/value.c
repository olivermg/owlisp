#include <stdlib.h>

#include "value.h"
#include "type.h"

static value_t* new_value( type_t type, void* primitive_value, int valuelength )
{
  value_t* newvalue = malloc( sizeof( value_t ) );

  newvalue->type = type;
  newvalue->value = malloc( valuelength );
  memcpy( newvalue->value, primitive_value, valuelength );

  return newvalue;
}

value_t* new_value_integer( int intvalue )
{
  return new_value( TYPE_INTEGER, &intvalue, sizeof( intvalue ) );
}

void free_value( value_t* value )
{
  free( value->value );
  free( value );
}
