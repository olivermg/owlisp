#include <stdlib.h>
#include <string.h>

#include "binding.h"
#include "value.h"

binding_t* new_binding( char* name, value_t* value )
{
  binding_t* newbinding = malloc( sizeof( binding_t ) );

  size_t namelen = strlen( name ) + 1;
  newbinding->name = malloc( namelen );
  strncpy( newbinding->name, name, namelen );

  newbinding->value = value;

  return newbinding;
}

void free_binding( binding_t* binding )
{
  free( binding->name );
  free_value( binding->value );
  free( binding );
}
