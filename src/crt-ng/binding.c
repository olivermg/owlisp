#include <stdlib.h>
#include <string.h>

#include "binding.h"

binding_t* new_binding( char* name, int value )
{
  binding_t* newbinding = malloc( sizeof( binding_t ) );

  size_t namelen = strlen( name ) + 1;
  newbinding->name = malloc( namelen );
  strncpy( newbinding->name, name, namelen );

  newbinding->value = value;

  return newbinding;
}

void update_binding( binding_t* binding, int value )
{
  binding->value = value;
}

void free_binding( binding_t* binding )
{
  free( binding->name );
  free( binding );
}
