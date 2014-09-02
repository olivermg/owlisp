#include <stdlib.h>

#include "environment.h"
#include "binding.h"

environment_t* new_environment( environment_t* parent, binding_t bindings[], int bindingscount )
{
  environment_t* newenv = malloc( sizeof( environment_t ) );

  newenv->parent = parent;
  newenv->bindings = bindings;
  newenv->bindingscount = bindingscount;

  return newenv;
}

void free_environment( environment_t* environment )
{
  for ( int i = 0; i < environment->bindingscount; i++ ) {
    free_binding( environment->bindings[i] );
  }
  free( environment );
}

void free_all_environments( environment_t* environment )
{
  do {
    environment_t* parent = environment->parent;
    free_environment( environment );
    environment = parent;
  } while ( environment );
}
