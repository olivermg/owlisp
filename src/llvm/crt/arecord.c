#include <stdlib.h>

#include "arecord.h"

arecord_t* new_arecord()
{
  arecord_t* newar = calloc( 1, sizeof( arecord_t ) );

  return newar;
}

void free_arecord( arecord_t* ar )
{
  free( ar );
}

void set_static_link( arecord_t* ar, void* static_link )
{
  ar->static_link = static_link;
}

void* get_static_link( arecord_t* ar )
{
  return ar->static_link;
}

void set_dynamic_link( arecord_t* ar, void* dynamic_link )
{
  ar->dynamic_link = dynamic_link;
}

void* get_dynamic_link( arecord_t* ar )
{
  return ar->dynamic_link;
}

void set_local_variables( arecord_t* ar, frame_t* local_variables )
{
  ar->local_variables = local_variables;
}

frame_t* get_local_variables( arecord_t* ar )
{
  return ar->local_variables;
}
