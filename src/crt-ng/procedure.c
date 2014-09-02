#include <stdlib.h>

#include "procedure.h"
#include "gc.h"

procedure_t* new_procedure( char* parameters[], void (code*)(), frame_t* environment )
{
  procedure_t* newprocedure = gc_calloc( 1, sizeof( procedure_t ) );

  return newprocedure;
}
