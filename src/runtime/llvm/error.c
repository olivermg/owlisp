#include <stdio.h>
#include <stdlib.h>

#include "error.h"

void fatal_error( const char* message )
{
  fprintf( stderr, "ERROR: %s\n", message );

  exit( 1 );
}
