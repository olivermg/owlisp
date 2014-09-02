#include <stdlib.h>
#include <string.h>

#include "procedure.h"
#include "environment.h"

procedure_t* new_procedure( char* parameters[], int parameterscount, void (*code)(), environment_t* environment )
{
  procedure_t* newprocedure = malloc( sizeof( procedure_t ) );

  newprocedure->parameters = malloc( sizeof( char* ) * parameterscount );
  for ( int i = 0; i < parameterscount; i++ ) {
    size_t paramlen = strlen( parameters[i] ) + 1;
    newprocedure->parameters[i] = malloc( paramlen );
    strncpy( newprocedure->parameters[i], parameters[i], paramlen );
  }

  newprocedure->parameterscount = parameterscount;
  newprocedure->code = code;
  newprocedure->environment = environment;

  return newprocedure;
}

void invoke_procedure( procedure_t* procedure )
{
  procedure->code();
}

void free_procedure( procedure_t* procedure )
{
  for ( int i = 0; i < procedure->parameterscount; i++ ) {
    free( procedure->parameters[i] );
  }
  free( procedure->parameters );
  free( procedure );
}
