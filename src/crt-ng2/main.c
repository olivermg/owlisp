#include <stdio.h>
#include <owlisp/owlisprt.h>

Object* _U_MAIN( Env* ); // NOTE: this procedure is assumed to be defined in user code

int main( int argc, char* argv[] )
{
  Object* mainproc = newproc( &_U_MAIN );
  Object* result = invoke_obj( mainproc, 0 );
  printf( "result of _U_MAIN:\n" );
  print_obj( result );

  return 0;
}
