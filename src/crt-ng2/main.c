#include <stdio.h>
#include <owlisp/owlisprt.h>

Object* _U_MAIN( Env* ); // NOTE: this procedure is assumed to be defined in user code

static Object* identity( Env* env )
{
  return lookup_i( env, 0, 0 );
}

int main( int argc, char* argv[] )
{
  Object* mainproc = newproc( &_U_MAIN );
  Object* cpsfn = invoke_obj( mainproc, 0 );

  Object* identityproc = newproc( &identity );
  Object* result = invoke_obj( cpsfn, 1, identityproc ); // TODO: do this (applying final kexpr) in lisp

  printf( "result of _U_MAIN:\n" );
  print_obj( result );

  return 0;
}
