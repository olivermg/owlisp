#include <owlisp/owlisprt.h>

Object* U_MAIN( Env* ); // NOTE: this procedure is assumed to be defined in user code

int main( int argc, char* argv[] )
{
  Object* mainproc = newproc( &U_MAIN );
  invoke_obj( mainproc, 0 );

  return 0;
}
