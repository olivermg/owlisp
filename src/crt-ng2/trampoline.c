#include <owlisp/owlisprt.h>

Object* run_trampoline( trampoline_return_t tr )
{
  while ( CALL == tr.action) {
    tr = tr.call.fn( tr.call.env );
  };

  return tr.retval;
}
