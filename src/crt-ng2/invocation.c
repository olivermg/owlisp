#include <stdarg.h>
#include <stdlib.h>
#include <stdio.h>
#include <owlisp/owlisprt.h>

Object* invoke_obj(Object* o, unsigned long numargs, ...)
{
  // TODO: type safety

  va_list args;
  va_start(args, numargs);

  Proc* proc = NULL;
  Env* env = NULL;
  if ( o->class == &CProc ) {
    // invoke Proc:
    printf("...invoking proc...\n");
    proc = (Proc*)o;
    env = (Env*)newenv_v(NULL, numargs, args);
  } else if ( o->class == &CClosure ) {
    // invoke Closure:
    printf("...invoking closure...\n");
    proc = ((Closure*)o)->proc;
    env = (Env*)newenv_v(((Closure*)o)->env, numargs, args);
  }

  va_end(args);

  trampoline_return_t tr = proc->value(env); // TODO: optimize handling
  return tr.retval;
}
