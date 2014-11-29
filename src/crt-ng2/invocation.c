#include <stdarg.h>
#include <stdlib.h>
#include <stdio.h>
#include "invocation.h"

Object* invoke_obj(Object* o, unsigned long numargs, ...)
{
  // TODO: type safety

  va_list args;

  // TODO: handle variable number of arguments:
  va_start(args, numargs);
  Object* arg1 = va_arg(args, Object*);
  Object* arg2 = va_arg(args, Object*);
  va_end(args);

  Proc* proc = NULL;
  Env* env = NULL;
  if ( o->class == &CProc ) {
    // invoke Proc:
    printf("...invoking proc...\n");
    proc = (Proc*)o;
    env = (Env*)newenv(arg1, arg2, NULL);
  } else if ( o->class == &CClosure ) {
    // invoke Closure:
    printf("...invoking closure...\n");
    proc = ((Closure*)o)->proc;
    env = (Env*)newenv(arg1, arg2, ((Closure*)o)->env);
  }

  return proc->value(env);
}
