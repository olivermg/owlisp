#include <stdlib.h>
#include <stdio.h>
#include <owlisp/env.h>


/*
 * environment handling:
 */

static Env* gotoframe(Env* env, Envaddress* envaddress)
{
  Env* newenv = env;

  if ( NULL == env ) {
    printf("ERROR: can't find frame!\n");
  } else if ( envaddress->frameindex > 0 ) {
    Envaddress* newea = (Envaddress*)newenvaddress( envaddress->frameindex - 1, envaddress->varindex );
    newenv = gotoframe( env->parent, newea ); // FIXME: recursion may lead to stackoverflow, once we have cps in place
  }

  return newenv;
}

Object* lookup(Env *env, Envaddress* envaddress)
{
  Object* value = NULL;

  if ( envaddress->varindex == 0 ) {
    value = gotoframe( env, envaddress )->o1;
  } else {
    value = gotoframe( env, envaddress )->o2;
  }

  return value;
}

Object* lookup_i(Env* env, unsigned long frameindex, unsigned long varindex)
{
  Envaddress* ea = (Envaddress*)newenvaddress( frameindex, varindex );
  return lookup(env, ea);
}

void set(Env* env, Envaddress* envaddress, Object* value)
{
  if ( envaddress->varindex == 0 ) {
    gotoframe( env, envaddress )->o1 = value;
  } else {
    gotoframe( env, envaddress )->o2 = value;
  }
}

void set_i(Env* env, unsigned long frameindex, unsigned long varindex, Object* value)
{
  Envaddress* ea = (Envaddress*)newenvaddress( frameindex, varindex );
  set(env, ea, value);
}
