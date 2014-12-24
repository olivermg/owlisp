#include <stdlib.h>
#include <stdio.h>
#include <owlisp/owlisprt.h>


/*
 * environment handling:
 */

static Env* gotoframe(Env* env, Envaddress* envaddress)
{
  Env* newenv = env;

  if ( NULL == env ) {
    printf("ERROR: can't find frame!\n"); // TODO: throw exception or such
  } else if ( envaddress->frameindex > 0 ) {
    Envaddress* newea = (Envaddress*)newenvaddress( envaddress->frameindex - 1, envaddress->varindex );
    newenv = gotoframe( env->parent, newea ); // FIXME: recursion may lead to stackoverflow, once we have cps in place
  }

  return newenv;
}

Object* lookup(Env *env, Envaddress* envaddress)
{
  Object* value = NULL;

  Env* actualframe = gotoframe( env, envaddress );
  if ( NULL != actualframe && envaddress->varindex < actualframe->numobjects ) {
    value = actualframe->objects[envaddress->varindex];
  } else {
    // TODO: throw exception or such:
    printf("ERROR: invalid variable reference!\n");
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
  Env* actualframe = gotoframe( env, envaddress );
  if ( NULL != actualframe && envaddress->varindex < actualframe->numobjects ) {
    actualframe->objects[envaddress->varindex] = value;
  } else {
    // TODO: throw exception or such:
    printf("ERROR: invalid variable reference!\n");
  }
}

void set_i(Env* env, unsigned long frameindex, unsigned long varindex, Object* value)
{
  Envaddress* ea = (Envaddress*)newenvaddress( frameindex, varindex );
  set(env, ea, value);
}
