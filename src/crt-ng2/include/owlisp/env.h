#ifndef __ENV_H
#define __ENV_H

#include <owlisp/classes.h>

Object* lookup(Env *env, Envaddress* envaddress);
Object* lookup_i(Env* env, unsigned long frameindex, unsigned long varindex);
void set(Env* env, Envaddress* envaddress, Object* value);
void set_i(Env* env, unsigned long frameindex, unsigned long varindex, Object* value);

#endif
