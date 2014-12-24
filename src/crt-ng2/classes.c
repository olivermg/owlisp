#include <stdarg.h>
#include <stdlib.h>
#include <owlisp/owlisprt.h>


/*
 * class instances:
 */

Class CObject = { NULL };
Class CInteger = { &CObject };
Class CString = { &CObject };
Class CProc = { &CObject };
Class CEnv = { &CObject };
Class CEnvaddress = { &CObject };
Class CClosure = { &CObject };
Class CList = { &CObject };


/*
 * constructors:
 */

Object* newint(int value)
{
  Integer* o = malloc( sizeof(Integer) );
  o->object.class = &CInteger;
  o->value = value;

  return (Object*)o;
}

Object* newstring(char* value)
{
  String* o = malloc( sizeof(String) );
  o->object.class = &CString;
  o->value = value;

  return (Object*)o;
}

Object* newproc(proc_p value)
{
  Proc* o = malloc( sizeof(Proc) );
  o->object.class = &CProc;
  o->value = value;

  return (Object*)o;
}

static Object* newenv_alloc(Env* parent, unsigned long numobjects)
{
  Env* o = malloc( sizeof(Env) );
  o->object.class = &CEnv;
  o->parent = parent;
  o->numobjects = numobjects;
  o->objects = calloc( numobjects, sizeof(Object*) );

  return (Object*)o;
}

Object* newenv_v(Env* parent, unsigned long numobjects, va_list objects)
{
  Env* o = (Env*)newenv_alloc(parent, numobjects);

  unsigned long i;
  for ( i = 0; i < numobjects; i++ ) {
    Object* obj = va_arg(objects, Object*);
    o->objects[i] = obj;
  }

  return (Object*)o;
}

Object* newenv(Env* parent, unsigned long numobjects, ...)
{
  va_list objects;

  va_start(objects, numobjects);
  Object* env = newenv_v(parent, numobjects, objects);
  va_end(objects);

  return env;
}

Object* newenv_e(Env* parent, unsigned long numobjects)
{
  return newenv_alloc(parent, numobjects);
}

Object* newenvaddress(unsigned int frameindex, unsigned int varindex)
{
  Envaddress* o = malloc( sizeof(Envaddress) );
  o->object.class = &CEnvaddress;
  o->frameindex = frameindex;
  o->varindex = varindex;

  return (Object*)o;
}

Object* newclosure(Env* env, Proc* proc)
{
  Closure* o = malloc( sizeof(Closure) );
  o->object.class = &CClosure;
  o->env = env;
  o->proc = proc;

  return (Object*)o;
}

Object* newclosure_i(Env* env, proc_p p)
{
  Proc* proc = (Proc*)newproc(p);
  return newclosure(env, proc);
}

Object* newlist(Object* value, List* next)
{
  List* o = malloc( sizeof(List) );
  o->object.class = &CList;
  o->value = value;
  o->next = next;

  return (Object*)o;
}
