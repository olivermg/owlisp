#ifndef __CLASSES_H
#define __CLASSES_H


typedef struct _Class Class;
typedef struct _Object Object;
typedef struct _Integer Integer;
typedef struct _String String;
typedef struct _Proc Proc;
typedef struct _Env Env;
typedef struct _Envaddress Envaddress;
typedef struct _Closure Closure;
typedef struct _List List;


struct _Class {
  // NOTE: this struct must not be empty, since some compilers may map the below instances
  //       of this struct (CObject, CInteger, ...) to the very same memory address.
  Class* superclass;
};

struct _Object {
  Class* class;
};

struct _Integer {
  // NOTE: the fact that the first element is of type Object allows us to safely cast
  //       (Integer*) to (Object*) and vice versa, because the C specification says:
  //       "If a struct starts with an int, the struct* may also be cast to an int*,
  //       allowing to write int values into the first field."
  Object object;
  int value;
};

struct _String {
  Object object;
  char* value;
};

typedef Object* (*proc_p)(Env* env); // TODO: what is the best representation for args? va_arg, List*, env?
struct _Proc {
  Object object;
  proc_p value;
};

struct _Env {
  Object object;
  Object* o1;
  Object* o2;
  Env* parent;
};

struct _Envaddress {
  Object object;
  unsigned int frameindex;
  unsigned int varindex;
};

struct _Closure {
  Object object;
  Env* env;
  Proc* proc;
};

struct _List {
  Object object;
  Object* value;
  List* next;
};


/*
 * constructors:
 */

Object* newint(int value);
Object* newstring(char* value);
Object* newproc(proc_p value);
Object* newenv(Object* o1, Object* o2, Env* parent);
Object* newenvaddress(unsigned int frameindex, unsigned int varindex);
Object* newclosure(Env* env, Proc* proc);
Object* newclosure_i(Env* env, proc_p p);
Object* newlist(Object* value, List* next);


/*
 * class instances:
 */

Class CObject;
Class CInteger;
Class CString;
Class CProc;
Class CEnv;
Class CEnvaddress;
Class CClosure;
Class CList;

#endif
