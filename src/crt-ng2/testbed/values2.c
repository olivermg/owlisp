#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>


typedef struct _Class Class;
typedef struct _Object Object;
typedef struct _Integer Integer;
typedef struct _String String;
typedef struct _Proc Proc;
typedef struct _Env Env;
typedef struct _Closure Closure;
typedef struct _List List;


struct _Class {
  //  Class* superclass; // TODO: implement inheritance
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


/* class instances: */
Class CObject = {};
Class CInteger = {};
Class CString = {};
Class CProc = {};
Class CEnv = {};
Class CClosure = {};
Class CList = {};


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

Object* newenv(Object* o1, Object* o2, Env* parent)
{
  Env* o = malloc( sizeof(Env) );
  o->object.class = &CEnv;
  o->o1 = o1;
  o->o2 = o2;
  o->parent = parent;

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

Object* newlist(Object* value, List* next)
{
  List* o = malloc( sizeof(List) );
  o->object.class = &CList;
  o->value = value;
  o->next = next;

  return (Object*)o;
}


Object* add_obj(Object* o1, Object* o2)
{
  // TODO: type safety

  Integer* i1 = (Integer*)o1;
  Integer* i2 = (Integer*)o2;

  return newint( i1->value + i2->value );
}

Object* invoke_obj(Object* o, unsigned long numargs, ...)
{
  // TODO: type safety

  va_list args;

  va_start(args, numargs);
  Object* arg1 = va_arg(args, Object*);
  Object* arg2 = va_arg(args, Object*);
  va_end(args);

  Proc* proc = NULL;
  Env* env = NULL;
  if ( o->class == &CProc ) {
    printf("...invoking proc...\n");
    proc = (Proc*)o;
    env = (Env*)newenv(arg1, arg2, NULL);
  } else if ( o->class == &CClosure ) {
    printf("...invoking closure...\n");
    proc = ((Closure*)o)->proc;
    env = (Env*)newenv(arg1, arg2, ((Closure*)o)->env);
  }

  return proc->value(env);
}

Object* car_obj(Object* list)
{
  // TODO: type safety

  return ((List*)list)->value;
}

Object* cdr_obj(Object* list)
{
  // TODO: type safety

  return (Object*)((List*)list)->next;
}

void print_obj(Object* o)
{
  printf("%p => ", o);
  if ( NULL == o ) {
    printf("NULL\n");
  } else if ( o->class == &CInteger ) {
    printf("integer: %d\n", ((Integer*)o)->value);
  } else if ( o->class == &CString ) {
    printf("string: %s\n", ((String*)o)->value);
  } else if ( o->class == &CProc) {
    printf("proc: %p\n", ((Proc*)o)->value);
  } else if ( o->class == &CEnv) {
    printf("env:\n\t");
    print_obj( ((Env*)o)->o1 );
    printf("\t");
    print_obj( ((Env*)o)->o2 );
    print_obj( (Object*)((Env*)o)->parent );
  } else if ( o->class == &CClosure ) {
    printf("closure:\n\t");
    print_obj( (Object*)((Closure*)o)->env );
    printf("\t");
    print_obj( (Object*)((Closure*)o)->proc );
  } else if ( o->class == &CList ) {
    printf("list:\n\t");
    print_obj( ((List*)o)->value );
    print_obj( (Object*)((List*)o)->next );
  } else {
    printf("ERROR: unknown type %p\n", o->class);
  }
}


Object* identity(Env* env)
{
  printf("identity called!\n");

  return env->o1;
}

int main()
{
  Object* o1 = newint(33);
  Object* o2 = newstring("wurstbrot");
  Object* o3 = newproc(&identity);
  Object* o4 = newlist(o1, NULL);
  Object* o5 = newlist(o2, (List*)o4);
  Object* o6 = newint(44);
  Object* o7 = add_obj(o1, o6);
  Object* o8 = invoke_obj(o3, 1, o2);
  Object* o9 = newenv( newint(66), newint(77), NULL );
  Object* o10 = newclosure( (Env*)o9, (Proc*)o3 );
  Object* o11 = invoke_obj( o10, 1, o6 );

  print_obj(o1);
  print_obj(o2);
  print_obj(o3);
  //  print_obj(o4);
  print_obj(o5);
  print_obj(o7);
  print_obj(o8);
  print_obj(o9);
  print_obj(o10);
  print_obj(o11);

  return 0;
}

/*
Object* PROC1(unsigned long numargs, ...) {
  va_list args;

  //  Object* VAR1 = car_obj( cdr_obj( args ) );
  va_start(args, numargs);
  va_arg(args, Object*);
  Object* VAR1 = va_arg(args, Object*);
  va_end(args);

  return VAR1;
}

int main() {
  Object* VAR2 = newproc( &PROC1 );
  Object* VAR3 = newint( 11 );
  Object* VAR4 = newint( 22 );
  //  Object* l = newlist( VAR3, (List*)newlist( VAR4, NULL ) );
  Object* VAR5 = invoke_obj( VAR2, 2, VAR3, VAR4 );

  print_obj( VAR5 );

  return 0;
}
*/
