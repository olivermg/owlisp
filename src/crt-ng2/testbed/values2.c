#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>


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

Object* newenv(Object* o1, Object* o2, Env* parent)
{
  Env* o = malloc( sizeof(Env) );
  o->object.class = &CEnv;
  o->o1 = o1;
  o->o2 = o2;
  o->parent = parent;

  return (Object*)o;
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

Object* newlist(Object* value, List* next)
{
  List* o = malloc( sizeof(List) );
  o->object.class = &CList;
  o->value = value;
  o->next = next;

  return (Object*)o;
}


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

void set(Env* env, Envaddress* envaddress, Object* value)
{
  if ( envaddress->varindex == 0 ) {
    gotoframe( env, envaddress )->o1 = value;
  } else {
    gotoframe( env, envaddress )->o2 = value;
  }
}


/*
 * runtime functionality:
 */

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


/*
 * test application (probably compiler-generated) code:
 */

Object* identity(Env* env)
{
  printf("identity called!\n");

  Envaddress* ea = (Envaddress*)newenvaddress( 0, 1 );

  return lookup( env, ea );
}

Object* inner(Env* env)
{
  Envaddress* eai0 = (Envaddress*)newenvaddress( 0, 0 );
  Object* innerint0 = lookup(env, eai0);

  Envaddress* eao0 = (Envaddress*)newenvaddress( 1, 0 );
  Object* outerint0 = lookup(env, eao0);

  Envaddress* eao1 = (Envaddress*)newenvaddress( 1, 1 );
  Object* outerint1 = lookup(env, eao1);

  printf("\n\tinnerint0 from inner: ");
  print_obj(innerint0);
  printf("\touterint0 from inner: ");
  print_obj(outerint0);
  printf("\touterint1 from inner: ");
  print_obj(outerint1);
  printf("\n");

  return outerint1;
}

Object* outer(Env* env)
{
  Object* outerint = newint( 315 );

  Envaddress* ea = (Envaddress*)newenvaddress( 0, 1 );
  set( env, ea, outerint );

  // TODO: simplify handling of creating proc/closure and calling it:
  Proc* iproc = (Proc*)newproc(&inner);
  Object* iclosure = newclosure(env, iproc);
  Object* arg1 = newint( 1337 );
  return invoke_obj(iclosure, 1, arg1 );
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
  Object* o8 = invoke_obj(o3, 2, o2, o1);
  Object* o9 = newenv( newint(66), newint(77), NULL );
  Object* o10 = newclosure( (Env*)o9, (Proc*)o3 );
  Object* o11 = invoke_obj( o10, 2, o6, o7 );
  Object* o12 = newproc(&outer);
  Object* o13 = invoke_obj( o12, 1, o6 );

  printf("\no1  (int)           : "); print_obj(o1);
  printf("\no2  (string)        : "); print_obj(o2);
  printf("\no3  (proc)          : "); print_obj(o3);
  printf("\no5  (list)          : "); print_obj(o5);
  printf("\no7  (add res)       : "); print_obj(o7);
  printf("\no8  (call proc res) : "); print_obj(o8);
  printf("\no9  (env)           : "); print_obj(o9);
  printf("\no10 (closure)       : "); print_obj(o10);
  printf("\no11 (call clos res) : "); print_obj(o11);
  printf("\no12 (closure real)  : "); print_obj(o12);
  printf("\no13 (call clos res) : "); print_obj(o13);

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
