#include <stdio.h>
#include <stdlib.h>


typedef struct _Class Class;
typedef struct _Object Object;
typedef struct _Integer Integer;
typedef struct _String String;
typedef struct _Proc Proc;
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

typedef Object* (*proc_p)(Object*);
struct _Proc {
  Object object;
  proc_p value;
};

struct _List {
  Object object;
  Object* value;
  List* next;
};


/* class instances: */
/*
Class object = { NULL };
Class integer = { &object };
Class string = { &object };
*/
Class object = {};
Class integer = {};
Class string = {};
Class proc = {};
Class list = {};


Object* newint(int value)
{
  Integer* o = malloc( sizeof(Integer) );
  o->object.class = &integer;
  o->value = value;

  return (Object*)o;
}

Object* newstring(char* value)
{
  String* o = malloc( sizeof(String) );
  o->object.class = &string;
  o->value = value;

  return (Object*)o;
}

Object* newproc(proc_p value)
{
  Proc* o = malloc( sizeof(Proc) );
  o->object.class = &proc;
  o->value = value;

  return (Object*)o;
}

Object* newlist(Object* value, List* next)
{
  List* o = malloc( sizeof(List) );
  o->object.class = &list;
  o->value = value;
  o->next = next;

  return (Object*)o;
}


void print_obj(Object* o)
{
  if ( NULL == o ) {
    printf("NULL\n");
  } else if ( o->class == &integer ) {
    printf("integer: %d\n", ((Integer*)o)->value);
  } else if ( o->class == &string ) {
    printf("string: %s\n", ((String*)o)->value);
  } else if ( o->class == &proc) {
    printf("proc: %p\n", ((Proc*)o)->value);
  } else if ( o->class == &list ) {
    printf("list:\n\t");
    print_obj( ((List*)o)->value );
    print_obj( (Object*)((List*)o)->next );
  } else {
    printf("ERROR: unknown type %p\n", o->class);
  }
}


Object* identity(Object* o)
{
  printf("identity called!");

  return o;
}

int main()
{
  Object* o1 = newint(33);
  Object* o2 = newstring("wurstbrot");
  Object* o3 = newproc(&identity);
  Object* o4 = newlist(o1, NULL);
  Object* o5 = newlist(o2, (List*)o4);

  print_obj(o1);
  print_obj(o2);
  print_obj(o3);
  //  print_obj(o4);
  print_obj(o5);

  return 0;
}
