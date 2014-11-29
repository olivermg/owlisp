#include <stdio.h>
#include "print.h"

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
  } else if ( o->class == &CEnvaddress ) {
    printf("envaddress: %d, %d\n", ((Envaddress*)o)->frameindex, ((Envaddress*)o)->varindex);
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
