#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <assert.h>


/* predeclarations: */
typedef struct _value_t* value_p;
typedef struct _list_t* list_p;
typedef enum _type_t type_t;


typedef value_p (*function_p)(value_p);

struct _list_t {
  long size;
  value_p* vs;
};

enum _type_t {
  UNDEF = 0,
  INT,
  PROC,
  LIST
};

struct _value_t {
  type_t t;
  union {
    int INT;
    function_p PROC;
    list_p LIST;
  };
};


#define box_value(DEST,T,...)				\
  value_p DEST;						\
  {							\
    struct _value_t V = { T, { .T = __VA_ARGS__ } };	\
    DEST = malloc( sizeof(struct _value_t) );		\
    *DEST = V;						\
  }

#define box_undef(DEST)				\
  value_p DEST;					\
  {						\
    DEST = malloc( sizeof(struct _value_t) );	\
    *DEST = { UNDEF, { .INT = 0 } }		\
  }

#define box_empty_list(DEST)			\
  value_p DEST;					\
  {						\
    struct _list_t L = { 0, NULL };		\
    struct _value_t V = { LIST, L };		\
  }

#define unbox_value(vx) \
  &vx.INT


inline value_p box_list(long numargs, ...) // TODO: not beautiful to have special handler for lists
{
  list_p l = malloc( sizeof(struct _list_t) );
  l->size = numargs;
  l->vs = malloc( sizeof(value_p) * numargs );
  box_value(list, LIST, l);

  va_list args;
  va_start(args, numargs);
  for (long i = 0; i < numargs; i++) {
    list->LIST->vs[i] = va_arg(args, value_p);
  }
  va_end(args);

  return list;
}


typedef value_p (*map_primitive_p)(value_p);

value_p map_primitive(map_primitive_p fn, value_p args)
{
  assert(args->t == LIST);

  long listlen = args->LIST->size;
  value_p mappedlist = box_list(listlen, args->);

  for (long i = 0; i < args.LIST.size; i++) {
    mappedlist.LIST.vs[i] = fn(args.LIST.vs[i]);
  }

  return mappedlist;
}


value_t op_inc(value_t v)
{
  value_t result = box_value( INT, v.INT + 1 ); // FIXME: unsafe!

  return result;
}

value_t inc(value_t args)
{
  return map_primitive(op_inc, args);
}

/*
value_t sub(long numargs, ...)
{
  operate_args(numargs, -, diff);

  return diff;
}

void print()
{
}
*/


value_t proc1(value_t vs)
{
  /* TODO:
   * actually, we don't want to use any primitive types in generated code, i.e. we don't compile code
   * making use of "unbox_value".
   * that results in the need to have primitive functions - like "add", for we can't use the C native
   * operators on value_t
   */
  int int1 = *(int*)unbox_value(vs.LIST.vs[0]);
  int int2 = *(int*)unbox_value(vs.LIST.vs[1]);

  printf("called proc1 with %d and %d\n", int1, int2);

  //  value_t vr = add(2, vs[0], vs[1]);
  //  value_t vr = box_value(INT, int1 + int2);
  value_t vr = inc(vs);

  return vr.LIST.vs[1];	// FIXME: unsafe!
}

int main()
{
  value_t v1 = box_value( INT, 22 );
  value_t v2 = box_value( INT, 33 );

  value_t vls[] = { v1, v2 };
  list_t vl = { 2, vls };
  value_t vs = box_value( LIST, vl );

  value_t vp = box_value( PROC, &proc1 );

  value_t vr = (*(function_t*)unbox_value(vp))(vs);

  printf("result: %d\n", *(int*)unbox_value(vr));

  return *(int*)unbox_value(vr);
}
