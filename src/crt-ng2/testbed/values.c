#include <stdio.h>
#include <stdlib.h>
//#include <stdarg.h>
#include <assert.h>


#define box_value(t,v) \
  { t, { .t = v } }

#define box_undef() \
  { UNDEF, { .INT = 0 } }

#define unbox_value(vx) \
  &vx.v

/*
#define types_same(t1,t2,t) \
  (t1 == t && t2 == t)

#define types_any(t1,t2,tda,tdb) \
  ((t1 == tda && (t2 == tda || t2 == tdb)) || (t1 == tdb && (t2 == tda || t2 == tdb)))

#define operate_args(last,op,dest) \
  value_t dest = box_undef();			\
  va_list args; va_start(args, last);		\
  for(long i = 0; i < last; i++) {		\
    value_t value = va_arg(args, value_t);	\
    switch (value.t) {				\
    case INT:					\
      dest.v.INT = dest.v.INT + value.v.INT;	\
      break;					\
    default:								\
      printf("don't know how to apply operator %s to type %d\n", "op", value.t); \
      break;								\
    }									\
  }									\
  va_end(args);
*/

/* predeclarations: */
typedef struct _value_t value_t;
typedef struct _list_t list_t;
typedef enum _type_t type_t;


typedef value_t (*function_t)(value_t);

struct _list_t {
  long size;
  value_t* vs;
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
    function_t PROC;
    list_t LIST;
  } v;
};


typedef value_t (*map_primitive_t)(value_t);

value_t map_primitive(map_primitive_t fn, value_t args)
{
  assert(args.t == LIST);

  long listlen = args.v.LIST.size;
  value_t* mappedvs = malloc(sizeof(value_t) * listlen);
  list_t ml = { listlen, mappedvs };
  value_t mappedlist = box_value(LIST, ml);

  for (long i = 0; i < args.v.LIST.size; i++) {
    mappedvs[i] = fn(args.v.LIST.vs[i]);
  }

  return mappedlist;
}


value_t op_inc(value_t v)
{
  value_t result = box_value( INT, v.v.INT + 1 ); // FIXME: unsafe!

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
  int int1 = *(int*)unbox_value(vs.v.LIST.vs[0]);
  int int2 = *(int*)unbox_value(vs.v.LIST.vs[1]);

  printf("called proc1 with %d and %d\n", int1, int2);

  //  value_t vr = add(2, vs[0], vs[1]);
  //  value_t vr = box_value(INT, int1 + int2);
  value_t vr = inc(vs);

  return vr.v.LIST.vs[1];	// FIXME: unsafe!
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
