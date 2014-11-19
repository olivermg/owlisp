#include <stdio.h>
#include <stdarg.h>


#define box_value(t,v) \
  { t, { .t = v } }

#define box_undef() \
  { UNDEF, { .INT = 0 } }

#define unbox_value(vx) \
  &vx.v

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


typedef enum _type_t {
  UNDEF = 0,
  INT,
  PROC,
} type_t;

struct _value_t;

typedef struct _value_t (*function_t)(struct _value_t*);

typedef union _val {
  int INT;
  function_t PROC;
} val;

typedef struct _value_t {
  type_t t;
  val v;
} value_t;

typedef value_t value_t_list[8]; // TODO: why don't we want to use va_arg?


value_t add(long numargs, ...)
{
  operate_args(numargs, +, sum);

  return sum;
}

value_t sub(long numargs, ...)
{
  operate_args(numargs, -, diff);

  return diff;
}


value_t proc1(value_t_list vs)
{
  /* TODO:
   * actually, we don't want to use any primitive types in generated code, i.e. we don't compile code
   * making use of "unbox_value".
   * that results in the need to have primitive functions - like "add", for we can't use the C native
   * operators on value_t
   */
  int int1 = *(int*)unbox_value(vs[0]);
  int int2 = *(int*)unbox_value(vs[1]);

  printf("called proc1 with %d and %d\n", int1, int2);

  value_t vr = add(2, vs[0], vs[1]);

  return vr;	// FIXME: unsafe!
}

int main()
{
  value_t v1 = box_value( INT, 22 );
  value_t v2 = box_value( INT, 33 );
  value_t_list vs = { v1, v2 };

  value_t vp = box_value( PROC, &proc1 );

  value_t vr = (*(function_t*)unbox_value(vp))(vs);

  printf("result: %d\n", *(int*)unbox_value(vr));

  return *(int*)unbox_value(vr);
}
