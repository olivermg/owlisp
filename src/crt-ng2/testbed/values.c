#include <stdio.h>
#include <stdarg.h>


#define box_value(t,v) \
  { t, { .t = v } }

#define unbox_value(vx) \
  &vx.v


typedef enum _type_t {
  INT = 1,
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


value_t add(int numargs, ...)
{
  value_t sum = box_value(INT, 0);

  va_list values;

  va_start(values, numargs);
  for (int i = 0; i < numargs; i++) {
    value_t value = va_arg(values, value_t);
    sum.v.INT += value.v.INT;
  }
  va_end(values);

  return sum;
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
