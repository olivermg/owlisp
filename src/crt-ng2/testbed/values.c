#include <stdio.h>

#define box_value(t,v) \
  { t, { .t = v } }

//#define unbox_value(vx)			\
//  vx.v.t


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

typedef value_t value_t_list[8];


void* unbox_value(value_t v)
{
  void* unboxed = NULL;

  switch (v.t) {
  case INT:
    unboxed = &v.v.INT; // TODO: ain't this always the same address in v.v (=0)? so we should not even have to do the switch/case
    break;
  case PROC:
    unboxed = &v.v.PROC;
    break;
  default:
    printf("unknown type %d!\n", v.t);
    break;
  }

  return unboxed;
}


value_t proc1(value_t_list vs)
{
  int int1 = *(int*)unbox_value(vs[0]); // TODO: actually, we don't want to use any primitive types in "application level" code
  int int2 = *(int*)unbox_value(vs[1]);

  printf("called proc1 with %d and %d\n", int1, int2);

  value_t vr = box_value( INT, int1 + int2 );

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
