#include <stdio.h>

#define box_value(t,v) \
  { t, { .t = v } }

#define unbox_value(t,vx) \
  vx.v.t


typedef enum _type_t {
  UNDEF = 0,
  INT,
  PROC,
} type_t;

struct _value_t;

typedef union _val {
  int INT;
  struct _value_t (*PROC)(struct _value_t*);
} val;

typedef struct _value_t {
  type_t t;
  val v;
} value_t;

typedef value_t value_t_list[8];


value_t proc1(value_t_list vs)
{
  int int1 = unbox_value(INT, vs[0]);
  int int2 = unbox_value(INT, vs[1]);

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

  value_t vr = unbox_value(PROC, vp)(vs);

  printf("result: %d\n", unbox_value(INT, vr));

  return unbox_value(INT, vr);
}
