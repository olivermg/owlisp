#include <stdio.h>

#define value(t,v) \
	{ t, { .F_##t = v } }


#define F_INT i
#define F_PROC p
typedef enum _type_t {
	UNDEF = 0,
	INT,
	PROC,
} type_t;

struct _value_t;

typedef union _val {
	int i;
	struct _value_t (*p)(struct _value_t*);
} val;

typedef struct _value_t {
	type_t t;
	val v;
} value_t;

typedef value_t value_t_list[8];


value_t proc1(value_t_list vs)
{
	printf("called proc1 with %d and %d\n", vs[0].v.i, vs[1].v.i);

	value_t vr = value( INT, vs[0].v.i + vs[1].v.i );

	return vr;	// FIXME: unsafe!
}

int main()
{
	value_t v1 = value( INT, 22 );
	value_t v2 = value( INT, 33 );
	value_t_list vs = { v1, v2 };

	value_t vp = value( PROC, &proc1 );

	value_t vr = vp.v.p(vs);

	printf("result: %d\n", vr.v.i);

	return vr.v.i;
}

