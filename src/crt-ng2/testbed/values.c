#include <stdio.h>

#define ARGLISTSIZE	8


typedef union _val {
	int i;
	union _val (*p)(union _val*);
} val;

typedef val val_list[ARGLISTSIZE];


val proc1(val_list vs)
{
	printf("called proc1 with %d and %d\n", vs[0].i, vs[1].i);

	val vr = { .i = vs[0].i + vs[1].i };

	return vr;	// FIXME: unsafe!
}

int main()
{
	val v1 = { .i = 22 };
	val v2 = { .i = 33 };
	val_list vs = { v1, v2 };

	val vp = { .p = &proc1 };

	val v3 = vp.p(vs);

	return v3.i;
}

