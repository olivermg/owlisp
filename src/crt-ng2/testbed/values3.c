#include <stdio.h>

#include <owlisp/owlisprt.h>

Object* PROC1(Env* env)
{
	printf("PROC1 start\n");
	print_obj((Object*)env);
	printf("PROC1 end\n");

	return NULL;
}

Object* _U_MAIN(Env* env)
{
	Object* o1 = newint(11);
	Object* o2 = newint(22);
	Object* o3 = newint(33);

	Object* proc1 = newproc(&PROC1);
	invoke_obj(proc1, 3, o1, o2, o3);

	//Object* env1 = newenv(NULL, 3, o1, o2, o3);
	//print_obj((Object*)env1);

	return 0;
}

