#ifndef __PARSER_H
#define __PARSER_H 1


#define mksym(x) new_obj(TSYM, 1, (x))
#define symname(x) (null(x) ? "nil" : (char*)((x)->objs[0]))

#define mkint(x) new_obj(TINT, 1, (x)) // TODO: don't put value in ptr memory location
#define intvalue(x) (int)((x)->objs[0])

#define mkproc(args,code,env) new_obj(TPROC, 3, (args), (code), (env))
#define procargs(p) ((p)->objs[0])
#define proccode(p) ((p)->objs[1])
#define procenv(p) ((p)->objs[2])

#define eq(x,y) ((x) == (y))
#define null(x) eq(x, nil)

#define cons(x,y) new_obj(TCONS, 2, (x), (y))
#define car(x) ((x)->objs[0])
#define cdr(x) ((x)->objs[1])
#define setcar(x,v) ((x)->objs[0] = (v))
#define setcdr(x,v) ((x)->objs[1] = (v))

#define extend(env,sym,val) cons(cons((sym), (val)), (env))

#define error(x) do { fprintf(stderr, "ERROR: %s\n", x); exit(1); } while (0)


typedef enum _type_t {
    TSYM,
    TINT,
    TCONS,
    TPROC
} type_t;

typedef struct _obj_t {
    type_t type;
    struct _obj_t* objs[1];
} obj_t;


obj_t* nil;
obj_t* interned_syms;
obj_t* global_env;

obj_t* new_obj(type_t type, unsigned long numargs, ...);
//obj_t* intern(char* name);
//obj_t* eval(obj_t* expr, obj_t* env);
//obj_t* readobj(FILE* stream);

#endif
