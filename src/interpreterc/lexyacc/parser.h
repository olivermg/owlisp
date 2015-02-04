#ifndef __PARSER_H
#define __PARSER_H 1


#define YYSTYPE obj_t*

#define mksym(x)                     new_obj(TSYM, 1, strdup(x))
#define symname(x)                   (null(x) ? "nil" : (char*)((x)->objs[0]))

#define mkref(f,v)                   new_obj(TREF, 2, (f), (v))
#define refframe(r)                  (int)((r)->objs[0])
#define refvar(r)                    (int)((r)->objs[1])

#define mkint(x)                     new_obj(TINT, 1, (x)) // TODO: don't put value in ptr memory location
#define intvalue(x)                  (int)((x)->objs[0])

#define mklambda(code)               new_obj(TLAMBDA, 1, (code))
#define lambdacode(l)                ((l)->objs[0])

#define mkproc(code,lenv)            new_obj(TPROC, 2, (code), (lenv))
//#define procparams(p)                ((p)->objs[0])
#define proccode(p)                  ((p)->objs[0])
#define proclenv(p)                  ((p)->objs[1])

#define mkapply(proc,args)           new_obj(TAPPLY, 2, (proc), (args))
#define applyproc(a)                 ((a)->objs[0])
#define applyargs(a)                 ((a)->objs[1])

#define mkif(pred,then,else)         new_obj(TIF, 3, (pred), (then), (else))
#define ifpred(i)                    ((i)->objs[0])
#define ifthen(i)                    ((i)->objs[1])
#define ifelse(i)                    ((i)->objs[2])

#define eq(x,y)                      ((x) == (y))
#define null(x)                      eq(x, nil)

#define cons(x,y)                    new_obj(TCONS, 2, (x), (y))
#define car(x)                       ((x)->objs[0])
#define cdr(x)                       ((x)->objs[1])
#define setcar(x,v)                  ((x)->objs[0] = (v))
#define setcdr(x,v)                  ((x)->objs[1] = (v))

#define extend(env,val)              cons(cons((val), car(env)), cdr(env))

#define error(x)                     do { fprintf(stderr, "ERROR: %s\n", x); exit(1); } while (0)


typedef enum _type_t {
    TSYM,
    TREF,
    TINT,
    TCONS,
    TLAMBDA,
    TPROC,
    TAPPLY,
    TIF
} type_t;

typedef struct _obj_t {
    type_t type;
    struct _obj_t* objs[1];
} obj_t;


obj_t* nil;
obj_t* quote;

obj_t* program;

obj_t* interned_syms;
obj_t* global_env;

obj_t* new_obj(type_t type, unsigned long numargs, ...);
void indent(int indentation);
void print_cons(obj_t* obj, int indentation);
void print_obj(obj_t* obj, int indentation);
obj_t* multiple_extend(obj_t* env, obj_t* vals);
obj_t* register_frame(obj_t* frame, obj_t* syms);
obj_t* drop_frame(obj_t* frame);
obj_t* find_symbol_address(obj_t* frame, obj_t* sym, int frameidx);
obj_t* find_symbol(char* name);
obj_t* intern(char* name);
obj_t* assoc(obj_t* key, obj_t* alist);
obj_t* progn(obj_t* exprs, obj_t* env);
obj_t* apply(obj_t* proc, obj_t* vals, obj_t* env);
obj_t* evlis(obj_t* exprs, obj_t* env);
obj_t* eval(obj_t* expr, obj_t* env);

#endif
