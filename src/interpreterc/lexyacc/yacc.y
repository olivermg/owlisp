%{
#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>


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


int yylex();
int yyerror();

obj_t* new_obj(type_t type, unsigned long numargs, ...);
//obj_t* intern(char* name);
//obj_t* eval(obj_t* expr, obj_t* env);
//obj_t* readobj(FILE* stream);

%}

%union {
    char* string;
}

%token                  CAR
%token                  CDR
%token                  CONS
%token                  IF
%token                  LAMBDA
%token                  QUOTE
%token                  FUNCALL
%token	<string>	ATOM
%token			OPENPAR
			CLOSEPAR

%%

exprs:
	|	expr exprs
		;

expr:		ATOM { printf("found ATOM: %s\n", yylval.string); }
	|	cons
		;

cons:		OPENPAR primopexpr CLOSEPAR
	;

primopexpr: 	carexpr
	|	cdrexpr
	|	consexpr
	|	ifexpr
	|	lambdaexpr
	|	quoteexpr
	|	funcallexpr
	;

carexpr:	CAR expr { printf("CAR!\n"); }
	;

cdrexpr: 	CDR expr
	;

consexpr:	CONS expr expr
	;

ifexpr:		IF expr expr expr
	;

lambdaexpr:	LAMBDA cons exprs
	;

quoteexpr:	QUOTE expr
	;

funcallexpr:	FUNCALL expr exprs
	;

%%

obj_t* new_obj(type_t type, unsigned long numargs, ...)
{
    va_list va;

    obj_t* newobj = malloc(sizeof(obj_t) + (numargs - 1)*sizeof(obj_t*));
    newobj->type = type;
    va_start(va, numargs);
    for (unsigned long i = 0; i < numargs; i++) {
	newobj->objs[i] = va_arg(va, obj_t*);
    }
    va_end(va);

    return newobj;
}

void init()
{
  nil = new_obj(TSYM, 1, 0);
  setcar(nil, nil);

  interned_syms = cons(nil, nil);
  global_env = cons(cons(nil, nil), nil);
}


int yyerror(char* msg)
{
    fprintf(stderr, "ERROR: %s\n", msg);
    exit(1);
}

int yywrap()
{
    return 1;
}

int main()
{
    init();

    yyparse();
    return 0;
}
