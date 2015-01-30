%{
#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include "parser.h"
#include "y.tab.h"


//#define YYSTYPE obj_t*

int yylex();
int yyerror();

%}

/*
%union {
    obj_t* obj;
    char* string;
}
*/

%token                  CAR
%token                  CDR
%token                  CONS
%token                  IF
%token                  LAMBDA
%token                  QUOTE
%token                  FUNCALL
%token			INT
%token			OPENPAR
			CLOSEPAR

%%

exprs:
	|	exprs expr { $$ = $2; }
		;

expr:		atom
	|	cons
		;

atom:		INT
	;

cons:		OPENPAR primopexpr CLOSEPAR { $$ = $2; }
	;

primopexpr: 	carexpr
	|	cdrexpr
	|	consexpr
	|	ifexpr
	|	lambdaexpr
	|	quoteexpr
	|	funcallexpr
	;

carexpr:	CAR expr { $$ = car($2); }
	;

cdrexpr: 	CDR expr { $$ = cdr($2); }
	;

consexpr:	CONS expr expr { $$ = cons($2, $3); }
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

void print_obj(obj_t* obj)
{
  switch (obj->type) {
  case TSYM:
    printf("SYM(%s)", symname(obj));
    break;
  case TINT:
    printf("INT(%d)", intvalue(obj));
    break;
  case TCONS:
    printf("CONS(");
    print_obj(car(obj));
    printf(", ");
    print_obj(cdr(obj));
    printf(")");
    break;
  case TPROC:
    printf("PROC(");
    print_obj(procargs(obj));
    printf(", ");
    print_obj(proccode(obj));
    printf(", ");
    print_obj(procenv(obj));
    printf(")");
    break;
  default:
    printf("unknown object of type %d!\n", obj->type);
    break;
  }
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
