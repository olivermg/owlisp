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

%define api.value.type {obj_t*}
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
	|	expr exprs
		;

expr:		atom { printf("found ATOM: (%d,%p)\n", yylval->type, yylval->objs); }
	|	cons
		;

atom:		INT
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

consexpr:	CONS expr { printf("cons_car:%p\n",yylval); } expr
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
