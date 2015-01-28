%{
#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>

int yylex();
int yyerror();

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

obj_t* new_obj(type_t type, unsigned long numargs, ...);

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
    yyparse();
    return 0;
}
