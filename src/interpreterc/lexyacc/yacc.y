%{
#include <stdlib.h>
#include <stdio.h>

int yylex();
int yyerror();

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
