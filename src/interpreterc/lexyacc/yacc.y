%{
#include <stdlib.h>
#include <stdio.h>

int yylex();
int yyerror();

%}

%union {
    char* string;
}

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

cons:		OPENPAR { printf("starting CONS..."); } exprs CLOSEPAR { printf("...ending CONS\n"); }
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
