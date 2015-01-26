%{
#include <stdlib.h>
#include <stdio.h>

int yylex();
int yyerror();

%}

%union {
    int integer;
    char* string;
}

%token			SPACE
%token	<integer>	INT
%token	<string>	SYMBOL
%token	<string>	OPENPAR
			CLOSEPAR

%%

expr:
	| 	atom SPACE expr
		;

atom:		INT { printf("found INT: %d\n", yylval.integer); }
	|	SYMBOL { printf("found SYMBOL: %s\n", yylval.string); }
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
