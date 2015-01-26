%{
#include <stdlib.h>
#include <stdio.h>
%}

%token NUM
%token OPENPAR CLOSEPAR

%%

expr:
      | NUM { printf("found NUM: %d\n", yylval); } expr
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
